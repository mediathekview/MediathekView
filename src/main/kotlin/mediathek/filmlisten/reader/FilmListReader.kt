/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.filmlisten.reader

import kotlinx.coroutines.*
import mediathek.config.CommandLineOptions
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.daten.Country
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.filmlisten.FilmListMetadataStore
import mediathek.tool.ProgressMonitorInputStream
import mediathek.tool.TrailerTeaserChecker
import mediathek.tool.datum.DateUtil
import mediathek.tool.episodes.TitleParserManager
import mediathek.tool.http.MVHttpClient
import mediathek.tool.time.Stopwatch
import okhttp3.Request
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import org.tukaani.xz.XZInputStream
import tools.jackson.core.JsonParser
import tools.jackson.core.JsonToken
import tools.jackson.core.ObjectReadContext
import tools.jackson.core.json.JsonFactory
import java.io.BufferedInputStream
import java.io.FileNotFoundException
import java.io.InputStream
import java.net.URI
import java.net.URISyntaxException
import java.net.URL
import java.nio.file.Files
import java.nio.file.NoSuchFileException
import java.nio.file.Path
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import javax.swing.event.EventListenerList

open class FilmListReader : AutoCloseable {
    private val listeners = EventListenerList()
    private val progressEvent = ListenerFilmeLadenEvent("", "Download", 0, 0, false)
    private val ttc = TrailerTeaserChecker()
    private val manager = TitleParserManager()
    private var progress = 0
    private lateinit var filmSink: (DatenFilm) -> Unit
    private var sender = ""
    private var thema = ""

    fun addAdListener(listener: ListenerFilmeLaden) {
        listeners.add(ListenerFilmeLaden::class.java, listener)
    }

    /**
     * Remove all registered listeners when we do not need them anymore.
     */
    private fun removeRegisteredListeners() {
        for (listener in listeners.getListeners(ListenerFilmeLaden::class.java)) {
            listeners.remove(ListenerFilmeLaden::class.java, listener)
        }
    }

    private fun selectDecompressor(source: String, input: InputStream): InputStream =
        when (source.substring(source.lastIndexOf('.'))) {
            ".xz" -> XZInputStream(input, DECOMPRESSOR_MEMORY_LIMIT, false)
            ".json" -> input
            else -> throw UnsupportedOperationException("Unbekanntes Dateiformat entdeckt.")
        }

    private fun parseNeu(jp: JsonParser, datenFilm: DatenFilm) {
        val value = nextTextValue(jp)
        datenFilm.isNew = value.toBoolean()
    }

    protected open fun parseWebsiteLink(jp: JsonParser, datenFilm: DatenFilm) {
        val value = nextTextValue(jp)
        if (!value.isNullOrEmpty()) {
            datenFilm.websiteUrl = value
        }
    }

    private fun parseDescription(jp: JsonParser, datenFilm: DatenFilm) {
        val value = nextTextValue(jp)
        if (!value.isNullOrEmpty()) {
            datenFilm.description = value
        }
    }

    protected open fun parseGeo(jp: JsonParser, datenFilm: DatenFilm) {
        val geoStr = checkedString(jp)

        if (geoStr.isEmpty()) {
            datenFilm.clearCountries()
        } else {
            /*
            This code is more performant than String.split as we do not allocate arrays on every call.
             */
            var start = 0
            val length = geoStr.length
            for (i in 0..length) {
                if (i < length && geoStr[i] != '-') {
                    continue
                }
                val geoItem = geoStr.substring(start, i)
                start = i + 1
                if (geoItem.isEmpty()) {
                    continue
                }
                try {
                    datenFilm.addCountry(Country.valueOf(geoItem))
                } catch (_: IllegalArgumentException) {
                    logger.error("Unable to parse string {} to Country enum", geoItem)
                }
            }
        }
    }

    private fun parseSender(jp: JsonParser, datenFilm: DatenFilm) {
        val parsedSender = checkedString(jp)
        if (parsedSender.isEmpty()) {
            datenFilm.sender = sender
        } else {
            datenFilm.sender = parsedSender
            // store for future reads
            sender = parsedSender
        }

        if (datenFilm.sender.equals(SENDER_RBTV, ignoreCase = true)) {
            datenFilm.sender = SENDER_RADIO_BREMEN
        }
    }

    private fun parseThema(jp: JsonParser, datenFilm: DatenFilm) {
        val value = normalizeTypographicDoubleQuotes(checkedString(jp))
        if (value.isEmpty()) {
            datenFilm.thema = thema
        } else {
            datenFilm.thema = value
            thema = value
        }

        // we need to check thema as well as (currently) ARD also puts teaser only into thema...
        if (ttc.check(datenFilm.thema)) {
            datenFilm.isTrailerTeaser = true
        }
    }

    private fun resetCompressedFieldState() {
        sender = ""
        thema = ""
    }

    private fun checkedString(jp: JsonParser): String = nextTextValue(jp) ?: ""

    private fun normalizeTypographicDoubleQuotes(value: String): String {
        for (index in value.indices) {
            if (isTypographicDoubleQuote(value[index])) {
                return normalizeTypographicDoubleQuotes(value, index)
            }
        }
        return value
    }

    private fun normalizeTypographicDoubleQuotes(value: String, firstQuoteIndex: Int): String {
        val normalized = value.toCharArray()
        for (index in firstQuoteIndex until normalized.size) {
            if (isTypographicDoubleQuote(normalized[index])) {
                normalized[index] = '"'
            }
        }
        return String(normalized)
    }

    private fun isTypographicDoubleQuote(value: Char): Boolean =
        value == '„' || value == '“' || value == '”'

    private fun nextTextValue(jp: JsonParser): String? {
        val token = jp.nextToken()
        if (token == null || token == JsonToken.VALUE_NULL) {
            return null
        }
        return jp.valueAsString
    }

    private fun parseMetaData(jp: JsonParser, listeFilme: ListeFilme) {
        while (true) {
            val jsonToken = jp.nextToken() ?: return
            if (jsonToken == JsonToken.END_OBJECT) {
                break
            }
            if (jp.isExpectedStartArrayToken) {
                val meta = listeFilme.metaData
                nextTextValue(jp)
                meta.datum = nextTextValue(jp).orEmpty()
                nextTextValue(jp)
                nextTextValue(jp)
                meta.id = nextTextValue(jp).orEmpty()
                // update to fire pcs
                listeFilme.metaData = meta

                break
            }
        }
    }

    private fun skipFieldDescriptions(jp: JsonParser) {
        while (true) {
            val jsonToken = jp.nextToken() ?: return
            if (jsonToken == JsonToken.END_OBJECT) {
                break
            }
            if (jp.isExpectedStartArrayToken) {
                // sind nur die Feldbeschreibungen, brauch mer nicht
                jp.nextToken()
                break
            }
        }
    }

    private fun parseUrlSubtitle(jp: JsonParser, datenFilm: DatenFilm) {
        datenFilm.subtitleUrl = checkedString(jp)
    }

    private fun parseUrlKlein(jp: JsonParser, datenFilm: DatenFilm) {
        datenFilm.lowQualityUrl = checkedString(jp)
    }

    private fun parseUrlHd(jp: JsonParser, datenFilm: DatenFilm) {
        datenFilm.highQualityUrl = checkedString(jp)
    }

    private fun parseDatumLong(jp: JsonParser, datenFilm: DatenFilm) {
        val str = checkedString(jp)
        datenFilm.setDatumLongSeconds(parseSignedLong(str))
    }

    private fun parseSignedLong(value: String?): Long {
        if (value.isNullOrEmpty()) {
            return 0
        }

        var negative = false
        var start = 0
        if (value[0] == '-') {
            negative = true
            start = 1
        }

        if (start >= value.length) {
            logDatumLongParseError(value)
            return 0
        }

        var result = 0L
        for (i in start..<value.length) {
            val c = value[i]
            if (c !in '0'..'9') {
                logDatumLongParseError(value)
                return 0
            }
            result = result * 10 + (c - '0')
        }
        return if (negative) -result else result
    }

    private fun logDatumLongParseError(value: String) {
        if (CommandLineOptions.isDebugModeEnabled()) {
            logger.error("Failed to parse datum long string: {}", value)
        }
    }

    private fun parseSendedatum(jp: JsonParser, datenFilm: DatenFilm) {
        datenFilm.sendeDatum = checkedString(jp)
    }

    private fun parseFilmLength(jp: JsonParser, datenFilm: DatenFilm) {
        datenFilm.setFilmLengthSeconds(parseDurationSecondsOrZero(checkedString(jp)))
    }

    private fun parseDurationSecondsOrZero(value: String?): Int {
        if (value.isNullOrEmpty()) {
            return 0
        }

        val firstColon = value.indexOf(':')
        if (firstColon <= 0) {
            return 0
        }

        val secondColon = value.indexOf(':', firstColon + 1)
        if (secondColon <= firstColon + 1 || secondColon >= value.length - 1) {
            return 0
        }

        return try {
            val hours = parsePositiveInt(value, 0, firstColon)
            val minutes = parsePositiveInt(value, firstColon + 1, secondColon)
            val seconds = parsePositiveInt(value, secondColon + 1, value.length)
            hours * 3600 + minutes * 60 + seconds
        } catch (_: NumberFormatException) {
            0
        }
    }

    private fun parsePositiveInt(value: String, start: Int, end: Int): Int {
        if (start >= end) {
            throw NumberFormatException("Empty integer segment")
        }

        var result = 0
        for (i in start..<end) {
            val c = value[i]
            if (c !in '0'..'9') {
                throw NumberFormatException("Invalid integer segment")
            }
            result = result * 10 + (c - '0')
        }
        return result
    }

    private fun parseGroesse(jp: JsonParser, datenFilm: DatenFilm) {
        val value = checkedString(jp)
        datenFilm.setFileSize(value)
    }

    /**
     * Skip over file entry.
     * This is used when fields were deleted in DatenFilm but still exit in filmlist file.
     */
    private fun skipToken(jp: JsonParser) {
        jp.nextToken()
    }

    /**
     * Skip the remaining values of the current film row once we already know it will be rejected.
     */
    private fun skipRemainingArray(jp: JsonParser) {
        while (true) {
            val token = jp.nextToken() ?: return
            if (token == JsonToken.END_ARRAY) {
                return
            }
        }
    }

    private fun parseTime(jp: JsonParser, datenFilm: DatenFilm) {
        var zeit = checkedString(jp)
        if (zeit.isNotEmpty() && zeit.length < 8) {
            zeit += ":00" // add seconds
        }
        datenFilm.sendeZeit = zeit
    }

    /**
     * Check if the title contains keywords which specify an audio version
     */
    private fun parseAudioVersion(title: String, film: DatenFilm) {
        if (title.contains("Hörfassung") ||
            title.contains("Audiodeskription") ||
            title.contains("AD |") ||
            title.endsWith("(AD)") ||
            title.contains("Hörspiel") ||
            title.contains("Hörfilm") ||
            title.contains("mit gesprochenen Untertiteln")
        ) {
            film.isAudioVersion = true
        }
    }

    private fun parseSignLanguage(title: String, film: DatenFilm) {
        if (title.contains("Gebärden")) {
            film.isSignLanguage = true
        }
    }

    private fun parseTrailerTeaser(title: String, film: DatenFilm) {
        if (ttc.check(title)) {
            film.isTrailerTeaser = true
        }
    }

    private fun parseTitel(jp: JsonParser, datenFilm: DatenFilm) {
        val title = normalizeTypographicDoubleQuotes(checkedString(jp))
        datenFilm.title = title
        // check title if it is audio version
        parseAudioVersion(title, datenFilm)
        // check if it is in sign language
        parseSignLanguage(title, datenFilm)
        parseTrailerTeaser(title, datenFilm)
        // check for burned in subtitles
        if (title.contains("(mit Untertitel)")) {
            datenFilm.setBurnedInSubtitles(true)
        }
    }

    private fun parseUrl(jp: JsonParser, datenFilm: DatenFilm) {
        datenFilm.urlNormalQuality = checkedString(jp)
    }

    private fun parseLivestream(datenFilm: DatenFilm) {
        if (datenFilm.thema == THEMA_LIVE) {
            datenFilm.isLivestream = true
        }
    }

    private fun readData(jp: JsonParser, listeFilme: ListeFilme) {
        check(jp.nextToken() == JsonToken.START_OBJECT) { "Expected data to start with an Object" }

        parseMetaData(jp, listeFilme)

        skipFieldDescriptions(jp)

        val config = ApplicationConfiguration.getInstance()
        val loadTrailer = config.filmListLoadTrailer
        val loadAudiodescription = config.filmListLoadAudioDescription
        val loadSignLanguage = config.filmListLoadSignLanguage
        val loadLivestreams = config.filmListLoadLivestreams

        while (true) {
            val jsonToken = jp.nextToken() ?: return
            if (jsonToken == JsonToken.END_OBJECT) {
                break
            }
            if (jp.isExpectedStartArrayToken) {
                val datenFilm = DatenFilm()
                parseSender(jp, datenFilm)
                val senderApproved = SenderFilmlistLoadApprover.isApproved(datenFilm.sender)
                parseThema(jp, datenFilm)
                if (!senderApproved) {
                    if (jp.currentToken() != JsonToken.END_ARRAY) {
                        skipRemainingArray(jp)
                    }
                    continue
                }
                parseTitel(jp, datenFilm)
                if (!loadTrailer && datenFilm.isTrailerTeaser) {
                    skipRemainingArray(jp)
                    continue
                }
                if (!loadAudiodescription && datenFilm.isAudioVersion) {
                    skipRemainingArray(jp)
                    continue
                }
                if (!loadSignLanguage && datenFilm.isSignLanguage) {
                    skipRemainingArray(jp)
                    continue
                }
                if (!loadLivestreams && datenFilm.thema == THEMA_LIVE) {
                    skipRemainingArray(jp)
                    continue
                }
                parseSendedatum(jp, datenFilm)
                parseTime(jp, datenFilm)
                parseFilmLength(jp, datenFilm)
                parseGroesse(jp, datenFilm)
                parseDescription(jp, datenFilm)
                parseUrl(jp, datenFilm)
                parseWebsiteLink(jp, datenFilm)
                parseUrlSubtitle(jp, datenFilm)
                skipToken(jp)
                parseUrlKlein(jp, datenFilm)
                skipToken(jp)
                parseUrlHd(jp, datenFilm)
                skipToken(jp)
                parseDatumLong(jp, datenFilm)
                skipToken(jp) // HISTORY_URL
                parseGeo(jp, datenFilm)
                parseNeu(jp, datenFilm)

                // this will check after all data has been read
                parseLivestream(datenFilm)
                checkPlayList(datenFilm)

                // just initialize the film object, rest will be done in one of the filters
                datenFilm.init()

                // this will add the film to the filmlist if it passes...
                filmSink(datenFilm)
            }
        }
    }

    /**
     * Check if this film entry is a playlist entry, ends with .m3u8
     *
     * @param datenFilm the film to check.
     */
    private fun checkPlayList(datenFilm: DatenFilm) {
        if (datenFilm.urlNormalQuality.endsWith(PLAYLIST_SUFFIX)) {
            datenFilm.isPlayList = true
        }
    }

    fun readFilmListe(source: String, listeFilme: ListeFilme, days: Int) {
        try {
            logger.trace("Liste Filme lesen von: {}", source)
            resetCompressedFieldState()
            listeFilme.clear()

            if (days == 0) {
                filmSink = { film: DatenFilm ->
                    listeFilme.add(film)
                }
            } else {
                val cutoffDate = LocalDate.now().minusDays(days.toLong())
                filmSink = { film: DatenFilm ->
                    // do not filter livestreams
                    if (film.isLivestream) {
                        listeFilme.add(film)
                    } else {
                        val filmDate = DateUtil.convertToLocalDate(film.datumFilmTimeMillis)
                        if (!cutoffDate.isAfter(filmDate)) {
                            listeFilme.add(film)
                        }
                    }
                }
            }

            notifyStart(source) // für die Progressanzeige

            if (source.startsWith("http")) {
                val sourceUrl = URI(source)
                processFromWeb(sourceUrl.toURL(), listeFilme)
            } else {
                processFromFile(source, listeFilme)
            }

            parseSeasonAndEpisode(listeFilme)
        } catch (ex: URISyntaxException) {
            logger.warn(ex)
        } catch (ex: java.io.IOException) {
            logger.warn(ex)
        }

        notifyFertig(source, listeFilme)
    }

    private fun parseSeasonAndEpisode(listeFilme: ListeFilme) {
        val stopwatch = Stopwatch.createStarted()

        val detectedCount = runBlocking {
            withContext(Dispatchers.Default) {
                val films = listeFilme.snapshot()
                if (films.isEmpty()) {
                    0
                } else {
                    parseSeasonAndEpisode(films)
                }
            }
        }

        stopwatch.stop()
        logger.info("Season and episode detection took: {}", stopwatch)
        logger.info("Number of detected seasons and episodes: {}", detectedCount)
    }

    private suspend fun parseSeasonAndEpisode(films: List<DatenFilm>): Int = coroutineScope {
        val workerCount = Runtime.getRuntime().availableProcessors().coerceAtLeast(1)
        val chunkSize = ((films.size + workerCount - 1) / workerCount).coerceAtLeast(1)
        films.chunked(chunkSize)
            .map { chunk ->
                async {
                    var detectedCount = 0
                    for (film in chunk) {
                        val result = manager.parse(film.sender, film.title)
                        result.ifPresent { seasonEpisode ->
                            film.setSeasonEpisode(seasonEpisode)
                            detectedCount++
                        }
                    }
                    detectedCount
                }
            }
            .awaitAll()
            .sum()
    }

    /**
     * Read a locally available filmlist.
     *
     * @param source file path as string
     * @param listeFilme the list to read to
     */
    private fun processFromFile(source: String, listeFilme: ListeFilme) {
        try {
            val filePath = Path.of(source)
            val fileSize = Files.size(filePath)
            if (fileSize == 0L) {
                Files.deleteIfExists(filePath)
            }

            val monitor = progressMonitor(source)

            Files.newInputStream(filePath).use { sourceFile ->
                BufferedInputStream(sourceFile, BUFFER_SIZE).use { bufferedSource ->
                    ProgressMonitorInputStream(bufferedSource, fileSize, monitor).use { input ->
                        selectDecompressor(source, input).use { inputStream ->
                            JsonFactory().createParser(ObjectReadContext.empty(), inputStream).use { jp ->
                                readData(jp, listeFilme)
                            }
                        }
                    }
                }
            }
        } catch (_: FileNotFoundException) {
            logNonExistingFilmList(source)
            listeFilme.clear()
        } catch (_: NoSuchFileException) {
            logNonExistingFilmList(source)
            listeFilme.clear()
        } catch (ex: Exception) {
            logger.error("FilmListe: {}", source, ex)
            listeFilme.clear()
        }
    }

    private fun logNonExistingFilmList(source: String) {
        logger.debug("FilmListe existiert nicht: {}", source)
    }

    private fun buildClientInfo(): String =
        listOf(
            Konstanten.PROGRAMMNAME,
            Konstanten.MVVERSION,
            SystemUtils.OS_ARCH,
            SystemUtils.OS_NAME,
            SystemUtils.OS_VERSION,
        ).joinToString(",") { it.toString() }

    /**
     * Download and process a filmliste from the web.
     *
     * @param source source url as string
     * @param listeFilme the list to read to
     */
    private fun processFromWeb(source: URL, listeFilme: ListeFilme) {
        val request = Request.Builder()
            .url(source)
            .header("MV-Client", buildClientInfo())
            .get()
            .build()

        try {
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                response.body.use { body ->
                    if (response.isSuccessful) {
                        val endRequest = response.request
                        if (CommandLineOptions.isEnhancedLoggingEnabled()) {
                            logger.trace("Final Endpoint URL for filmlist: {}", endRequest.url.toString())
                        }
                        FilmListMetadataStore.writeEtag(source.toString(), response.header("ETag"))
                        val monitor = progressMonitor(source.toString())
                        ProgressMonitorInputStream(body.byteStream(), body.contentLength(), monitor).use { input ->
                            selectDecompressor(source.toString(), input).use { inputStream ->
                                JsonFactory().createParser(ObjectReadContext.empty(), inputStream).use { jp ->
                                    readData(jp, listeFilme)
                                }
                            }
                        }
                    } else {
                        logger.warn(
                            "processFromWeb HTTP Response Code: {} for {}",
                            response.code,
                            response.request.url.toUrl(),
                        )
                    }
                }
            }
        } catch (ex: Exception) {
            logger.error("FilmListe: {}", source, ex)
            listeFilme.clear()
        }
    }

    private fun notifyStart(url: String) {
        progress = 0
        for (listener in listeners.getListeners(ListenerFilmeLaden::class.java)) {
            listener.start(ListenerFilmeLadenEvent(url, "", PROGRESS_MAX, 0, false))
        }
    }

    private fun notifyProgress(url: String, iProgress: Int) {
        progress = iProgress
        if (progress > PROGRESS_MAX) {
            progress = PROGRESS_MAX
        }
        for (listener in listeners.getListeners(ListenerFilmeLaden::class.java)) {
            progressEvent.senderUrl = url
            progressEvent.progress = progress
            progressEvent.max = PROGRESS_MAX
            listener.progress(progressEvent)
        }
    }

    private fun notifyFertig(url: String, liste: ListeFilme) {
        logger.info(
            "Liste Filme gelesen am: {}",
            DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm")
                .format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault())),
        )
        logger.info("  erstellt am: {}", liste.metaData.generationDateTimeAsString)
        logger.info("  Anzahl Filme: {}", liste.size)
        for (listener in listeners.getListeners(ListenerFilmeLaden::class.java)) {
            progressEvent.senderUrl = url
            progressEvent.text = ""
            progressEvent.max = PROGRESS_MAX
            progressEvent.progress = progress
            listener.fertig(progressEvent)
        }
    }

    override fun close() {
        removeRegisteredListeners()
    }

    private fun progressMonitor(sourceString: String): (bytesRead: Long, size: Long) -> Unit {
        var oldProgress = 0
        var lastUpdate = 0L

        return { bytesRead, size ->
            if (size > 0) {
                val iProgress = (bytesRead * 100 / size).toInt()
                val now = System.currentTimeMillis()

                if (iProgress >= oldProgress + 1 || now - lastUpdate > MIN_TIME_BETWEEN_UPDATES_MS) {
                    oldProgress = iProgress
                    lastUpdate = now
                    notifyProgress(sourceString, iProgress)
                }
            }
        }
    }

    private companion object {
        private const val PROGRESS_MAX = 100
        private const val DECOMPRESSOR_MEMORY_LIMIT = -1
        private const val THEMA_LIVE = "Livestream"
        private const val PLAYLIST_SUFFIX = ".m3u8"
        private const val SENDER_RBTV = "rbtv"
        private const val SENDER_RADIO_BREMEN = "Radio Bremen TV"
        private const val BUFFER_SIZE = 64 * 1024
        private const val MIN_TIME_BETWEEN_UPDATES_MS = 500L
        private val logger = LogManager.getLogger()
    }
}
