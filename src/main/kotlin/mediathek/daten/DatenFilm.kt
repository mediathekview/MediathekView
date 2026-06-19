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

package mediathek.daten

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.abo.DatenAbo
import mediathek.gui.bookmark.BookmarkData
import mediathek.tool.FileSize
import mediathek.tool.GermanStringSorter
import mediathek.tool.RuntimeArchitecture
import mediathek.tool.datum.DatumFilm
import mediathek.tool.episodes.SeasonEpisode
import org.apache.commons.lang3.time.DurationFormatUtils
import org.apache.logging.log4j.LogManager
import java.net.HttpURLConnection
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.security.NoSuchAlgorithmException
import java.time.DateTimeException
import java.time.LocalDate
import java.util.*
import java.util.concurrent.atomic.AtomicInteger
import kotlin.time.Duration.Companion.seconds

class DatenFilm private constructor(
    val filmNr: Int,
) : Comparable<DatenFilm> {

    data class FilmIdentity(
        val sender: String,
        val thema: String,
        val normalQualityUrl: String,
        val websiteUrl: String,
    )

    private var countrySet: EnumSet<Country>? = null
    private var countriesAsStringCache: String? = null
    private var knownBlockedCountries: EnumSet<Country>? = null
    private var flags = 0
    var fileSizeInMegabytes: Int = 0
        private set
    var availableUntil: LocalDate? = null
    private var datumFilmTimeMillisStorage = UNDEFINED_DATUM_FILM_TIME_MILLIS
    private var datumFilmCache: DatumFilm? = null
    private var descriptionStorage: String? = null
    var sender: String = ""
        set(value) {
            field = value
            invalidateSha256()
        }
    var thema: String = ""
        set(value) {
            field = value
            invalidateSha256()
        }
    var title: String = ""
    private var sendeDateTimeStorage = packSendeDateTime(
        UNDEFINED_SENDE_DATUM_EPOCH_DAY,
        UNDEFINED_SENDE_ZEIT_SECOND_OF_DAY,
    )
    private var sendeDateTimeFallback: Array<String?>? = null
    var filmLength: Int = 0
        private set
    private var filmLengthAsStringCache = ""
    var season: Int = 0
        private set
    var episode: Int = 0
        private set
    private var sha256Cache: String? = null
    private var filmIdentityCache: FilmIdentity? = null
    private var subtitleUrlStorage: String? = null
    private var websiteUrlStorage: String? = null
    private var lowQualityUrlStorage: String? = null
    private var normalQualityUrlStorage = ""
    private var highQualityUrlStorage: String? = null
    var bookmark: BookmarkData? = null
    var abo: DatenAbo? = null
    private var datumLongSeconds = 0L
    private var cachedFileSizeLookups: MutableMap<FileSizeLookupKey, FileSize.LookupResult>? = null
    private var canBootstrapFileSizeFromNormalQualityUrl = true

    constructor() : this(FILMNR_GENERATOR.getAndIncrement())

    constructor(other: DatenFilm) : this(other.filmNr) {
        datumFilmTimeMillisStorage = other.datumFilmTimeMillisStorage
        fileSizeInMegabytes = other.fileSizeInMegabytes
        descriptionStorage = other.descriptionStorage
        sender = other.sender
        thema = other.thema
        title = other.title
        other.countrySet?.takeIf { it.isNotEmpty() }?.let { countrySet = EnumSet.copyOf(it) }
        countriesAsStringCache = other.countriesAsStringCache
        other.knownBlockedCountries?.takeIf { it.isNotEmpty() }?.let {
            knownBlockedCountries = EnumSet.copyOf(it)
        }
        flags = other.flags
        sendeDateTimeStorage = other.sendeDateTimeStorage
        sendeDateTimeFallback = other.sendeDateTimeFallback?.copyOf()
        filmLength = other.filmLength
        filmLengthAsStringCache = other.filmLengthAsStringCache
        season = other.season
        episode = other.episode
        availableUntil = other.availableUntil
        sha256Cache = other.sha256Cache
        filmIdentityCache = other.filmIdentityCache
        subtitleUrlStorage = other.subtitleUrlStorage
        websiteUrlStorage = other.websiteUrlStorage
        lowQualityUrlStorage = other.lowQualityUrlStorage
        normalQualityUrlStorage = UrlHostDictionary.compress(other.urlNormalQuality)
        highQualityUrlStorage = other.highQualityUrlStorage
        bookmark = other.bookmark
        abo = other.abo
        datumLongSeconds = other.datumLongSeconds
        other.cachedFileSizeLookups?.takeIf { it.isNotEmpty() }?.let {
            cachedFileSizeLookups = HashMap(it)
        }
        canBootstrapFileSizeFromNormalQualityUrl = other.canBootstrapFileSizeFromNormalQualityUrl
    }

    fun setFilmLengthSeconds(durationInSeconds: Int) {
        filmLength = durationInSeconds.coerceAtLeast(0)
        filmLengthAsStringCache = ""
    }

    val datumFilm: DatumFilm
        get() {
            if (isDatumFilmUndefined) {
                return DatumFilm.UNDEFINED_FILM_DATE
            }
            return datumFilmCache ?: DatumFilm(datumFilmTimeMillisStorage).also { datumFilmCache = it }
        }

    val isDatumFilmUndefined: Boolean
        get() = datumFilmTimeMillisStorage == UNDEFINED_DATUM_FILM_TIME_MILLIS

    val datumFilmTimeMillis: Long
        get() = datumFilmTimeMillisStorage

    var lowQualityUrl: String
        get() = lowQualityUrlStorage ?: ""
        set(value) {
            lowQualityUrlStorage = value.ifEmpty { null }
        }

    var highQualityUrl: String
        get() = highQualityUrlStorage?.let {
            if (isCompressedUrl(it)) decompressUrl(it) else it
        } ?: ""
        set(value) {
            highQualityUrlStorage = value.takeIf { it.isNotEmpty() }?.let(::compressUrlIfBeneficial)
        }

    internal val storedHighQualityUrl: String
        get() = highQualityUrlStorage ?: ""

    fun setDatumLongSeconds(datumLongSeconds: Long) {
        this.datumLongSeconds = datumLongSeconds
    }

    var sendeDatum: String
        get() {
            val fallback = sendeDateTimeFallback?.get(SENDE_DATUM_FALLBACK_INDEX)
            if (fallback != null) {
                return fallback
            }

            return if (sendeDatumEpochDay == UNDEFINED_SENDE_DATUM_EPOCH_DAY) {
                ""
            } else {
                formatSendeDatum(sendeDatumEpochDay)
            }
        }
        set(value) {
            val epochDay = parseSendeDatum(value)
            if (epochDay == null) {
                sendeDatumEpochDay = UNDEFINED_SENDE_DATUM_EPOCH_DAY
                setSendeDateTimeFallback(SENDE_DATUM_FALLBACK_INDEX, value.ifEmpty { null })
            } else {
                sendeDatumEpochDay = epochDay
                setSendeDateTimeFallback(SENDE_DATUM_FALLBACK_INDEX, null)
            }
        }

    var sendeZeit: String
        get() {
            val fallback = sendeDateTimeFallback?.get(SENDE_ZEIT_FALLBACK_INDEX)
            if (fallback != null) {
                return fallback
            }

            return if (sendeZeitSecondOfDay == UNDEFINED_SENDE_ZEIT_SECOND_OF_DAY) {
                ""
            } else {
                formatSendeZeit(sendeZeitSecondOfDay)
            }
        }
        set(value) {
            val secondOfDay = parseSendeZeit(value)
            if (secondOfDay == null) {
                sendeZeitSecondOfDay = UNDEFINED_SENDE_ZEIT_SECOND_OF_DAY
                setSendeDateTimeFallback(SENDE_ZEIT_FALLBACK_INDEX, value.ifEmpty { null })
            } else {
                sendeZeitSecondOfDay = secondOfDay
                setSendeDateTimeFallback(SENDE_ZEIT_FALLBACK_INDEX, null)
            }
        }

    private var sendeDatumEpochDay: Int
        get() = (sendeDateTimeStorage shr Int.SIZE_BITS).toInt()
        set(value) {
            sendeDateTimeStorage = packSendeDateTime(value, sendeZeitSecondOfDay)
        }

    private var sendeZeitSecondOfDay: Int
        get() = sendeDateTimeStorage.toInt()
        set(value) {
            sendeDateTimeStorage = packSendeDateTime(sendeDatumEpochDay, value)
        }

    private fun setSendeDateTimeFallback(index: Int, value: String?) {
        if (value == null && sendeDateTimeFallback == null) {
            return
        }

        val fallback = sendeDateTimeFallback ?: arrayOfNulls<String>(SENDE_DATE_TIME_FALLBACK_SIZE)
            .also { sendeDateTimeFallback = it }
        fallback[index] = value
        if (fallback.all { it == null }) {
            sendeDateTimeFallback = null
        }
    }

    private fun hasFlag(flag: Int): Boolean = flags and flag != 0

    private fun setFlag(flag: Int, enabled: Boolean) {
        flags = if (enabled) {
            flags or flag
        } else {
            flags and flag.inv()
        }
    }

    var isTrailerTeaser: Boolean
        get() = hasFlag(FLAG_TRAILER_TEASER)
        set(value) = setFlag(FLAG_TRAILER_TEASER, value)

    var isDuplicate: Boolean
        get() = hasFlag(FLAG_DUPLICATE)
        set(value) = setFlag(FLAG_DUPLICATE, value)

    var isAudioVersion: Boolean
        get() = hasFlag(FLAG_AUDIO_VERSION)
        set(value) = setFlag(FLAG_AUDIO_VERSION, value)

    var isPlayList: Boolean
        get() = hasFlag(FLAG_PLAYLIST)
        set(value) = setFlag(FLAG_PLAYLIST, value)

    var isSignLanguage: Boolean
        get() = hasFlag(FLAG_SIGN_LANGUAGE)
        set(value) = setFlag(FLAG_SIGN_LANGUAGE, value)

    val fileSizeAsString: String
        get() = if (fileSizeInMegabytes == 0) "" else fileSizeInMegabytes.toString()

    fun setFileSize(sizeText: String?) {
        try {
            fileSizeInMegabytes = FileSize.megabyteTextToInt(sizeText ?: "")
        } catch (ex: NumberFormatException) {
            logger.error("String: {}", sizeText, ex)
            fileSizeInMegabytes = 0
        }
    }

    var description: String
        get() = descriptionStorage ?: ""
        set(value) {
            if (value.isNotEmpty()) {
                descriptionStorage = value
            }
        }

    var websiteUrl: String
        get() = websiteUrlStorage?.let(UrlHostDictionary::expand) ?: ""
        set(value) {
            websiteUrlStorage = value.takeIf { it.isNotEmpty() }?.let(UrlHostDictionary::compress)
            invalidateSha256()
        }

    var isNew: Boolean
        get() = hasFlag(FLAG_NEW_ENTRY)
        set(value) = setFlag(FLAG_NEW_ENTRY, value)

    var isLivestream: Boolean
        get() = hasFlag(FLAG_LIVESTREAM)
        set(value) = setFlag(FLAG_LIVESTREAM, value)

    fun setBurnedInSubtitles(value: Boolean) {
        setFlag(FLAG_BURNED_IN_SUBTITLES, value)
    }

    fun hasBurnedInSubtitles(): Boolean = hasFlag(FLAG_BURNED_IN_SUBTITLES)

    fun hasSubtitle(): Boolean = subtitleUrlStorage != null

    fun hasAnySubtitles(): Boolean = hasSubtitle() || hasBurnedInSubtitles()

    fun clearCountries() {
        countrySet = null
        countriesAsStringCache = null
    }

    fun addCountry(country: Country) {
        val countries = countrySet ?: EnumSet.noneOf(Country::class.java).also { countrySet = it }
        if (countries.add(country)) {
            countriesAsStringCache = null
        }
    }

    fun hasCountries(): Boolean = countrySet?.isNotEmpty() == true

    fun hasCountry(country: Country): Boolean = countrySet?.contains(country) == true

    fun markGeoBlockedForLocation(location: Country) {
        val blockedCountries = knownBlockedCountries ?: EnumSet.noneOf(Country::class.java)
            .also { knownBlockedCountries = it }
        blockedCountries.add(location)
    }

    fun isGeoBlockedForLocation(location: Country): Boolean {
        if (knownBlockedCountries?.contains(location) == true) {
            return true
        }
        if (!hasCountries()) {
            return false
        }
        if (hasCountry(Country.EU)) {
            return !(hasCountry(location) || EU_COUNTRIES.contains(location))
        }
        return !hasCountry(location)
    }

    val countriesAsString: String
        get() {
            val countries = countrySet
            if (countries.isNullOrEmpty()) {
                return ""
            }
            countriesAsStringCache?.let { return it }

            val result = buildString {
                val iterator = countries.iterator()
                append(iterator.next())
                while (iterator.hasNext()) {
                    append('-').append(iterator.next())
                }
            }
            countriesAsStringCache = result
            return result
        }

    // TODO This function might not be necessary as getUrlNormalOrRequested does almost the same
    fun getUrlFuerAufloesung(resolution: FilmResolution.Enum?): String =
        when (resolution) {
            FilmResolution.Enum.LOW,
            FilmResolution.Enum.HIGH_QUALITY,
            -> getUrlNormalOrRequested(resolution)

            else -> urlNormalQuality
        }

    fun lookupFileSizeForUrl(url: String): FileSize.LookupResult = lookupFileSizeForUrl(url, false, null)

    fun lookupFileSizeForUrl(
        url: String,
        forceFetch: Boolean,
        resolution: String?,
    ): FileSize.LookupResult =
        lookupFileSizeForUrl(url, forceFetch, resolution, true)

    fun lookupFileSizeForUrl(
        url: String,
        forceFetch: Boolean,
        resolution: String?,
        probeHlsSegments: Boolean,
    ): FileSize.LookupResult {
        if (!forceFetch) {
            val cachedLookupResult = getCachedFileSizeLookup(url, resolution)
            if (cachedLookupResult != null) {
                applyFileSizeLookupResult(url, cachedLookupResult, resolution)
                return cachedLookupResult
            }
        }

        val lookupResult = FileSize.lookupFileSize(url, forceFetch, resolution, probeHlsSegments)
        applyFileSizeLookupResult(url, lookupResult, resolution)
        return lookupResult
    }

    internal fun applyFileSizeLookupResult(
        url: String,
        lookupResult: FileSize.LookupResult,
        resolution: String? = lookupResult.quality,
    ) {
        if (isForbiddenHlsLookup(url, lookupResult)) {
            markGeoBlockedForLocation(ApplicationConfiguration.getInstance().geographicLocation)
        }
        cacheFileSizeLookup(url, resolution, lookupResult)
    }

    private fun isForbiddenHlsLookup(url: String, lookupResult: FileSize.LookupResult): Boolean {
        if (lookupResult.httpStatusCode != HttpURLConnection.HTTP_FORBIDDEN) {
            return false
        }

        if (url.lowercase(Locale.ROOT).contains(".m3u8")) {
            return true
        }

        return lookupResult.resolutionUrl?.encodedPath?.endsWith(".m3u8") == true
    }

    private fun getCachedFileSizeLookup(url: String, resolution: String?): FileSize.LookupResult? {
        cachedFileSizeLookups?.get(FileSizeLookupKey(url, resolution))?.let { cachedLookupResult ->
            if (cachedLookupResult.sizeText.isNotEmpty() || cachedLookupResult.httpStatusCode != null) {
                return cachedLookupResult
            }
        }

        if (
            canBootstrapFileSizeFromNormalQualityUrl &&
            (resolution == null || resolution == FilmResolution.Enum.NORMAL.name) &&
            url.equals(urlNormalQuality, ignoreCase = true) &&
            fileSizeInMegabytes > 0
        ) {
            val cachedSizeInBytes = fileSizeInMegabytes.toLong() * FileSize.ONE_MIB
            val bootstrapLookupResult = FileSize.LookupResult(cachedSizeInBytes, null, null, null)
            fileSizeLookupCache()[FileSizeLookupKey(url, resolution)] = bootstrapLookupResult
            return bootstrapLookupResult
        }

        return null
    }

    private fun cacheFileSizeLookup(url: String, resolution: String?, lookupResult: FileSize.LookupResult) {
        if (lookupResult.sizeText.isEmpty() && lookupResult.httpStatusCode == null) {
            return
        }

        fileSizeLookupCache()[FileSizeLookupKey(url, resolution)] = lookupResult
        if (
            (resolution == null || resolution == FilmResolution.Enum.NORMAL.name) &&
            url.equals(urlNormalQuality, ignoreCase = true)
        ) {
            setFileSize(lookupResult.sizeText)
            canBootstrapFileSizeFromNormalQualityUrl = true
        }
    }

    private fun fileSizeLookupCache(): MutableMap<FileSizeLookupKey, FileSize.LookupResult> =
        cachedFileSizeLookups ?: HashMap<FileSizeLookupKey, FileSize.LookupResult>().also { cachedFileSizeLookups = it }

    val sha256: String
        get() {
            sha256Cache?.let { return it }
            val hash = if (USE_SHA256_FAST_PATH) {
                IntelAmdSha256FastPath.hash(this)
            } else {
                getSha256Default()
            }
            sha256Cache = hash
            return hash
        }

    private fun getSha256Default(): String {
        val digest = createSha256Digest()
        digest.update(sender.toByteArray(StandardCharsets.UTF_16LE))
        digest.update(thema.toByteArray(StandardCharsets.UTF_16LE))
        digest.update(urlNormalQuality.toByteArray(StandardCharsets.UTF_16LE))
        digest.update(websiteUrl.toByteArray(StandardCharsets.UTF_16LE))
        return HexFormat.of().formatHex(digest.digest())
    }

    val filmIdentity: FilmIdentity
        get() {
            filmIdentityCache?.let { return it }

            val identity = FilmIdentity(sender, thema, urlNormalQuality, websiteUrl)
            filmIdentityCache = identity
            return identity
        }

    private fun invalidateSha256() {
        sha256Cache = null
        filmIdentityCache = null
    }

    val isHighQuality: Boolean
        get() = highQualityUrlStorage != null

    fun hasLowQuality(): Boolean = lowQualityUrlStorage != null

    override fun compareTo(other: DatenFilm): Int {
        val senderComparison = sorter.compare(sender, other.sender)
        return if (senderComparison == 0) {
            sorter.compare(thema, other.thema)
        } else {
            senderComparison
        }
    }

    private fun setupDatumFilm() {
        if (sendeDatum.isNotEmpty()) {
            if (datumLongSeconds == 0L) {
                sendeDatum = ""
                sendeZeit = ""
                datumFilmTimeMillisStorage = 0
            } else {
                datumFilmTimeMillisStorage = datumLongSeconds.seconds.inWholeMilliseconds
            }
            datumFilmCache = null
        }
    }

    fun init() {
        setupDatumFilm()
    }

    private fun getUrlNormalOrRequested(resolution: FilmResolution.Enum?): String {
        val requestedUrl = getUrlByResolution(resolution)
        return if (requestedUrl.isEmpty()) {
            urlNormalQuality
        } else {
            try {
                if (isCompressedUrl(requestedUrl)) {
                    decompressUrl(requestedUrl)
                } else {
                    requestedUrl
                }
            } catch (ex: Exception) {
                logger.error("getUrlNormalOrRequested(auflösung: {}, requestedUrl: {})", resolution, requestedUrl, ex)
                ""
            }
        }
    }

    fun decompressUrl(requestedUrl: String): String {
        return decompressUrl(requestedUrl, urlNormalQuality)
    }

    private fun decompressUrl(requestedUrl: String, baseUrl: String): String {
        val indexPipe = requestedUrl.indexOf(COMPRESSION_MARKER)
        val prefixLength = parseCompressionPrefixLength(requestedUrl, indexPipe)
        return buildString(prefixLength + requestedUrl.length - indexPipe - 1) {
            append(baseUrl, 0, prefixLength)
            append(requestedUrl, indexPipe + 1, requestedUrl.length)
        }
    }

    private fun compressUrlIfBeneficial(requestedUrl: String): String {
        val baseUrl = urlNormalQuality
        if (isCompressedUrl(requestedUrl) || baseUrl.isEmpty()) {
            return requestedUrl
        }

        var prefixLength = 0
        val maxPrefixLength = minOf(baseUrl.length, requestedUrl.length)
        while (prefixLength < maxPrefixLength && baseUrl[prefixLength] == requestedUrl[prefixLength]) {
            ++prefixLength
        }

        val compressedUrl = "$prefixLength$COMPRESSION_MARKER${requestedUrl.substring(prefixLength)}"
        return if (compressedUrl.length < requestedUrl.length) compressedUrl else requestedUrl
    }

    private fun parseCompressionPrefixLength(requestedUrl: String, markerIndex: Int): Int {
        if (markerIndex <= 0) {
            throw NumberFormatException(requestedUrl.take(markerIndex.coerceAtLeast(0)))
        }

        var prefixLength = 0
        for (index in 0..<markerIndex) {
            val digit = requestedUrl[index].digitToIntOrNull()
                ?: throw NumberFormatException(requestedUrl.substring(0, markerIndex))
            if (prefixLength > (Int.MAX_VALUE - digit) / 10) {
                throw NumberFormatException(requestedUrl.substring(0, markerIndex))
            }
            prefixLength = prefixLength * 10 + digit
        }
        return prefixLength
    }

    private fun getUrlByResolution(resolution: FilmResolution.Enum?): String =
        when (resolution) {
            FilmResolution.Enum.HIGH_QUALITY -> highQualityUrl
            FilmResolution.Enum.LOW -> lowQualityUrl
            else -> urlNormalQuality
        }

    val filmLengthAsString: String
        get() {
            if (filmLength == 0) {
            return ""
        }
        if (filmLengthAsStringCache.isEmpty()) {
            val duration = filmLength.seconds.inWholeMilliseconds
            filmLengthAsStringCache = DurationFormatUtils.formatDuration(duration, "HH:mm:ss", true)
        }
        return filmLengthAsStringCache
        }

    var urlNormalQuality: String
        get() = UrlHostDictionary.expand(normalQualityUrlStorage)
        set(urlNormalQuality) {
            val previousUrl = this.urlNormalQuality
            normalQualityUrlStorage = UrlHostDictionary.compress(urlNormalQuality)
            handleNormalQualityUrlChange(previousUrl, urlNormalQuality)
            invalidateSha256()
        }

    private fun handleNormalQualityUrlChange(previousUrl: String, newUrl: String) {
        if (previousUrl.equals(newUrl, ignoreCase = true)) {
            return
        }

        rebaseHighQualityUrlStorage(previousUrl)
        cachedFileSizeLookups = null
        if (previousUrl.isNotEmpty() || !canBootstrapFileSizeFromNormalQualityUrl) {
            canBootstrapFileSizeFromNormalQualityUrl = false
        }
    }

    private fun rebaseHighQualityUrlStorage(previousNormalQualityUrl: String) {
        val storedUrl = highQualityUrlStorage ?: return
        val expandedUrl = if (isCompressedUrl(storedUrl)) {
            runCatching { decompressUrl(storedUrl, previousNormalQualityUrl) }.getOrElse { return }
        } else {
            storedUrl
        }
        highQualityUrlStorage = compressUrlIfBeneficial(expandedUrl)
    }

    var subtitleUrl: String
        get() = subtitleUrlStorage?.let(UrlHostDictionary::expand) ?: ""
        set(value) {
            subtitleUrlStorage = value.takeIf { it.isNotEmpty() }?.let(UrlHostDictionary::compress)
        }

    val isBookmarked: Boolean
        get() = bookmark != null

    fun setSeasonEpisode(seasonEpisode: SeasonEpisode) {
        season = seasonEpisode.season
        episode = seasonEpisode.episode
    }

    private object IntelAmdSha256FastPath {
        private const val UTF16LE_BUFFER_SIZE = 8192
        private val digest = ThreadLocal.withInitial { createSha256Digest() }
        private val utf16LeBuffer = ThreadLocal.withInitial { ByteArray(UTF16LE_BUFFER_SIZE) }

        fun hash(film: DatenFilm): String {
            val currentDigest = digest.get()
            currentDigest.reset()
            updateDigestUtf16Le(currentDigest, film.sender)
            updateDigestUtf16Le(currentDigest, film.thema)
            updateDigestUtf16Le(currentDigest, film.urlNormalQuality)
            updateDigestUtf16Le(currentDigest, film.websiteUrl)
            return HexFormat.of().formatHex(currentDigest.digest())
        }

        private fun updateDigestUtf16Le(digest: MessageDigest, value: String) {
            val buffer = utf16LeBuffer.get()
            var position = 0
            var index = 0
            while (index < value.length) {
                if (position + 2 > buffer.size) {
                    digest.update(buffer, 0, position)
                    position = 0
                }
                val ch = value[index]
                if (ch.isSurrogate()) {
                    digest.update(buffer, 0, position)
                    digest.update(value.substring(index).toByteArray(StandardCharsets.UTF_16LE))
                    return
                }
                buffer[position++] = ch.code.toByte()
                buffer[position++] = (ch.code ushr 8).toByte()
                ++index
            }
            if (position > 0) {
                digest.update(buffer, 0, position)
            }
        }
    }

    companion object {
        val EU_COUNTRIES: EnumSet<Country> = EnumSet.of(Country.DE, Country.AT, Country.FR)

        const val FILM_NR = 0
        const val FILM_SENDER = 1
        const val FILM_THEMA = 2
        const val FILM_TITEL = 3
        const val FILM_ABSPIELEN = 4
        const val FILM_AUFZEICHNEN = 5
        const val FILM_MERKEN = 6
        const val FILM_DATUM = 7
        const val FILM_ZEIT = 8
        const val FILM_DAUER = 9
        const val FILM_GROESSE = 10
        const val FILM_HD = 11
        const val FILM_UT = 12
        const val FILM_GEO = 13
        const val FILM_URL = 14
        const val FILM_DATUM_LONG = 15
        const val FILM_REF = 16
        const val MAX_ELEM = 17
        const val COMPRESSION_MARKER = '|'

        private const val FLAG_AUDIO_VERSION = 1 shl 0
        private const val FLAG_TRAILER_TEASER = 1 shl 1
        private const val FLAG_SIGN_LANGUAGE = 1 shl 2
        private const val FLAG_LIVESTREAM = 1 shl 3
        private const val FLAG_NEW_ENTRY = 1 shl 4
        private const val FLAG_BURNED_IN_SUBTITLES = 1 shl 5
        private const val FLAG_PLAYLIST = 1 shl 6
        private const val FLAG_DUPLICATE = 1 shl 7
        private val sorter = GermanStringSorter
        private val logger = LogManager.getLogger(DatenFilm::class.java)
        private val USE_SHA256_FAST_PATH = RuntimeArchitecture.isIntelOrAmd64Bit
        private val UNDEFINED_DATUM_FILM_TIME_MILLIS = DatumFilm.UNDEFINED_FILM_DATE.time
        private const val UNDEFINED_SENDE_DATUM_EPOCH_DAY = Int.MIN_VALUE
        private const val UNDEFINED_SENDE_ZEIT_SECOND_OF_DAY = -1
        private const val SENDE_DATUM_FALLBACK_INDEX = 0
        private const val SENDE_ZEIT_FALLBACK_INDEX = 1
        private const val SENDE_DATE_TIME_FALLBACK_SIZE = 2
        private val FILMNR_GENERATOR = AtomicInteger(0)

        fun isCompressedUrl(requestedUrl: String): Boolean =
            requestedUrl.indexOf(COMPRESSION_MARKER) != -1

        private fun createSha256Digest(): MessageDigest {
            try {
                return MessageDigest.getInstance("SHA-256")
            } catch (ex: NoSuchAlgorithmException) {
                logger.error("Failed to create SHA-256 message digest", ex)
                throw IllegalStateException("SHA-256 algorithm is unavailable", ex)
            }
        }

        private fun parseSendeDatum(value: String): Int? {
            if (value.isEmpty()) {
                return UNDEFINED_SENDE_DATUM_EPOCH_DAY
            }
            if (value.length != 10 || value[2] != '.' || value[5] != '.') {
                return null
            }

            val day = parseTwoDigitPositiveInt(value, 0) ?: return null
            val month = parseTwoDigitPositiveInt(value, 3) ?: return null
            val year = parseFourDigitPositiveInt(value, 6) ?: return null
            return try {
                LocalDate.of(year, month, day).toEpochDay().toInt()
            } catch (_: DateTimeException) {
                null
            }
        }

        private fun formatSendeDatum(epochDay: Int): String {
            val date = LocalDate.ofEpochDay(epochDay.toLong())
            return buildString(10) {
                appendTwoDigits(date.dayOfMonth)
                append('.')
                appendTwoDigits(date.monthValue)
                append('.')
                appendFourDigits(date.year)
            }
        }

        private fun parseSendeZeit(value: String): Int? {
            if (value.isEmpty()) {
                return UNDEFINED_SENDE_ZEIT_SECOND_OF_DAY
            }
            if (value.length != 8 || value[2] != ':' || value[5] != ':') {
                return null
            }

            val hour = parseTwoDigitPositiveInt(value, 0) ?: return null
            val minute = parseTwoDigitPositiveInt(value, 3) ?: return null
            val second = parseTwoDigitPositiveInt(value, 6) ?: return null
            if (hour !in 0..23 || minute !in 0..59 || second !in 0..59) {
                return null
            }
            return hour * 3600 + minute * 60 + second
        }

        private fun formatSendeZeit(secondOfDay: Int): String {
            val hour = secondOfDay / 3600
            val minute = secondOfDay % 3600 / 60
            val second = secondOfDay % 60
            return buildString(8) {
                appendTwoDigits(hour)
                append(':')
                appendTwoDigits(minute)
                append(':')
                appendTwoDigits(second)
            }
        }

        private fun StringBuilder.appendTwoDigits(value: Int) {
            append(('0'.code + value / 10).toChar())
            append(('0'.code + value % 10).toChar())
        }

        private fun StringBuilder.appendFourDigits(value: Int) {
            append(('0'.code + value / 1000 % 10).toChar())
            append(('0'.code + value / 100 % 10).toChar())
            append(('0'.code + value / 10 % 10).toChar())
            append(('0'.code + value % 10).toChar())
        }

        private fun parseTwoDigitPositiveInt(value: String, start: Int): Int? =
            parseFixedWidthPositiveInt(value, start, start + 2)

        private fun parseFourDigitPositiveInt(value: String, start: Int): Int? =
            parseFixedWidthPositiveInt(value, start, start + 4)

        private fun parseFixedWidthPositiveInt(value: String, start: Int, end: Int): Int? {
            var result = 0
            for (index in start..<end) {
                val digit = value[index].digitToIntOrNull() ?: return null
                result = result * 10 + digit
            }
            return result
        }

        private fun packSendeDateTime(epochDay: Int, secondOfDay: Int): Long =
            epochDay.toLong() shl Int.SIZE_BITS or (secondOfDay.toLong() and 0xffffffffL)
    }

    private data class FileSizeLookupKey(
        val url: String,
        val resolution: String?,
    )
}

private object UrlHostDictionary {
    private const val MARKER = '~'
    private val baseIds = HashMap<String, Int>()
    private val bases = ArrayList<String>()

    fun compress(url: String): String {
        if (url.isEmpty() || isCompressed(url)) {
            return url
        }

        val baseEnd = hostPrefixEnd(url)
        if (baseEnd <= 0 || baseEnd >= url.length) {
            return url
        }

        val base = url.substring(0, baseEnd)
        val suffix = url.substring(baseEnd)
        val id = idFor(base)
        val compressed = "$MARKER$id/$suffix"
        return if (compressed.length < url.length) compressed else url
    }

    @Synchronized
    private fun idFor(base: String): Int =
        baseIds[base] ?: bases.size.also { id ->
            bases += base
            baseIds[base] = id
        }

    fun expand(url: String): String {
        if (!isCompressed(url)) {
            return url
        }

        val separator = url.indexOf('/', startIndex = 1)
        if (separator <= 1) {
            return url
        }

        val id = url.substring(1, separator).toIntOrNull() ?: return url
        val base = baseFor(id) ?: return url
        return base + url.substring(separator + 1)
    }

    @Synchronized
    private fun baseFor(id: Int): String? = bases.getOrNull(id)

    private fun isCompressed(url: String): Boolean = url.firstOrNull() == MARKER

    private fun hostPrefixEnd(url: String): Int {
        val schemeSeparator = url.indexOf("://")
        if (schemeSeparator <= 0) {
            return -1
        }
        val pathStart = url.indexOf('/', startIndex = schemeSeparator + 3)
        return if (pathStart < 0) -1 else pathStart + 1
    }
}
