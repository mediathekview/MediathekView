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

import mediathek.controller.starter.DownloadRuntimeState
import mediathek.controller.starter.StartStatus
import mediathek.daten.abo.DatenAbo
import mediathek.tool.*
import mediathek.tool.datum.Datum
import org.apache.commons.lang3.time.FastDateFormat
import org.apache.logging.log4j.LogManager
import java.net.URI
import java.net.URISyntaxException
import javax.xml.stream.XMLStreamException
import javax.xml.stream.XMLStreamReader

class DatenDownload() : Comparable<DatenDownload> {
    var datumFilm: Datum = Datum(0)

    var film: DatenFilm? = null

    val runtime: DownloadRuntimeState = DownloadRuntimeState()

    var pSet: DatenPset? = null

    var abo: DatenAbo? = null

    var nr: Int = 0

    var quelle: DownloadSource = DownloadSource.ALL

    var art: DownloadType = DownloadType.DIRECT

    private var websiteUrl = ""
    private var legacyTypeText: String? = null
    private var legacySourceText: String? = null

    constructor(
        pSet: DatenPset,
        film: DatenFilm,
        quelle: DownloadSource,
        abo: DatenAbo?,
        name: String,
        pfad: String,
        aufloesung: String,
    ) : this() {
        this.film = film
        this.pSet = pSet
        this.abo = abo
        this.quelle = quelle
        sender = film.sender
        topic = film.thema
        title = film.title
        filmUrl = film.urlNormalQuality
        subtitleUrl = film.subtitleUrl
        date = film.sendeDatum
        time = film.sendeZeit
        duration = film.filmLengthAsString
        historyUrl = film.urlNormalQuality
        downloadUrl = if (aufloesung.isEmpty()) {
            film.getUrlFuerAufloesung(pSet.aufloesung)
        } else {
            film.getUrlFuerAufloesung(FilmResolution.Enum.fromLegacyString(aufloesung))
        }

        if (downloadUrl.contains("?")) {
            downloadUrl = getUrlWithoutParameters(downloadUrl)
        }

        isInfoFile = pSet.shouldCreateInfofile()
        isSubtitle = pSet.shouldDownloadSubtitle()
        isSpotlight = pSet.isSpotlight
        geo = if (!film.hasCountries()) "" else film.countriesAsString

        websiteUrl = film.websiteUrl

        setGroesse("")

        aufrufBauen(pSet, film, abo, name, pfad)
        init()
    }

    constructor(
        pSet: DatenPset,
        film: DatenFilm,
        quelle: DownloadSource,
        abo: DatenAbo?,
        name: String,
        pfad: String,
        aufloesung: String,
        info: Boolean,
        subtitle: Boolean,
    ) : this(pSet, film, quelle, abo, name, pfad, aufloesung) {
        isInfoFile = info
        isSubtitle = subtitle
    }

    internal fun applyLegacyColumn(index: Int, value: String) {
        require(index in 0 until DownloadColumns.COUNT) { "Unknown legacy download column: $index" }
        when (index) {
            DownloadColumns.NR -> nr = value.toIntOrNull() ?: nr
            DownloadColumns.ABO -> aboName = value
            DownloadColumns.SENDER -> sender = value
            DownloadColumns.TOPIC -> topic = value
            DownloadColumns.TITLE -> title = value
            DownloadColumns.SIZE -> storedSizeInMiB = value.toLongOrNull()
            DownloadColumns.DATE -> date = value
            DownloadColumns.TIME -> time = value
            DownloadColumns.DURATION -> duration = value
            DownloadColumns.INTERRUPTED -> isInterruptedFlag = value.toBoolean()
            DownloadColumns.GEO -> geo = value
            DownloadColumns.FILM_URL -> filmUrl = value
            DownloadColumns.HISTORY_URL -> historyUrl = value
            DownloadColumns.URL -> downloadUrl = value
            DownloadColumns.RTMP_URL -> rtmpUrl = value
            DownloadColumns.SUBTITLE_URL -> subtitleUrl = value
            DownloadColumns.PROGRAM_SET -> programSetName = value
            DownloadColumns.PROGRAM -> programName = value
            DownloadColumns.PROGRAM_INVOCATION -> programInvocation = value
            DownloadColumns.PROGRAM_INVOCATION_ARRAY -> programInvocationArray = value
            DownloadColumns.PROGRAM_RESTART -> isRestart = value.toBoolean()
            DownloadColumns.TARGET_FILE_NAME -> targetFileName = value
            DownloadColumns.TARGET_PATH -> targetPath = value
            DownloadColumns.TARGET_PATH_FILE_NAME -> targetPathFileName = value
            DownloadColumns.TYPE -> legacyTypeText = value
            DownloadColumns.SOURCE -> legacySourceText = value
            DownloadColumns.DEFERRED -> isDeferred = value.toBoolean()
            DownloadColumns.INFO_FILE -> isInfoFile = value.toBoolean()
            DownloadColumns.SPOTLIGHT -> isSpotlight = value.toBoolean()
            DownloadColumns.SUBTITLE -> isSubtitle = value.toBoolean()
            DownloadColumns.DOWNLOAD_MANAGER -> isDownloadManager = value.toBoolean()
        }
    }

    var aboName: String = ""

    var sender: String = ""

    var topic: String = ""

    var title: String = ""

    var historyUrl: String = ""

    var filmUrl: String = ""

    var downloadUrl: String = ""

    var subtitleUrl: String = ""

    var rtmpUrl: String = ""

    var date: String = ""

    var time: String = ""

    var duration: String = ""

    var geo: String = ""

    var programSetName: String = ""

    var programName: String = ""

    var programInvocation: String = ""

    var programInvocationArray: String = ""

    var targetFileName: String = ""

    var targetPath: String = ""

    var targetPathFileName: String = ""

    fun setTarget(fileSpecifier: FileSpecifier) {
        targetFileName = fileSpecifier.fileName
        targetPath = fileSpecifier.path
        targetPathFileName = GuiFunktionen.addsPfad(fileSpecifier.path, fileSpecifier.fileName)
    }

    private fun getUrlWithoutParameters(url: String): String =
        try {
            val uri = URI(url)
            URI(
                uri.scheme,
                uri.authority,
                uri.path,
                null,
                uri.fragment,
            ).toString()
        } catch (e: URISyntaxException) {
            logger.error("Failed to parse url, returning unmodified", e)
            url
        }

    fun setGroesseFromFilm() {
        val currentFilm = film ?: return
        if (currentFilm.urlNormalQuality == downloadUrl) {
            runtime.filmSize.setSize(currentFilm.fileSize.toString())
        } else {
            runtime.filmSize.size = 0
        }
    }

    fun setGroesse(groesse: String) {
        if (film != null && groesse.isNotEmpty()) {
            runtime.filmSize.setSize(groesse)
        }
    }

    fun queryLiveSize() {
        val currentFilm = film ?: return
        runtime.filmSize.setSize(currentFilm.getFileSizeForUrl(downloadUrl))
    }

    fun init() {
        datumFilm = getDatumForObject()
        applyPendingLegacyTypeAndSource()
        applyStoredSizeInMiB()
    }

    private fun applyPendingLegacyTypeAndSource() {
        if (legacyTypeText != null || legacySourceText != null) {
            applyLegacyTypeAndSource(legacyTypeText.orEmpty(), legacySourceText.orEmpty())
        }
    }

    private fun applyStoredSizeInMiB() {
        storedSizeInMiB?.let { size ->
            runtime.filmSize.size = size * FileSize.ONE_MiB
        }
    }

    private var storedSizeInMiB: Long? = null

    private fun applyLegacyTypeAndSource(type: String, source: String) {
        try {
            art = DownloadType.fromLegacyText(type)
            quelle = DownloadSource.fromLegacyText(source)
        } catch (ex: Exception) {
            logger.error("Art: {}, Quelle: {}", type, source, ex)
            art = DownloadType.PROGRAM
            quelle = DownloadSource.BUTTON
        }
    }

    internal fun toConfig(): DownloadConfig =
        DownloadConfig(
            aboName = aboName,
            sender = sender,
            topic = topic,
            title = title,
            sizeInMiB = runtime.filmSize.size / FileSize.ONE_MiB,
            date = date,
            time = time,
            duration = duration,
            interrupted = isInterruptedFlag,
            filmUrl = filmUrl,
            historyUrl = historyUrl,
            url = downloadUrl,
            rtmpUrl = rtmpUrl,
            subtitleUrl = subtitleUrl,
            programSet = programSetName,
            program = programName,
            programInvocation = programInvocation,
            programInvocationArray = programInvocationArray,
            restart = isRestart,
            targetFileName = targetFileName,
            targetPath = targetPath,
            targetPathFileName = targetPathFileName,
            type = art,
            source = quelle,
            deferred = isDeferred,
            infoFile = isInfoFile,
            spotlight = isSpotlight,
            subtitle = isSubtitle,
            downloadManager = isDownloadManager,
        )

    var isDeferred: Boolean = false
        internal set

    internal var isInterruptedFlag: Boolean = false

    val isInterrupted: Boolean
        get() = !isFinished && isInterruptedFlag

    fun notStarted(): Boolean = runtime.runState == null

    val isWaiting: Boolean
        get() = runtime.runState?.status == StartStatus.INITIALIZED

    val isFinished: Boolean
        get() = runtime.runState?.status == StartStatus.FINISHED

    fun runNotFinished(): Boolean =
        runtime.runState?.status?.isBefore(StartStatus.FINISHED) == true

    fun running(): Boolean =
        runtime.runState?.status == StartStatus.RUNNING

    private fun copyTo(target: DatenDownload) {
        target.aboName = aboName
        target.sender = sender
        target.topic = topic
        target.title = title
        target.historyUrl = historyUrl
        target.filmUrl = filmUrl
        target.downloadUrl = downloadUrl
        target.subtitleUrl = subtitleUrl
        target.rtmpUrl = rtmpUrl
        target.date = date
        target.time = time
        target.duration = duration
        target.geo = geo
        target.programSetName = programSetName
        target.programName = programName
        target.programInvocation = programInvocation
        target.programInvocationArray = programInvocationArray
        target.targetFileName = targetFileName
        target.targetPath = targetPath
        target.targetPathFileName = targetPathFileName
        target.storedSizeInMiB = storedSizeInMiB
        target.legacyTypeText = legacyTypeText
        target.legacySourceText = legacySourceText
        target.isDeferred = isDeferred
        target.isInterruptedFlag = isInterruptedFlag
        target.isRestart = isRestart
        target.isDownloadManager = isDownloadManager
        target.isInfoFile = isInfoFile
        target.isSubtitle = isSubtitle
        target.isSpotlight = isSpotlight
        target.quelle = quelle
        target.art = art
        target.websiteUrl = websiteUrl
        target.datumFilm = datumFilm
        target.film = film
        target.runtime.copyFrom(runtime)
        target.pSet = pSet
        target.abo = abo
        target.nr = nr
    }

    val copy: DatenDownload
        get() {
            val ret = DatenDownload()
            copyTo(ret)
            return ret
        }

    fun aufMichKopieren(datenDownload: DatenDownload) {
        datenDownload.copyTo(this)
    }

    val isFromAbo: Boolean
        get() = aboName.isNotEmpty()

    val isAutomaticStartBlockedByAbo: Boolean
        get() = isFromAbo && abo?.isDoNotStartAutomatically == true

    var isRestart: Boolean = false

    var isDownloadManager: Boolean = false

    var isInfoFile: Boolean = false

    var isSubtitle: Boolean = false

    var isSpotlight: Boolean = false

    val textRestzeit: String
        get() = DownloadRuntimeText.remainingTime(runtime.runState)

    val textBandbreite: String
        get() = DownloadRuntimeText.bandwidth(runtime.runState)

    fun checkAufrufBauen(): Boolean =
        pSet != null && film != null

    fun aufrufBauen() {
        aufrufBauen(
            checkNotNull(pSet),
            checkNotNull(film),
            abo,
            targetFileName,
            targetPath,
        )
    }

    private fun aufrufBauen(pSet: DatenPset, film: DatenFilm, abo: DatenAbo?, nname: String, ppfad: String) {
        try {
            val programm = pSet.getProgUrl(downloadUrl)
            pSet.zielDateiname = pSet.zielDateiname.replace("%n", "").replace("%p", "")
            pSet.zielPfad = pSet.zielPfad.replace("%n", "").replace("%p", "")

            for (prog in pSet.listeProg) {
                prog.targetFileName = prog.targetFileName.replace("%n", "").replace("%p", "")
            }

            programSetName = pSet[DatenPset.PROGRAMMSET_NAME]

            art = if (pSet.checkDownloadDirekt(downloadUrl) && pSet.progsContainPath()) {
                DownloadType.DIRECT
            } else {
                DownloadType.PROGRAM
            }
            programName = if (art == DownloadType.DIRECT) {
                DownloadType.DIRECT.label
            } else {
                programm?.name ?: "Unknown"
            }
            if (programm != null) {
                isRestart = programm.isRestart
                isDownloadManager = programm.isDownloadManager
                applyTarget(DownloadTargetBuilder.build(createTargetRequest(pSet, film, abo, nname, ppfad)))
                applyInvocation(programm)
            }
        } catch (ex: Exception) {
            logger.error("aufrufBauen", ex)
        }
    }

    private fun createTargetRequest(
        pSet: DatenPset,
        film: DatenFilm,
        abo: DatenAbo?,
        fileName: String,
        path: String,
    ): DownloadTargetRequest =
        DownloadTargetRequest(
            pSet = pSet,
            film = film,
            abo = abo,
            requestedFileName = fileName,
            requestedPath = path,
            downloadUrl = downloadUrl,
            topic = topic,
            title = title,
        )

    private fun applyTarget(target: DownloadTarget) {
        targetFileName = target.fileName
        targetPath = target.path
        target.pathFileName?.let { pathFileName ->
            targetPathFileName = pathFileName
        }
        target.aboName?.let { aboName = it }
    }

    private fun applyInvocation(programm: DatenProg) {
        val invocation = DownloadProgramInvocationBuilder.build(
            downloadType = art,
            program = programm,
            request = DownloadInvocationRequest(
                downloadUrl = downloadUrl,
                rtmpUrl = rtmpUrl,
                targetPath = targetPath,
                targetFileName = targetFileName,
                targetPathFileName = targetPathFileName,
                websiteUrl = websiteUrl,
            ),
        )
        programInvocation = invocation.command
        programInvocationArray = invocation.commandArray
    }

    fun getDatumForObject(): Datum {
        val tmp = Datum(0)
        if (date.isNotEmpty()) {
            try {
                if (time.isNotEmpty()) {
                    tmp.time = sdf_datum_zeit.parse(date + time).time
                } else {
                    tmp.time = sdf_datum.parse(date).time
                }
            } catch (ex: Exception) {
                logger.error("Datum: {}, Zeit: {}", date, time, ex)
            }
        }
        return tmp
    }

    val fileNameWithoutSuffix: String
        get() = GuiFunktionen.getFileNameWithoutExtension(targetPathFileName)

    override fun compareTo(other: DatenDownload): Int {
        val ret = sorter.compare(sender, other.sender)
        return if (ret == 0) {
            sorter.compare(topic, other.topic)
        } else {
            ret
        }
    }

    companion object {
        const val TAG = "Downlad"

        private val sorter = GermanStringSorter
        private val sdf_datum_zeit = FastDateFormat.getInstance("dd.MM.yyyyHH:mm:ss")
        private val sdf_datum = FastDateFormat.getInstance("dd.MM.yyyy")
        private val logger = LogManager.getLogger(DatenDownload::class.java)

        internal fun fromConfig(config: DownloadConfig): DatenDownload =
            DatenDownload().also { download ->
                download.aboName = config.aboName
                download.sender = config.sender
                download.topic = config.topic
                download.title = config.title
                download.storedSizeInMiB = config.sizeInMiB
                download.date = config.date
                download.time = config.time
                download.duration = config.duration
                download.isInterruptedFlag = config.interrupted
                download.filmUrl = config.filmUrl
                download.historyUrl = config.historyUrl
                download.downloadUrl = config.url
                download.rtmpUrl = config.rtmpUrl
                download.subtitleUrl = config.subtitleUrl
                download.programSetName = config.programSet
                download.programName = config.program
                download.programInvocation = config.programInvocation
                download.programInvocationArray = config.programInvocationArray
                download.isRestart = config.restart
                download.targetFileName = config.targetFileName
                download.targetPath = config.targetPath
                download.targetPathFileName = config.targetPathFileName
                download.art = config.type
                download.quelle = config.source
                download.isDeferred = config.deferred
                download.isInfoFile = config.infoFile
                download.isSpotlight = config.spotlight
                download.isSubtitle = config.subtitle
                download.isDownloadManager = config.downloadManager
                download.init()
            }

        @Throws(XMLStreamException::class)
        fun readFromConfig(parser: XMLStreamReader): DatenDownload =
            LegacyDownloadXmlReader.read(parser)

    }
}
