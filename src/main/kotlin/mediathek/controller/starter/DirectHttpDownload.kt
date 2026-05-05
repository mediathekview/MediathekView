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

package mediathek.controller.starter

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import mediathek.config.Config
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.controller.ByteRateLimiter
import mediathek.controller.MVBandwidthCountingInputStream
import mediathek.controller.ThrottlingInputStream
import mediathek.controller.history.SeenHistoryController
import mediathek.daten.DatenDownload
import mediathek.gui.dialog.DialogContinueDownload
import mediathek.gui.dialog.MeldungDownloadfehler
import mediathek.gui.messages.*
import mediathek.mainwindow.MediathekGui
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.FileSize
import mediathek.tool.FileUtils
import mediathek.tool.MessageBus
import mediathek.tool.http.MVHttpClient
import net.engio.mbassy.bus.MBassador
import net.engio.mbassy.listener.Handler
import okhttp3.*
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.internal.http2.StreamResetException
import org.apache.logging.log4j.LogManager
import java.io.BufferedOutputStream
import java.io.File
import java.io.IOException
import java.io.InputStream
import java.net.HttpURLConnection
import java.nio.file.Files
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.time.Duration
import java.time.LocalDateTime
import java.util.*
import javax.swing.SwingUtilities
import kotlin.time.Duration.Companion.milliseconds

class DirectHttpDownload(
    private val daten: Daten,
    private val datenDownload: DatenDownload
) : Thread() {

    private val start: Start = datenDownload.start
    private val rateLimiter = ByteRateLimiter(downloadLimit())
    private val messageBus: MBassador<BaseEvent> = MessageBus.messageBus
    private val httpClient: OkHttpClient = MVHttpClient.httpClient
    private val http11Client: OkHttpClient = httpClient.newBuilder()
        .protocols(listOf(Protocol.HTTP_1_1))
        .build()

    private var state = HttpDownloadState.DOWNLOAD
    private var alreadyDownloaded = 0L
    private lateinit var finalFile: File
    private lateinit var file: File
    private var retAbbrechen = false
    private var dialogAbbrechenIsVis = false
    private var ancillaryDownloads = DirectDownloadAncillaryFiles.empty(logger)

    init {
        messageBus.subscribe(this)
        name = "DIRECT DL THREAD_${datenDownload.arr[DatenDownload.DOWNLOAD_TITEL]}"

        start.status = Start.STATUS_RUN
        StarterClass.notifyStartEvent(datenDownload)
    }

    /**
     * Handles the rate limit change launched somewhere in the UI
     *
     * @param evt the new limit
     */
    @Handler
    private fun handleRateLimitChanged(evt: DownloadRateLimitChangedEvent) {
        val limit = calcLimit(evt.newLimit.toLong(), evt.active)
        logger.info("thread changing download speed limit to {} KB", limit)
        rateLimiter.setRate(limit)
    }

    private fun calcLimit(limit: Long, active: Boolean): Long {
        return if (limit <= 0 || !active) {
            Long.MAX_VALUE
        } else {
            limit * FileUtils.ONE_KB
        }
    }

    private fun calculateDownloadLimit(limit: Long): Long {
        val active = ApplicationConfiguration.getConfiguration()
            .getBoolean(ApplicationConfiguration.DownloadRateLimiter.ACTIVE, false)
        return calcLimit(limit, active)
    }

    /**
     * Try to read the download limit from config file, other set to artificial limit 1GB/s!
     *
     * @return the limit in KB/s
     */
    private fun downloadLimit(): Long {
        val downloadLimit = ApplicationConfiguration.getConfiguration()
            .getLong(ApplicationConfiguration.DownloadRateLimiter.LIMIT, 0)
        return calculateDownloadLimit(downloadLimit)
    }

    /**
     * Return the content length of the requested Url.
     *
     * @param url [java.net.URL] to the specified content.
     * @return Length in bytes or -1 on error.
     */
    @Throws(IOException::class)
    private fun getContentLength(url: HttpUrl, client: OkHttpClient): Long {
        val request = Request.Builder().url(url).head()
            .header("User-Agent", userAgent())
            .build()

        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) {
                return -1
            }

            var contentSize = FileSize.getContentLength(response)
            // alles unter 300k sind Playlisten, ...
            if (contentSize < 300_000) {
                contentSize = -1
            }
            return contentSize
        }
    }

    @Throws(IOException::class)
    private fun getContentLengthWithFallback(url: HttpUrl): Long {
        return try {
            getContentLength(url, httpClient)
        } catch (ex: IOException) {
            if (!isRetryableStreamException(ex)) {
                throw ex
            }

            logger.info("HEAD request failed for {}, retrying with HTTP/1.1", url, ex)
            getContentLength(url, http11Client)
        }
    }

    private fun userAgent(): String {
        return ApplicationConfiguration.getConfiguration()
            .getString(ApplicationConfiguration.APPLICATION_USER_AGENT)
    }

    /**
     * Start the actual download process here.
     *
     * @throws IOException the io errors that may occur.
     */
    @Throws(IOException::class)
    private fun CoroutineScope.downloadContent(inputStream: InputStream) {
        ancillaryDownloads = DirectDownloadAncillaryFiles.start(this, datenDownload, logger)
        datenDownload.interruptRestart()
        datenDownload.mVFilmSize.aktSize = alreadyDownloaded

        val options = if (alreadyDownloaded != 0L) {
            arrayOf(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND)
        } else {
            arrayOf(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
        }

        Files.newOutputStream(file.toPath(), *options).use { fileSink ->
            BufferedOutputStream(fileSink, DOWNLOAD_BUFFER_SIZE).use { bufferedSink ->
                ThrottlingInputStream(inputStream, rateLimiter).use { throttledInput ->
                    MVBandwidthCountingInputStream(throttledInput).use { bandwidthInput ->
                        start.mVBandwidthCountingInputStream = bandwidthInput
                        val buffer = ByteArray(DOWNLOAD_BUFFER_SIZE)
                        var previousProgress = 0L
                        var startProgress = -1L
                        var aktSize = 0L
                        var melden = false

                        while (!start.stoppen) {
                            val len = bandwidthInput.read(buffer)
                            if (len == -1) {
                                break
                            }
                            alreadyDownloaded += len.toLong()
                            bufferedSink.write(buffer, 0, len)
                            datenDownload.mVFilmSize.addAktSize(len.toLong())

                            //für die Anzeige prüfen ob sich was geändert hat
                            if (aktSize != datenDownload.mVFilmSize.aktSize) {
                                aktSize = datenDownload.mVFilmSize.aktSize
                                melden = true
                            }
                            if (datenDownload.mVFilmSize.size > 0) {
                                var progress = aktSize * 1000L / datenDownload.mVFilmSize.size
                                if (startProgress == -1L) {
                                    startProgress = progress
                                }
                                // p muss zwischen 1 und 999 liegen
                                progress = when {
                                    progress == 0L -> Start.PROGRESS_GESTARTET.toLong()
                                    progress >= 1000L -> 999L
                                    else -> progress
                                }
                                start.percent = progress.toInt()
                                if (progress != previousProgress) {
                                    previousProgress = progress
                                    // Restzeit ermitteln
                                    if (progress > 2 && progress > startProgress) {
                                        // sonst macht es noch keinen Sinn
                                        val diffZeit = Duration.between(start.startTime, LocalDateTime.now()).seconds
                                        val restProzent = 1000L - progress
                                        start.restSekunden = diffZeit * restProzent / (progress - startProgress)
                                    }
                                    melden = true
                                }
                            }
                            val aktBandwidth = bandwidthInput.bandwidth // bytes per second
                            if (aktBandwidth != start.bandbreite) {
                                start.bandbreite = aktBandwidth
                                melden = true
                            }
                            if (melden) {
                                DownloadProgressEventPublisher.publishThrottled()
                                melden = false
                            }
                        }
                        bufferedSink.flush()
                    }
                }
            }
        }

        start.bandbreite = start.mVBandwidthCountingInputStream.sumBandwidth
        finishSuccessfulDownload()
    }

    @Throws(IOException::class)
    private fun finishSuccessfulDownload() {
        if (!start.stoppen) {
            DirectDownloadPartFiles.moveCompletedPartToFinal(file, finalFile)

            start.status = when {
                datenDownload.quelle == DatenDownload.QUELLE_BUTTON -> Start.STATUS_FERTIG
                StarterClass.pruefen(daten, datenDownload, start) -> Start.STATUS_FERTIG
                else -> Start.STATUS_ERR
            }
        }
    }

    private fun printHttpErrorMessage(response: Response) {
        val responseCode = "Responsecode: ${response.code}\n${response.message}"
        logger.error("HTTP-Fehler: {} {}", response.code, response.message)

        if (start.countRestarted >= Konstanten.MAX_DOWNLOAD_RESTARTS) {
            showDownloadError("URL des Films:\n${datenDownload.arr[DatenDownload.DOWNLOAD_URL]}\n\n$responseCode\n")
        }

        state = HttpDownloadState.ERROR
        start.status = Start.STATUS_ERR
    }

    private fun buildDownloadRequest(url: HttpUrl): Request {
        val request = Request.Builder().url(url).get()
            .header("User-Agent", userAgent())
        if (alreadyDownloaded != 0L) {
            request.header("Range", "bytes=$alreadyDownloaded-")
        }

        return request.build()
    }

    @Throws(IOException::class)
    private fun CoroutineScope.executeDownloadRequest(url: HttpUrl, client: OkHttpClient): Boolean {
        val request = buildDownloadRequest(url)
        client.newCall(request).execute().use { response ->
            if (response.isSuccessful) {
                downloadContent(response.body.byteStream())
                return true
            }

            val responseCode = response.code
            if (responseCode == HTTP_RANGE_NOT_SATISFIABLE) {
                // Reset and try once again without range.
                alreadyDownloaded = 0
                val retryRequest = buildDownloadRequest(url)
                client.newCall(retryRequest).execute().use { retryResponse ->
                    if (retryResponse.isSuccessful) {
                        downloadContent(retryResponse.body.byteStream())
                        return true
                    }
                    printHttpErrorMessage(retryResponse)
                    return false
                }
            }

            if (responseCode == HttpURLConnection.HTTP_NOT_FOUND) {
                logger.error("HTTP error 404 received for URL: {}", request.url.toString())
                state = HttpDownloadState.ERROR
                start.status = Start.STATUS_ERR
            } else {
                printHttpErrorMessage(response)
            }
            return false
        }
    }

    private fun isHttp2InternalStreamReset(ex: IOException): Boolean {
        var current: Throwable? = ex
        while (current != null) {
            if (current is StreamResetException) {
                val msg = current.message.toString()
                if (msg.contains("INTERNAL_ERROR")) {
                    return true
                }
            }

            val msg = current.message
            if (msg != null) {
                val lower = msg.lowercase(Locale.ROOT)
                if (lower.contains("stream was reset") && lower.contains("internal_error")) {
                    return true
                }
            }
            current = current.cause
        }
        return false
    }

    private fun isRetryableStreamException(ex: IOException): Boolean {
        if (isHttp2InternalStreamReset(ex)) {
            return true
        }

        val lower = ex.message?.lowercase(Locale.ROOT) ?: return false
        return lower.contains("stream was reset") ||
                lower.contains("unexpected end of stream") ||
                lower.contains("connection reset") ||
                lower.contains("broken pipe") ||
                lower.contains("remote host terminated handshake")
    }

    private suspend fun waitForRetry(retryCount: Int, ex: IOException) {
        logger.warn(
            "Transient download error (retry {}/{}), resuming at byte {}",
            retryCount,
            MAX_TRANSIENT_DOWNLOAD_RETRIES,
            alreadyDownloaded,
            ex
        )
        delay(RETRY_DELAY_MILLIS.milliseconds)
    }

    @Synchronized
    override fun run() {
        StarterClass.startmeldung(datenDownload, start)

        messageBus.publishAsync(DownloadStartEvent())

        runBlocking {
            try {
                createDirectory()
                finalFile = File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
                file = DirectDownloadPartFiles.partFileFor(finalFile)

                if (!cancelDownload()) {
                    val url = datenDownload.arr[DatenDownload.DOWNLOAD_URL].toHttpUrlOrNull()
                        ?: throw IOException("Invalid download URL: ${datenDownload.arr[DatenDownload.DOWNLOAD_URL]}")
                    datenDownload.mVFilmSize.size = getContentLengthWithFallback(url)
                    datenDownload.mVFilmSize.aktSize = 0
                    var retryCount = 0
                    var forceHttp11 = false

                    while (!start.stoppen) {
                        val client = if (forceHttp11) http11Client else httpClient
                        try {
                            executeDownloadRequest(url, client)
                            break
                        } catch (ex: IOException) {
                            if (isRetryableStreamException(ex) && retryCount < MAX_TRANSIENT_DOWNLOAD_RETRIES) {
                                retryCount++
                                forceHttp11 = true
                                waitForRetry(retryCount, ex)
                                continue
                            }
                            throw ex
                        }
                    }
                }
            } catch (ex: IOException) {
                logger.error("run()", ex)
                start.status = Start.STATUS_ERR
                state = HttpDownloadState.ERROR

                removeSeenHistoryEntry()

                showDownloadError(ex.localizedMessage)
            } finally {
                awaitAncillaryDownloads()

                StarterClass.finalizeDownload(datenDownload, start, state)

                messageBus.publishAsync(DownloadFinishedEvent())
                messageBus.unsubscribe(this@DirectHttpDownload)
            }
        }
    }

    private fun removeSeenHistoryEntry() {
        datenDownload.film?.let {
            logger.trace("Removing failed download entry from history")
            SeenHistoryController().use { historyController ->
                historyController.markUnseen(it)
            }
        }
    }

    private suspend fun awaitAncillaryDownloads() {
        ancillaryDownloads.await()
    }

    private suspend fun cancelDownload(): Boolean {
        if (!file.exists() && !finalFile.exists()) {
            // dann ist alles OK
            return false
        }

        if (Config.isDownloadAndQuit()) {
            return resolveExistingDownloadForCli()
        }

        dialogAbbrechenIsVis = true
        retAbbrechen = true
        if (SwingUtilities.isEventDispatchThread()) {
            retAbbrechen = abbrechen()
        } else {
            SwingUtilities.invokeLater {
                retAbbrechen = abbrechen()
                dialogAbbrechenIsVis = false
            }
        }
        while (dialogAbbrechenIsVis) {
            delay(DIALOG_POLL_DELAY_MILLIS.milliseconds)
        }
        return retAbbrechen
    }

    private fun createDirectory() {
        try {
            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]))
        } catch (_: IOException) {
        }
    }

    private fun abbrechen(): Boolean {
        var result = false
        if (file.exists() || finalFile.exists()) {
            val hasPartFile = file.exists()
            val dialogContinueDownload = DialogContinueDownload(MediathekGui.ui(), datenDownload, true)
            dialogContinueDownload.isVisible = true

            when (dialogContinueDownload.result) {
                DialogContinueDownload.DownloadResult.CANCELLED -> {
                    // dann wars das
                    state = HttpDownloadState.CANCEL
                    result = true
                }

                DialogContinueDownload.DownloadResult.CONTINUE -> {
                    if (!hasPartFile && !moveLegacyFinalFileToPart()) {
                        state = HttpDownloadState.ERROR
                        result = true
                    } else {
                        alreadyDownloaded = file.length()
                    }
                }

                DialogContinueDownload.DownloadResult.RESTART_WITH_NEW_NAME -> {
                    if (dialogContinueDownload.isNewName) {
                        MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                        createDirectory()
                        finalFile = File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
                        file = DirectDownloadPartFiles.partFileFor(finalFile)
                    }
                }
            }
        }
        return result
    }

    private fun resolveExistingDownloadForCli(): Boolean {
        val hasPartFile = file.exists()
        logger.info(
            "CLI download mode: continuing existing direct download for {}",
            datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]
        )
        if (!hasPartFile && !moveLegacyFinalFileToPart()) {
            state = HttpDownloadState.ERROR
            return true
        }
        alreadyDownloaded = file.length()
        return false
    }

    private fun showDownloadError(message: String?) {
        if (Config.isDownloadAndQuit()) {
            logger.error("Download failed for {}: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME], message)
            return
        }
        SwingUtilities.invokeLater {
            MeldungDownloadfehler(MediathekGui.ui(), message, datenDownload).isVisible = true
        }
    }

    private fun moveLegacyFinalFileToPart(): Boolean {
        return try {
            DirectDownloadPartFiles.moveLegacyFinalFileToPart(finalFile, file)
            true
        } catch (ex: IOException) {
            logger.error("Failed to move existing download to part file", ex)
            false
        }
    }

    companion object {
        private const val HTTP_RANGE_NOT_SATISFIABLE = 416
        private const val MAX_TRANSIENT_DOWNLOAD_RETRIES = 3
        private const val RETRY_DELAY_MILLIS = 1_000L
        private const val DIALOG_POLL_DELAY_MILLIS = 100L
        /**
         * Keep the transfer buffer large enough that rate limiting does not depend on sub-millisecond sleep precision.
         * Windows is especially sensitive here when the limiter is driven by many 1 KiB reads per second.
         */
        private const val DOWNLOAD_BUFFER_SIZE = 256 * 1024
        private val logger = LogManager.getLogger(DirectHttpDownload::class.java)
    }
}
