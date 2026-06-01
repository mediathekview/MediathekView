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

import kotlinx.coroutines.coroutineScope
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
import mediathek.daten.DownloadSource
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

class CdnAwareDirectDownloadThread(
    private val datenDownload: DatenDownload
) : Thread("CDN AWARE DIRECT DL THREAD_${datenDownload.title}") {

    private val logger = LogManager.getLogger(javaClass)
    private val start = checkNotNull(datenDownload.runtime.runState)
    private val rateLimiter = ByteRateLimiter(downloadLimit())
    private val messageBus: MBassador<BaseEvent> = MessageBus.messageBus
    private val client: OkHttpClient = MVHttpClient.httpClient.newBuilder()
        .protocols(listOf(Protocol.HTTP_1_1))
        .dispatcher(dispatcher)
        .build()

    private var state = HttpDownloadState.DOWNLOAD
    private var alreadyDownloaded = 0L
    private lateinit var finalFile: File
    private lateinit var file: File
    private var retAbbrechen = false
    private var dialogAbbrechenIsVis = false
    private var ancillaryDownloads = DirectDownloadAncillaryFiles.empty(logger)
    private var previousProgress = 0L
    private var startProgress = -1L
    private val bandwidthStartedAtNanos = System.nanoTime()
    private var bandwidthWindowStartNanos = bandwidthStartedAtNanos
    private var totalBytesRead = 0L
    private var bandwidthWindowBytesRead = 0L
    private var currentBandwidth = 0L

    init {
        messageBus.subscribe(this)
        start.markRunning()
        DownloadStartEventPublisher.publish(datenDownload)
    }

    @Handler
    private fun handleRateLimitChanged(evt: DownloadRateLimitChangedEvent) {
        val limit = calcLimit(evt.newLimit.toLong(), evt.active)
        logger.info("thread changing download speed limit to {} KB", limit)
        rateLimiter.setRate(limit)
    }

    @Synchronized
    override fun run() {
        DownloadLogMessages.logStart(datenDownload, start)
        messageBus.publishAsync(DownloadStartEvent())

        runBlocking {
            try {
                createDirectory()
                finalFile = File(datenDownload.targetPathFileName)
                file = DirectDownloadPartFiles.partFileFor(finalFile)

                if (!cancelDownload()) {
                    val url = parseDownloadUrl()
                    logger.info(
                        "Using dedicated HTTP/1.1 chunked downloader for sender {}",
                        datenDownload.sender
                    )

                    val contentLength = getContentLength(url)
                    if (contentLength <= 0) {
                        throw IOException("Could not determine content length for download")
                    }

                    datenDownload.runtime.filmSize.size = contentLength
                    prepareAncillaryDownloads()
                    executeChunkedDownload(url, contentLength)
                }
            } catch (ex: IOException) {
                handleDownloadFailure(ex)
            } finally {
                awaitAncillaryDownloads()
                DownloadCompletionHandler.finalizeDownload(datenDownload, start, state)
                messageBus.publishAsync(DownloadFinishedEvent())
                messageBus.unsubscribe(this@CdnAwareDirectDownloadThread)
            }
        }
    }

    @Throws(IOException::class)
    private fun parseDownloadUrl(): HttpUrl =
        datenDownload.downloadUrl.toHttpUrlOrNull()
            ?: throw IOException("Invalid download URL: ${datenDownload.downloadUrl}")

    private fun downloadLimit(): Long {
        val configuredLimit = ApplicationConfiguration.getConfiguration()
            .getLong(ApplicationConfiguration.DownloadRateLimiter.LIMIT, 0)
        return calculateDownloadLimit(configuredLimit)
    }

    private fun calculateDownloadLimit(limit: Long): Long {
        val active = ApplicationConfiguration.getConfiguration()
            .getBoolean(ApplicationConfiguration.DownloadRateLimiter.ACTIVE, false)
        return calcLimit(limit, active)
    }

    private fun calcLimit(limit: Long, active: Boolean): Long {
        if (limit <= 0 || !active) {
            return Long.MAX_VALUE
        }
        return limit * FileUtils.ONE_KB
    }

    private fun getContentLength(url: HttpUrl): Long {
        val request = Request.Builder()
            .url(url)
            .head()
            .header("User-Agent", userAgent())
            .header("Connection", "close")
            .build()

        client.newCall(request).execute().use { response ->
            if (!response.isSuccessful) {
                return -1
            }

            val contentSize = FileSize.getContentLength(response)
            return if (contentSize < 300_000) -1 else contentSize
        }
    }

    private fun userAgent(): String {
        return ApplicationConfiguration.getConfiguration()
            .getString(ApplicationConfiguration.APPLICATION_USER_AGENT)
    }

    private suspend fun prepareAncillaryDownloads() = coroutineScope {
        DownloadLifecycleActions.restartInterrupted(datenDownload)
        datenDownload.runtime.filmSize.aktSize = alreadyDownloaded
        ancillaryDownloads = DirectDownloadAncillaryFiles.start(this, datenDownload, logger)
    }

    private suspend fun executeChunkedDownload(url: HttpUrl, totalSize: Long) {
        var retryCount = 0

        while (!start.stoppen && alreadyDownloaded < totalSize) {
            val chunkStart = alreadyDownloaded
            val chunkEnd = minOf(chunkStart + DOWNLOAD_CHUNK_SIZE - 1, totalSize - 1)
            val request = buildChunkRequest(url, chunkStart, chunkEnd)

            try {
                client.newCall(request).execute().use { response ->
                    when (response.code) {
                        HTTP_PARTIAL_CONTENT -> {
                            val body = response.body
                            transferContent(body.byteStream())
                            retryCount = 0
                        }

                        HTTP_RANGE_NOT_SATISFIABLE if alreadyDownloaded >= totalSize -> {
                            break
                        }

                        HttpURLConnection.HTTP_NOT_FOUND -> {
                            logger.error("HTTP error 404 received for URL: {}", request.url)
                            state = HttpDownloadState.ERROR
                            start.markError()
                            return
                        }

                        else -> {
                            printHttpErrorMessage(response)
                            return
                        }
                    }
                }
            } catch (ex: IOException) {
                when {
                    isRetryableStreamException(ex) && alreadyDownloaded > chunkStart -> {
                        if (Config.isDebugModeEnabled())
                            logger.warn(
                                "Transient chunk error after partial progress, resuming at byte {}",
                                alreadyDownloaded,
                                ex
                            )
                        else {
                            logger.warn(
                                "Transient chunk error after partial progress, resuming at byte {}",
                                alreadyDownloaded
                            )
                        }
                        retryCount = 0
                        waitForRetry(0, ex)
                    }

                    isRetryableStreamException(ex) && retryCount < MAX_CHUNK_RETRIES -> {
                        retryCount++
                        waitForRetry(retryCount, ex)
                    }

                    else -> throw ex
                }
            }
        }

        if (alreadyDownloaded >= totalSize) {
            finishSuccessfulDownload()
        }
    }

    private fun buildChunkRequest(url: HttpUrl, rangeStart: Long, rangeEnd: Long): Request {
        return Request.Builder()
            .url(url)
            .get()
            .header("User-Agent", userAgent())
            .header("Connection", "close")
            .header("Range", "bytes=$rangeStart-$rangeEnd")
            .build()
    }

    private fun transferContent(inputStream: InputStream) {
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
                        var displayedBytes = datenDownload.runtime.filmSize.aktSize
                        var notifyProgress = false

                        while (!start.stoppen) {
                            val len = bandwidthInput.read(buffer)
                            if (len == -1) {
                                break
                            }

                            alreadyDownloaded += len
                            bufferedSink.write(buffer, 0, len)
                            datenDownload.runtime.filmSize.addAktSize(len.toLong())
                            val liveBandwidth = updateBandwidth(len.toLong())

                            if (displayedBytes != datenDownload.runtime.filmSize.aktSize) {
                                displayedBytes = datenDownload.runtime.filmSize.aktSize
                                notifyProgress = true
                            }

                            if (datenDownload.runtime.filmSize.size > 0) {
                                var progress = displayedBytes * 1000L / datenDownload.runtime.filmSize.size
                                if (startProgress == -1L) {
                                    startProgress = progress
                                }
                                progress = when {
                                    progress == 0L -> DownloadRunState.PROGRESS_GESTARTET.toLong()
                                    progress >= 1000L -> 999L
                                    else -> progress
                                }
                                start.updateProgress(progress.toInt())
                                if (progress != previousProgress) {
                                    previousProgress = progress
                                    if (progress > 2 && progress > startProgress) {
                                        val elapsed = Duration.between(start.startTime, LocalDateTime.now()).seconds
                                        val remainingProgress = 1000L - progress
                                        start.updateRemainingSeconds(elapsed * remainingProgress / (progress - startProgress))
                                    }
                                    notifyProgress = true
                                }
                            }

                            if (liveBandwidth != start.bandbreite) {
                                start.updateBandwidth(liveBandwidth)
                                notifyProgress = true
                            }

                            if (notifyProgress) {
                                DownloadProgressEventPublisher.publishThrottled()
                                notifyProgress = false
                            }
                        }

                        bufferedSink.flush()
                        start.updateBandwidth(overallAverageBandwidth())
                    }
                }
            }
        }
    }

    private fun updateBandwidth(bytesRead: Long): Long {
        totalBytesRead += bytesRead
        bandwidthWindowBytesRead += bytesRead
        refreshCurrentBandwidth(System.nanoTime())
        return currentBandwidth
    }

    private fun overallAverageBandwidth(): Long {
        if (totalBytesRead <= 0L) {
            return 0L
        }

        val elapsedNanos = System.nanoTime() - bandwidthStartedAtNanos
        if (elapsedNanos <= 0L) {
            return 0L
        }

        return totalBytesRead * NANOS_PER_SECOND / elapsedNanos
    }

    private fun refreshCurrentBandwidth(nowNanos: Long) {
        val elapsedNanos = nowNanos - bandwidthWindowStartNanos
        if (elapsedNanos < NANOS_PER_SECOND) {
            return
        }

        currentBandwidth = if (bandwidthWindowBytesRead > 0L) {
            bandwidthWindowBytesRead * NANOS_PER_SECOND / elapsedNanos
        } else {
            0L
        }

        bandwidthWindowBytesRead = 0L
        bandwidthWindowStartNanos = nowNanos
    }

    private fun finishSuccessfulDownload() {
        if (start.stoppen) {
            return
        }

        try {
            DirectDownloadPartFiles.moveCompletedPartToFinal(file, finalFile)
        } catch (ex: IOException) {
            logger.error("Failed to finalize download file", ex)
            start.markError()
            state = HttpDownloadState.ERROR
            removeSeenHistoryEntry()
            return
        }

        if (
            datenDownload.quelle == DownloadSource.BUTTON ||
            DownloadCompletionValidator.validateAndRecordSuccessfulAboDownload(Daten.getInstance(), datenDownload, start)
        ) {
            start.markFinished()
        } else {
            start.markError()
        }
    }

    private fun printHttpErrorMessage(response: Response) {
        val responseCode = "Responsecode: ${response.code}\n${response.message}"
        logger.error("HTTP-Fehler: {} {}", response.code, response.message)

        if (start.countRestarted >= Konstanten.MAX_DOWNLOAD_RESTARTS) {
            showDownloadError("URL des Films:\n${datenDownload.downloadUrl}\n\n$responseCode\n")
        }

        state = HttpDownloadState.ERROR
        start.markError()
    }

    private fun isRetryableStreamException(ex: IOException): Boolean {
        val lower = ex.message?.lowercase(Locale.ROOT) ?: return false
        return lower.contains("stream was reset") ||
                lower.contains("unexpected end of stream") ||
                lower.contains("connection reset") ||
                lower.contains("broken pipe") ||
                lower.contains("remote host terminated handshake")
    }

    private suspend fun waitForRetry(retryCount: Int, ex: IOException) {
        if (Config.isDebugModeEnabled()) {
            logger.warn(
                "Transient chunk error (retry {}/{}), resuming at byte {}",
                retryCount,
                MAX_CHUNK_RETRIES,
                alreadyDownloaded,
                ex
            )
        } else {
            logger.warn(
                "Transient chunk error (retry {}/{}), resuming at byte {}",
                retryCount,
                MAX_CHUNK_RETRIES,
                alreadyDownloaded
            )
        }
        delay(RETRY_DELAY_MILLIS.milliseconds)
    }

    private suspend fun awaitAncillaryDownloads() {
        ancillaryDownloads.await()
    }

    private fun handleDownloadFailure(ex: IOException) {
        logger.error("run()", ex)
        start.markError()
        state = HttpDownloadState.ERROR
        removeSeenHistoryEntry()

        showDownloadError(ex.localizedMessage)
    }

    private fun removeSeenHistoryEntry() {
        datenDownload.film?.let {
            logger.trace("Removing failed download entry from history")
            SeenHistoryController().use { historyController ->
                historyController.markUnseen(it)
            }
        }
    }

    private suspend fun cancelDownload(): Boolean {
        if (!file.exists() && !finalFile.exists()) {
            return false
        }

        if (Config.isDownloadAndQuit()) {
            return resolveExistingDownloadForCli()
        }

        dialogAbbrechenIsVis = true
        retAbbrechen = true
        if (SwingUtilities.isEventDispatchThread()) {
            retAbbrechen = abortOrResume()
            dialogAbbrechenIsVis = false
        } else {
            SwingUtilities.invokeLater {
                retAbbrechen = abortOrResume()
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
            Files.createDirectories(Paths.get(datenDownload.targetPath))
        } catch (_: IOException) {
        }
    }

    private fun abortOrResume(): Boolean {
        if (!file.exists() && !finalFile.exists()) {
            return false
        }

        val hasPartFile = file.exists()
        var result = false
        val dialog = DialogContinueDownload(MediathekGui.ui(), datenDownload, true)
        dialog.isVisible = true

        when (dialog.result) {
            DialogContinueDownload.DownloadResult.CANCELLED -> {
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
                if (dialog.isNewName) {
                    MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                    createDirectory()
                    finalFile = File(datenDownload.targetPathFileName)
                    file = DirectDownloadPartFiles.partFileFor(finalFile)
                }
            }
        }

        return result
    }

    private fun resolveExistingDownloadForCli(): Boolean {
        val hasPartFile = file.exists()
        logger.info(
            "CLI download mode: continuing existing direct download for {}",
            datenDownload.targetPathFileName
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
            logger.error("Download failed for {}: {}", datenDownload.targetPathFileName, message)
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
        private const val HTTP_PARTIAL_CONTENT = 206
        private const val RETRY_DELAY_MILLIS = 1_000L
        private const val MAX_CONCURRENT_REQUESTS = 2
        private const val MAX_CONCURRENT_REQUESTS_PER_HOST = 2
        private const val MAX_CHUNK_RETRIES = 25
        private const val DOWNLOAD_CHUNK_SIZE = 16L * 1024L * 1024L
        private const val DOWNLOAD_BUFFER_SIZE = 256 * 1024
        private const val DIALOG_POLL_DELAY_MILLIS = 100L
        private const val NANOS_PER_SECOND = 1_000_000_000L

        private val dispatcher: Dispatcher = Dispatcher().apply {
            maxRequests = MAX_CONCURRENT_REQUESTS
            maxRequestsPerHost = MAX_CONCURRENT_REQUESTS_PER_HOST
        }
    }
}
