package mediathek.controller.starter

import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.runBlocking
import mediathek.config.CommandLineOptions
import mediathek.config.Konstanten
import mediathek.controller.history.AboHistoryController
import mediathek.daten.DatenDownload
import mediathek.daten.DownloadSource
import mediathek.gui.dialog.DialogContinueDownload
import mediathek.gui.dialog.MeldungDownloadfehler
import mediathek.gui.messages.DownloadFinishedEvent
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.gui.messages.DownloadStartEvent
import mediathek.swing.SwingDispatch
import mediathek.tool.MessageBus
import mediathek.tool.notification.NotificationPublisher
import org.apache.logging.log4j.LogManager
import java.io.File
import java.nio.file.Files
import java.nio.file.LinkOption.NOFOLLOW_LINKS
import java.util.concurrent.TimeUnit.SECONDS
import javax.swing.JFrame

/**
 * Download files via an external program.
 */
class ExternalProgramDownload(
    private val aboHistoryControllerProvider: () -> AboHistoryController,
    private val datenDownload: DatenDownload,
    private val notificationPublisher: NotificationPublisher,
    private val dialogOwnerProvider: () -> JFrame? = { null },
) : Thread("EXTERNAL PROGRAM DL THREAD: ${datenDownload.title}") {

    private val start: DownloadRunState = checkNotNull(datenDownload.runtime.runState)
    private var file: File
    private var ffmpegOutput: FfmpegMp4Output? = null
    private var stagedAttempt: RuntimeExec? = null
    private var state = HttpDownloadState.DOWNLOAD
    private var ancillaryDownloads = DirectDownloadAncillaryFiles.empty(logger)

    init {
        start.markRunning()
        val targetFileName = datenDownload.targetPathFileName

        // JDK 25+ workaround
        val fileName = if (Runtime.version().feature() > 24 && targetFileName.isEmpty()) {
            EMPTY_FILENAME_JDK25_WORKAROUND
        } else {
            targetFileName
        }

        file = File(fileName)
        DownloadStartEventPublisher.publish(datenDownload)
    }

    override fun run() {
        MessageBus.messageBus.publishAsync(DownloadStartEvent())

        runBlocking {
            try {
                ffmpegOutput = FfmpegMp4Output.forDownload(datenDownload, start)
                createDownloadTargetDirectory(datenDownload.targetPath)

                if (!start.stoppen && !cancelDownload() && !start.stoppen) {
                    startAncillaryDownloads()
                    processDownload()
                }
            } catch (ex: CancellationException) {
                start.requestStop()
                throw ex
            } catch (ex: Exception) {
                if (!start.stoppen) {
                    logger.error("External-program download failed for {}", datenDownload.targetPathFileName, ex)
                    start.markError()
                    state = HttpDownloadState.ERROR
                    showDownloadError(ex.localizedMessage)
                }
            } finally {
                try {
                    DownloadCompletionHandler.finalizeDownload(
                        datenDownload, start, state, notificationPublisher,
                        handleTargetFile = ffmpegOutput?.published != false,
                    )
                } finally {
                    ffmpegOutput?.releaseAfter(start.process)
                }
                waitForPendingDownloads()
                MessageBus.messageBus.publish(DownloadFinishedEvent(datenDownload))
            }
        }
    }

    private fun CoroutineScope.startAncillaryDownloads() {
        ancillaryDownloads = DirectDownloadAncillaryFiles.start(this, datenDownload, logger)
    }

    private fun processDownload() {
        var filesize = -1L
        var stat = STAT_START

        while (stat < STAT_ENDE) {
            stat = when (stat) {
                STAT_START -> {
                    val started = !start.stoppen && starten()
                    when {
                        start.stoppen -> {
                            // Cancellation may arrive while ProcessBuilder.start() is returning.
                            // Clean up before bypassing the normal running-process polling path.
                            stopProcess()
                            STAT_FERTIG_OK
                        }
                        started -> if (datenDownload.isDownloadManager) STAT_FERTIG_OK else STAT_LAUFEN
                        else -> STAT_RESTART
                    }
                }

                STAT_LAUFEN -> laufendenDownloadPruefen()
                STAT_RESTART -> restartPruefen(filesize).also { nextStat ->
                    if (ffmpegOutput == null && nextStat == STAT_START && file.exists()) {
                        filesize = file.length()
                    }
                }

                STAT_PRUEFEN -> if (finishSuccessfulProcess()) STAT_FERTIG_OK else STAT_FERTIG_FEHLER

                STAT_FERTIG_FEHLER -> {
                    if (!start.stoppen) start.markError()
                    STAT_ENDE
                }

                STAT_FERTIG_OK -> {
                    if (!start.stoppen) start.markFinished()
                    STAT_ENDE
                }

                else -> STAT_ENDE
            }
        }
    }

    private fun finishSuccessfulProcess(): Boolean {
        val validateProgress = datenDownload.quelle != DownloadSource.BUTTON && !datenDownload.isDownloadManager
        val output = ffmpegOutput ?: return !validateProgress ||
            DownloadCompletionValidator.validateAndRecordSuccessfulAboDownload(
                aboHistoryControllerProvider(), datenDownload, start
            )

        if (start.stoppen) return true
        val part = output.partFile
        if (!part.isFile || !DownloadCompletionValidator.isSuccessfulDownload(part, start.takeIf { validateProgress })) {
            return false
        }
        if (start.stoppen) return true
        output.publish()
        if (validateProgress) {
            DownloadCompletionValidator.recordSuccessfulAboDownload(aboHistoryControllerProvider(), datenDownload)
        }
        return true
    }

    private fun laufendenDownloadPruefen(): Int {
        val process = checkNotNull(start.process)
        if (start.stoppen) {
            stopProcess()
            return STAT_FERTIG_OK
        }
        val exitCode = try {
            process.exitValue()
        } catch (_: IllegalThreadStateException) {
            try {
                sleep(PROCESS_POLL_DELAY_MILLIS)
            } catch (_: InterruptedException) {
                currentThread().interrupt()
            }
            return STAT_LAUFEN
        }
        stagedAttempt?.awaitProgress()
        if (start.stoppen) return STAT_FERTIG_OK
        if (exitCode != 0) return STAT_RESTART

        // FFmpeg can skip frames, so near-complete progress counts as complete after a successful exit.
        if (start.percent > 990) start.updateProgress(DownloadRunState.PROGRESS_FERTIG)
        return STAT_PRUEFEN
    }

    private fun restartPruefen(filesize: Long): Int {
        if (start.stoppen) return STAT_FERTIG_OK
        if (!datenDownload.isRestart) {
            return STAT_FERTIG_FEHLER
        }
        if (ffmpegOutput != null) {
            // Attempts create independent files, so growth across attempts is not a retry criterion.
            return if (start.startcounter < Konstanten.MAX_EXTERNAL_STARTS) STAT_START else STAT_FERTIG_FEHLER
        }

        if (filesize == -1L) {
            DownloadFileCleanup.deleteIfEmpty(file.toPath())
            return when {
                file.exists() -> STAT_START
                start.startcounter < Konstanten.MAX_EXTERNAL_STARTS -> STAT_START
                else -> STAT_FERTIG_FEHLER
            }
        }

        return when {
            !file.exists() -> STAT_FERTIG_FEHLER
            file.length() > filesize -> STAT_START
            else -> STAT_FERTIG_FEHLER
        }
    }

    private fun stopProcess() {
        val process = start.process ?: return
        process.destroy()
        if (ffmpegOutput != null && process.isAlive && !process.waitFor(FFMPEG_STOP_GRACE_SECONDS, SECONDS)) {
            // FFmpeg can defer SIGTERM while blocked reading a stream segment.
            process.destroyForcibly()
            process.waitFor(FFMPEG_STOP_GRACE_SECONDS, SECONDS)
        }
    }

    private suspend fun waitForPendingDownloads() {
        ancillaryDownloads.await()
    }

    private fun starten(): Boolean {
        // die Reihenfolge: startcounter - logStart ist wichtig!
        start.incrementStartCounter()
        DownloadLogMessages.logStart(datenDownload, start)
        val output = ffmpegOutput
        val preparedArguments = ArteHlsDownloadPreparer.prepare(datenDownload)
        if (start.stoppen) return false
        val arguments = if (output != null) {
            val stagedArguments = output.prepareAttempt(preparedArguments) ?: return false
            if (start.stoppen) return false
            start.updateProgress(DownloadRunState.PROGRESS_NICHT_GESTARTET)
            datenDownload.runtime.filmSize.aktSize = 0
            stagedArguments
        } else {
            preparedArguments
        }
        val runtimeExec = RuntimeExec(
            datenDownload.runtime.filmSize,
            start,
            datenDownload.programInvocation,
            datenDownload.programInvocationArray,
            arguments,
        )
        if (output != null) stagedAttempt = runtimeExec
        start.process = runtimeExec.exec(true)
        return start.process != null
    }

    private suspend fun cancelDownload(): Boolean {
        if (datenDownload.isDownloadManager) {
            // da kuemmert sich ein anderes Programm darum
            return false
        }
        if (!targetExists()) {
            // dann ist alles OK
            return false
        }

        if (CommandLineOptions.isDownloadAndQuit()) {
            return resolveExistingDownloadForCli()
        }

        return SwingDispatch.call(::abbrechen)
    }

    private fun abbrechen(): Boolean {
        var result = false
        if (targetExists()) {
            val dialogContinueDownload = DialogContinueDownload(dialogOwnerProvider(), datenDownload, false)
            dialogContinueDownload.isVisible = true

            when (dialogContinueDownload.result) {
                DialogContinueDownload.DownloadResult.CANCELLED -> {
                    // dann wars das
                    state = HttpDownloadState.CANCEL
                    result = true
                }

                DialogContinueDownload.DownloadResult.CONTINUE -> {
                    approveOverwrite()
                }

                DialogContinueDownload.DownloadResult.RESTART_WITH_NEW_NAME -> {
                    if (dialogContinueDownload.isNewName) {
                        // jetzt den Programmaufruf nochmal mit dem geaenderten Dateinamen nochmal bauen
                        datenDownload.rebuildInvocation()
                        MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                        createDownloadTargetDirectory(datenDownload.targetPath)
                        file = File(datenDownload.targetPathFileName)
                        ffmpegOutput = FfmpegMp4Output.forDownload(datenDownload, start)
                    }
                }
            }
        }
        return result
    }

    private fun resolveExistingDownloadForCli(): Boolean {
        logger.info(
            "CLI download mode: overwriting existing external-program target for {}",
            datenDownload.targetPathFileName
        )
        approveOverwrite()
        return false
    }

    private fun targetExists(): Boolean =
        if (ffmpegOutput == null) file.exists() else Files.exists(file.toPath(), NOFOLLOW_LINKS)

    private fun approveOverwrite() {
        if (start.stoppen) return
        val output = ffmpegOutput
        if (output != null) {
            output.approveOverwrite()
            return
        }
        try {
            Files.deleteIfExists(file.toPath())
            file = File(datenDownload.targetPathFileName)
        } catch (ex: Exception) {
            logger.error("File exists: {}", datenDownload.targetPathFileName, ex)
        }
    }

    private fun showDownloadError(message: String?) {
        if (CommandLineOptions.isDownloadAndQuit()) {
            logger.error("Download failed for {}: {}", datenDownload.targetPathFileName, message)
            return
        }
        SwingDispatch.dispatch {
            MeldungDownloadfehler(dialogOwnerProvider(), message, datenDownload).isVisible = true
        }
    }

    companion object {
        private const val STAT_START = 0
        private const val STAT_LAUFEN = 1
        private const val STAT_RESTART = 3
        private const val STAT_PRUEFEN = 4

        // ab hier ist schluss
        private const val STAT_FERTIG_OK = 10
        private const val STAT_FERTIG_FEHLER = 11
        private const val STAT_ENDE = 99

        private const val PROCESS_POLL_DELAY_MILLIS = 2_000L
        private const val FFMPEG_STOP_GRACE_SECONDS = 1L
        private const val EMPTY_FILENAME_JDK25_WORKAROUND =
            "ORACLE/DO/NOT/FUCK/AROUND/WITH/CORE/JAVA/CLASSES/WITHOUT/COMPATIBILITY/SWITCH"

        private val logger = LogManager.getLogger(ExternalProgramDownload::class.java)
    }
}
