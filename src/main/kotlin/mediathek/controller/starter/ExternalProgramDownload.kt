package mediathek.controller.starter

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import mediathek.config.Config
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.daten.DatenDownload
import mediathek.gui.dialog.DialogContinueDownload
import mediathek.gui.dialog.MeldungDownloadfehler
import mediathek.gui.messages.DownloadFinishedEvent
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.gui.messages.DownloadStartEvent
import mediathek.mainwindow.MediathekGui
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Paths
import javax.swing.SwingUtilities
import kotlin.time.Duration.Companion.milliseconds

/**
 * Download files via an external program.
 */
class ExternalProgramDownload(
    private val datenDownload: DatenDownload
) : Thread("EXTERNAL PROGRAM DL THREAD: ${datenDownload.arr[DatenDownload.DOWNLOAD_TITEL]}") {

    private val start: Start = datenDownload.start
    private var file: File
    private var retAbbrechen = false
    private var dialogAbbrechenIsVis = false
    private var state = HttpDownloadState.DOWNLOAD
    private var ancillaryDownloads = DirectDownloadAncillaryFiles.empty(logger)

    init {
        start.status = Start.STATUS_RUN
        var fileName = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]

        // JDK 25+ workaround
        if (Runtime.version().feature() > 24 && fileName.isEmpty()) {
            fileName = EMPTY_FILENAME_JDK25_WORKAROUND
        }

        file = File(fileName)
        StarterClass.notifyStartEvent(datenDownload)
        createDirectory()
    }

    override fun run() {
        MessageBus.messageBus.publishAsync(DownloadStartEvent())

        runBlocking {
            try {
                startAncillaryDownloads()

                if (!cancelDownload()) {
                    processDownload()
                }
            } catch (ex: Exception) {
                logger.error("run()", ex)
                showDownloadError(ex.localizedMessage)
            } finally {
                StarterClass.finalizeDownload(datenDownload, start, state)
                waitForPendingDownloads()
                MessageBus.messageBus.publish(DownloadFinishedEvent())
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
                    if (starten()) {
                        if (datenDownload.isDownloadManager) STAT_FERTIG_OK else STAT_LAUFEN
                    } else {
                        STAT_RESTART
                    }
                }

                STAT_LAUFEN -> laufendenDownloadPruefen()
                STAT_RESTART -> restartPruefen(filesize).also { nextStat ->
                    if (nextStat == STAT_START && file.exists()) {
                        filesize = file.length()
                    }
                }

                STAT_PRUEFEN -> {
                    if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON || datenDownload.isDownloadManager) {
                        STAT_FERTIG_OK
                    } else if (StarterClass.pruefen(Daten.getInstance(), datenDownload, start)) {
                        STAT_FERTIG_OK
                    } else {
                        STAT_FERTIG_FEHLER
                    }
                }

                STAT_FERTIG_FEHLER -> {
                    start.status = Start.STATUS_ERR
                    STAT_ENDE
                }

                STAT_FERTIG_OK -> {
                    start.status = Start.STATUS_FERTIG
                    STAT_ENDE
                }

                else -> STAT_ENDE
            }
        }
    }

    private fun laufendenDownloadPruefen(): Int {
        return try {
            if (start.stoppen) {
                start.process?.destroy()
                STAT_FERTIG_OK
            } else if (start.process.exitValue() != 0) {
                STAT_RESTART
            } else {
                /*
                 * In case of ffmpeg there may be frames skipped which prevents correct progress calculation,
                 * we therefore make percent max when the process terminated without error.
                 */
                if (start.percent > 990) {
                    start.percent = 1000
                }
                STAT_PRUEFEN
            }
        } catch (_: Exception) {
            try {
                sleep(PROCESS_POLL_DELAY_MILLIS)
            } catch (_: InterruptedException) {
                currentThread().interrupt()
            }
            STAT_LAUFEN
        }
    }

    private fun restartPruefen(filesize: Long): Int {
        if (!datenDownload.isRestart) {
            return STAT_FERTIG_FEHLER
        }

        if (filesize == -1L) {
            StarterClass.deleteIfEmpty(file.toPath())
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

    private suspend fun waitForPendingDownloads() {
        ancillaryDownloads.await()
    }

    private fun starten(): Boolean {
        // die Reihenfolge: startcounter - startmeldung ist wichtig!
        start.startcounter++
        StarterClass.startmeldung(datenDownload, start)
        val runtimeExec = RuntimeExec(
            datenDownload.mVFilmSize,
            datenDownload.start,
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF],
            datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]
        )
        start.process = runtimeExec.exec(true)
        return start.process != null
    }

    private suspend fun cancelDownload(): Boolean {
        if (datenDownload.isDownloadManager) {
            // da kuemmert sich ein anderes Programm darum
            return false
        }
        if (!file.exists()) {
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
            dialogAbbrechenIsVis = false
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

    private fun abbrechen(): Boolean {
        var result = false
        if (file.exists()) {
            val dialogContinueDownload = DialogContinueDownload(MediathekGui.ui(), datenDownload, false)
            dialogContinueDownload.isVisible = true

            when (dialogContinueDownload.result) {
                DialogContinueDownload.DownloadResult.CANCELLED -> {
                    // dann wars das
                    state = HttpDownloadState.CANCEL
                    result = true
                }

                DialogContinueDownload.DownloadResult.CONTINUE -> {
                    // dann mit gleichem Namen und Datei vorher loeschen
                    try {
                        Files.deleteIfExists(file.toPath())
                        file = File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
                    } catch (ex: Exception) {
                        // kann nicht geloescht werden, evtl. klappt ja das Ueberschreiben
                        logger.error("File exists: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME], ex)
                    }
                }

                DialogContinueDownload.DownloadResult.RESTART_WITH_NEW_NAME -> {
                    if (dialogContinueDownload.isNewName) {
                        // jetzt den Programmaufruf nochmal mit dem geaenderten Dateinamen nochmal bauen
                        datenDownload.aufrufBauen()
                        MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                        createDirectory(logFailure = false)
                        file = File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
                    }
                }
            }
        }
        return result
    }

    private fun resolveExistingDownloadForCli(): Boolean {
        logger.info(
            "CLI download mode: overwriting existing external-program target for {}",
            datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]
        )
        try {
            Files.deleteIfExists(file.toPath())
            file = File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
        } catch (ex: Exception) {
            logger.error("File exists: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME], ex)
        }
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

    private fun createDirectory(logFailure: Boolean = true) {
        try {
            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]))
        } catch (ex: IOException) {
            if (logFailure) {
                logger.error("Failed to create directories", ex)
            }
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
        private const val DIALOG_POLL_DELAY_MILLIS = 100L
        private const val EMPTY_FILENAME_JDK25_WORKAROUND =
            "ORACLE/DO/NOT/FUCK/AROUND/WITH/CORE/JAVA/CLASSES/WITHOUT/COMPATIBILITY/SWITCH"

        private val logger = LogManager.getLogger(ExternalProgramDownload::class.java)
    }
}
