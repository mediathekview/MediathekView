package mediathek.controller.starter

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.controller.history.MVUsedUrl
import mediathek.daten.*
import mediathek.gui.messages.ButtonStartEvent
import mediathek.gui.messages.StartEvent
import mediathek.mac.FinderCommentService
import mediathek.tool.*
import mediathek.tool.notification.MessageType
import mediathek.tool.notification.NotificationMessage
import mediathek.tool.notification.NotificationService
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import java.awt.GraphicsEnvironment
import java.awt.Taskbar
import java.awt.Toolkit
import java.io.File
import java.io.IOException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.time.Duration
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

private val logger = LogManager.getLogger(DownloadCompletionHandler::class.java)
private const val MIN_COMPLETED_DOWNLOAD_PERCENT = 995

internal object DownloadCompletionValidator {
    fun validateAndRecordSuccessfulAboDownload(daten: Daten, datenDownload: DatenDownload, start: DownloadRunState?): Boolean {
        if (!isSuccessfulDownload(datenDownload, start)) {
            return false
        }

        if (datenDownload.isFromAbo) {
            val usedUrl = MVUsedUrl(
                datenDownload.arr[DatenDownload.DOWNLOAD_THEMA],
                datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
                datenDownload.arr[DatenDownload.DOWNLOAD_HISTORY_URL],
            )
            daten.aboHistoryController.add(usedUrl)
        }

        return true
    }

    private fun isSuccessfulDownload(datenDownload: DatenDownload, start: DownloadRunState?): Boolean {
        // prüfen ob der Download geklappt hat und die Datei existiert und eine min. Größe hat
        val filePath = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]

        if (start != null && start.percent > -1 && start.percent < MIN_COMPLETED_DOWNLOAD_PERCENT) {
            // Prozent werden berechnet und es wurde vor 99,5% abgebrochen
            logger.error("Download fehlgeschlagen: 99,5% wurden nicht erreicht: {}", filePath)
            logger.error("Erreichte Prozente: {}%", start.percent / 10.0)
            return false
        }

        val file = File(filePath)
        return when {
            !file.exists() -> {
                logger.error("Download fehlgeschlagen, Datei existiert nicht: {}", filePath)
                false
            }

            file.length() < Konstanten.MIN_FILM_FILE_SIZE_KB -> {
                logger.error("Download fehlgeschlagen, Datei zu klein:{}", filePath)
                false
            }

            else -> true
        }
    }
}

internal object DownloadFileCleanup {
    /**
     * Delete the file if filesize is less that a constant value.
     *
     * @param path The file which is to be deleted.
     */
    fun deleteIfEmpty(path: Path) {
        try {
            if (Files.exists(path) && Files.size(path) < Konstanten.MIN_FILM_FILE_SIZE_KB) {
                // zum Wiederstarten/Aufräumen die leer/zu kleine Datei löschen, alles auf Anfang
                Files.delete(path)
            }
        } catch (_: IOException) {
            logger.trace("Fehler beim Löschen: {}", path.toAbsolutePath().toString())
        }
    }
}

internal object DownloadCompletionHandler {
    fun finalizeDownload(datenDownload: DatenDownload, start: DownloadRunState, state: HttpDownloadState) {
        DownloadFileCleanup.deleteIfEmpty(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]))
        setFileSize(datenDownload)

        if (SystemUtils.IS_OS_MAC_OSX) {
            writeSpotlightComment(datenDownload, state)
        }

        makeBeep()
        val completionMessage = DownloadLogMessages.logCompletion(datenDownload, start, state == HttpDownloadState.CANCEL)
        if (completionMessage.shouldNotify) {
            addNotification(datenDownload, completionMessage.successful)
        }

        if (state == HttpDownloadState.CANCEL) {
            datenDownload.resetDownload()
        } else {
            start.markCompletedProgress()
            datenDownload.mVFilmSize.aktSize = -1
        }
        DownloadStartEventPublisher.publish(datenDownload)

        if (SystemUtils.IS_OS_MAC_OSX && !GraphicsEnvironment.isHeadless() && Taskbar.isTaskbarSupported()) {
            Taskbar.getTaskbar().requestUserAttention(true, false)
        }
    }

    private fun makeBeep() {
        if (
            !GraphicsEnvironment.isHeadless() &&
            ApplicationConfiguration.getConfiguration()
                .getBoolean(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP, false)
        ) {
            Toolkit.getDefaultToolkit().beep()
        }
    }

    /**
     * Post a notification dialog whether download was successful or not.
     */
    private fun addNotification(datenDownload: DatenDownload, erfolgreich: Boolean) {
        val msg = NotificationMessage()
        val message: String

        if (erfolgreich) {
            msg.type = MessageType.INFO
            msg.title = "Download erfolgreich"
            message = String.format(
                "\"%s\" vom %s wurde geladen.",
                datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
                datenDownload.arr[DatenDownload.DOWNLOAD_SENDER],
            )
        } else {
            msg.type = MessageType.ERROR
            msg.title = "Download fehlerhaft"
            message = String.format(
                "Fehler beim Laden von \"%s\" des Senders %s aufgetreten.",
                datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
                datenDownload.arr[DatenDownload.DOWNLOAD_SENDER],
            )
        }
        msg.message = message

        NotificationService.displayNotification(msg)
    }

    private fun writeSpotlightComment(datenDownload: DatenDownload?, state: HttpDownloadState) {
        // we don´t write comments if download was cancelled...
        if (state != HttpDownloadState.CANCEL && datenDownload != null) {
            if (datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT].toBoolean()) {
                val filmPath = Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
                if (Files.exists(filmPath)) {
                    val strComment = datenDownload.film.description
                    if (!strComment.isNullOrEmpty()) {
                        FinderCommentService.writeFinderComment(filmPath, strComment, true)
                    }
                }
            }
        }
    }

    /**
     * tatsächliche Dateigröße eintragen
     *
     * @param datenDownload [DatenDownload] with the info of the file
     */
    private fun setFileSize(datenDownload: DatenDownload) {
        try {
            val testFile = File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
            if (testFile.exists()) {
                val length = testFile.length()
                if (length > 0) {
                    datenDownload.mVFilmSize.size = length
                }
            }
        } catch (_: Exception) {
            logger.error(
                "Fehler beim Ermitteln der Dateigröße: {}",
                datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME],
            )
        }
    }
}

internal object DownloadStartEventPublisher {
    fun publish(datenDownload: DatenDownload?) {
        val messageBus = MessageBus.messageBus

        messageBus.publishAsync(StartEvent())

        if (datenDownload?.quelle == DownloadSource.BUTTON) {
            messageBus.publishAsync(ButtonStartEvent())
        }
    }
}

internal object DownloadLogMessages {
    fun logStart(datenDownload: DatenDownload, start: DownloadRunState) {
        val text = mutableListOf<String>()
        val abspielen = datenDownload.quelle == DownloadSource.BUTTON
        if (abspielen) {
            text.add("Film abspielen")
        } else {
            if (start.startcounter > 1) {
                text.add("Download starten - Restart (Summe Starts: ${start.startcounter})")
            } else {
                text.add("Download starten")
            }
            text.addProgramSetAndTarget(datenDownload)
        }
        text.addUrl(datenDownload)
        text.add("Startzeit: ${formatTime(requireStartTime(start))}")
        text.addInvocation(datenDownload)
        logger.info(text)
    }

    fun logCompletion(datenDownload: DatenDownload, start: DownloadRunState, abgebrochen: Boolean): CompletionMessage {
        val text = mutableListOf<String>()
        if (abgebrochen) {
            text.add("Download wurde abgebrochen")
        } else if (datenDownload.quelle == DownloadSource.BUTTON) {
            text.add("Film fertig")
        } else {
            if (start.stoppen) {
                text.add("Download abgebrochen")
            } else if (start.isFinished) {
                // dann ists gut
                text.add("Download ist fertig und hat geklappt")
            } else if (start.isError) {
                text.add("Download ist fertig und war fehlerhaft")
            }
            if (datenDownload.isDownloadManager) {
                text.add("Programm ist ein Downloadmanager")
            }
            text.addProgramSetAndTarget(datenDownload)
        }

        val startTime = requireStartTime(start)
        val endZeit = LocalDateTime.now()
        text.add("Startzeit: ${formatTime(startTime)}")
        text.add("Endzeit: ${formatTime(endZeit)}")
        text.add("Restarts: ${start.countRestarted}")
        text.add("Dauer: ${Duration.between(startTime, endZeit).toSeconds()} s")

        if (datenDownload.art == DownloadType.DIRECT) {
            start.mVBandwidthCountingInputStream?.let { bandwidthInput ->
                text.add("Bytes gelesen: ${FileUtils.humanReadableByteCountBinary(bandwidthInput.sumByte)}")
                text.add("Bandbreite: ${BandwidthFormatter.format(bandwidthInput.sumBandwidth)}")
            }
        }
        text.addUrl(datenDownload)
        text.addInvocation(datenDownload)
        logger.info(text)

        return CompletionMessage(
            shouldNotify = !start.stoppen && !abgebrochen && datenDownload.quelle != DownloadSource.BUTTON,
            successful = !start.isError,
        )
    }

    private fun MutableList<String>.addProgramSetAndTarget(datenDownload: DatenDownload) {
        add("Programmset: ${datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET]}")
        add("Ziel: ${datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]}")
    }

    private fun MutableList<String>.addUrl(datenDownload: DatenDownload) {
        add("URL: ${datenDownload.arr[DatenDownload.DOWNLOAD_URL]}")
    }

    private fun MutableList<String>.addInvocation(datenDownload: DatenDownload) {
        if (datenDownload.art == DownloadType.DIRECT) {
            add(DownloadType.DIRECT.label)
        } else {
            add("Programmaufruf: ${datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]}")
            add("Programmaufruf[]: ${datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]}")
        }
    }

    private fun requireStartTime(start: DownloadRunState): LocalDateTime =
        checkNotNull(start.startTime) { "Download start time has not been initialized" }

    private fun formatTime(time: LocalDateTime): String = DateTimeFormatter.ISO_LOCAL_DATE_TIME.format(time)
}

internal data class CompletionMessage(
    val shouldNotify: Boolean,
    val successful: Boolean,
)
