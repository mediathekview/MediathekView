package mediathek.controller.starter;

import mediathek.config.Daten;
import mediathek.controller.starter.DirectHttpDownload.HttpDownloadState;
import mediathek.daten.DatenDownload;
import mediathek.gui.dialog.DialogContinueDownload;
import mediathek.gui.dialog.MeldungDownloadfehler;
import mediathek.gui.messages.DownloadFinishedEvent;
import mediathek.gui.messages.DownloadListChangedEvent;
import mediathek.gui.messages.DownloadStartEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MVInfoFile;
import mediathek.tool.MVSubtitle;
import mediathek.tool.MessageBus;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;

/**
 * Download files via an external program.
 */
public class ExternalProgramDownload extends Thread {

    private static final Logger logger = LogManager.getLogger();
    private static final int STAT_START = 0;
    private static final int STAT_LAUFEN = 1;
    private static final int STAT_RESTART = 3;
    private static final int STAT_PRUEFEN = 4;
    // ab hier ist schluss
    private static final int STAT_FERTIG_OK = 10;
    private static final int STAT_FERTIG_FEHLER = 11;
    private static final int STAT_ENDE = 99;
    private final DatenDownload datenDownload;
    private final Start start;
    private File file;
    private boolean retAbbrechen;
    private boolean dialogAbbrechenIsVis;
    private HttpDownloadState state = HttpDownloadState.DOWNLOAD;
    private CompletableFuture<Void> infoFuture;
    private CompletableFuture<Void> subtitleFuture;

    public ExternalProgramDownload(DatenDownload d) {
        setName("EXTERNAL PROGRAM DL THREAD: " + d.arr[DatenDownload.DOWNLOAD_TITEL]);

        datenDownload = d;
        start = datenDownload.start;
        start.status = Start.STATUS_RUN;
        file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        StarterClass.notifyStartEvent(datenDownload);

        try {
            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
        } catch (IOException e) {
            logger.error("Failed to create directories", e);
        }
    }

    @Override
    public synchronized void run() {
        long filesize = -1;
        int stat = STAT_START;

        MessageBus.getMessageBus().publishAsync(new DownloadStartEvent());

        startInfoFileDownload();

        startSubtitleFileDownload();

        try {
            if (!cancelDownload()) {
                while (stat < STAT_ENDE) {
                    switch (stat) {
                        case STAT_START:
                            // versuch das Programm zu Starten
                            if (starten()) {
                                if (datenDownload.isDownloadManager()) {
                                    stat = STAT_FERTIG_OK;
                                } else {
                                    stat = STAT_LAUFEN;
                                }
                            } else {
                                stat = STAT_RESTART;
                            }
                            break;
                        case STAT_LAUFEN:
                            //hier läuft der Download bis zum Abbruch oder Ende
                            try {
                                if (start.stoppen) {
                                    stat = STAT_FERTIG_OK;
                                    if (start.process != null) {
                                        start.process.destroy();
                                    }
                                } else {
                                    if (start.process.exitValue() != 0) {
                                        stat = STAT_RESTART;
                                    } else {
                                        /*
                                        in case of ffmpeg there may be frames skipped which prevents correct progress calculation,
                                        we therefore make percent max when the process terminated without error.
                                         */
                                        if (start.percent > 990)
                                            start.percent = 1000;
                                        stat = STAT_PRUEFEN;
                                    }
                                }
                            } catch (Exception ex) {
                                try {
                                    this.wait(2000);
                                } catch (InterruptedException ignored) {
                                }
                            }
                            break;
                        case STAT_RESTART:
                            if (!datenDownload.isRestart()) {
                                // dann wars das
                                stat = STAT_FERTIG_FEHLER;
                            } else if (filesize == -1) {
                                //noch nichts geladen
                                StarterClass.deleteIfEmpty(file.toPath());
                                if (file.exists()) {
                                    // dann bestehende Datei weitermachen
                                    filesize = file.length();
                                    stat = STAT_START;
                                } else // counter prüfen und bei einem Maxwert cancelDownload, sonst endlos
                                    if (start.startcounter < Start.STARTCOUNTER_MAX) {
                                        // dann nochmal von vorne
                                        stat = STAT_START;
                                    } else {
                                        // dann wars das
                                        stat = STAT_FERTIG_FEHLER;
                                    }
                            } else //jetzt muss das File wachsen, sonst kein Restart
                                if (!file.exists()) {
                                    // dann wars das
                                    stat = STAT_FERTIG_FEHLER;
                                } else if (file.length() > filesize) {
                                    //nur weitermachen wenn die Datei tasächlich wächst
                                    filesize = file.length();
                                    stat = STAT_START;
                                } else {
                                    // dann wars das
                                    stat = STAT_FERTIG_FEHLER;
                                }
                            break;
                        case STAT_PRUEFEN:
                            if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON || datenDownload.isDownloadManager()) {
                                //für die direkten Starts mit dem Button und die remote downloads wars das dann
                                stat = STAT_FERTIG_OK;
                            } else if (StarterClass.pruefen(Daten.getInstance(), datenDownload, start)) {
                                //fertig und OK
                                stat = STAT_FERTIG_OK;
                            } else {
                                //fertig und fehlerhaft
                                stat = STAT_FERTIG_FEHLER;
                            }
                            break;
                        case STAT_FERTIG_FEHLER:
                            start.status = Start.STATUS_ERR;
                            stat = STAT_ENDE;
                            break;
                        case STAT_FERTIG_OK:
                            start.status = Start.STATUS_FERTIG;
                            stat = STAT_ENDE;
                            break;
                    }
                }
            }
        } catch (Exception ex) {
            logger.error("run()", ex);
            SwingUtilities.invokeLater(() ->
                    new MeldungDownloadfehler(MediathekGui.ui(), ex.getLocalizedMessage(), datenDownload).setVisible(true));
        }

        StarterClass.finalizeDownload(datenDownload, start, state);

        waitForPendingDownloads();

        MessageBus.getMessageBus().publish(new DownloadFinishedEvent());
    }

    private void startInfoFileDownload() {
        final boolean downloadInfoFile = Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI]);
        if (downloadInfoFile) {
            logger.trace("Starting info file download");

            infoFuture = CompletableFuture.runAsync(() -> {
                try {
                    MVInfoFile infoFile = new MVInfoFile();
                    infoFile.writeInfoFile(datenDownload);
                } catch (IOException ex) {
                    logger.error("Failed to write info file", ex);
                }
            });
        }
    }

    private void startSubtitleFileDownload() {
        if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE])) {
            logger.trace("Starting subtitle file download");
            subtitleFuture = CompletableFuture.runAsync(() -> {
                MVSubtitle subtitleFile = new MVSubtitle();
                subtitleFile.writeSubtitle(datenDownload);
            });
        }
    }

    private void waitForPendingDownloads() {
        try {
            if (infoFuture != null)
                infoFuture.get();

            if (subtitleFuture != null)
                subtitleFuture.get();

        } catch (InterruptedException | ExecutionException e) {
            logger.error("waitForPendingDownloads().", e);
        }
    }

    private boolean starten() {
        boolean ret = false;
        // die Reihenfolge: startcounter - startmeldung ist wichtig!
        start.startcounter++;
        StarterClass.startmeldung(datenDownload, start);
        RuntimeExec runtimeExec = new RuntimeExec(datenDownload.mVFilmSize, datenDownload.start,
                datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF], datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        start.process = runtimeExec.exec(true);
        if (start.process != null) {
            ret = true;
        }
        return ret;
    }

    private boolean cancelDownload() {
        if (datenDownload.isDownloadManager()) {
            // da kümmert sich ein anderes Programm darum
            return false;
        }
        if (!file.exists()) {
            // dann ist alles OK
            return false;
        }

        dialogAbbrechenIsVis = true;
        retAbbrechen = true;
        if (SwingUtilities.isEventDispatchThread()) {
            retAbbrechen = abbrechen_();
        } else {
            SwingUtilities.invokeLater(() ->
            {
                retAbbrechen = abbrechen_();
                dialogAbbrechenIsVis = false;
            });
        }
        while (dialogAbbrechenIsVis) {
            try {
                wait(100);
            } catch (Exception ignored) {
            }
        }
        return retAbbrechen;
    }

    private boolean abbrechen_() {
        boolean result = false;
        if (file.exists()) {
            DialogContinueDownload dialogContinueDownload = new DialogContinueDownload(MediathekGui.ui(), datenDownload, false /*weiterführen*/);
            dialogContinueDownload.setVisible(true);

            switch (dialogContinueDownload.getResult()) {
                case CANCELLED:
                    // dann wars das
                    state = DirectHttpDownload.HttpDownloadState.CANCEL;
                    result = true;
                    break;

                case CONTINUE:
                    // dann mit gleichem Namen und Datei vorher löschen
                    try {
                        Files.deleteIfExists(file.toPath());
                        file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                    } catch (Exception ex) {
                        // kann nicht gelöscht werden, evtl. klappt ja das Überschreiben
                        logger.error("File exists: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME], ex);
                    }
                    break;

                case RESTART_WITH_NEW_NAME:
                    if (dialogContinueDownload.isNewName()) {
                        // jetzt den Programmaufruf nochmal mit dem geänderten Dateinamen nochmal bauen
                        datenDownload.aufrufBauen();
                        MessageBus.getMessageBus().publishAsync(new DownloadListChangedEvent());
                        try {
                            Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
                        } catch (IOException ignored) {
                        }
                        file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                    }
                    break;
            }
        }
        return result;
    }
}
