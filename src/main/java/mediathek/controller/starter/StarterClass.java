package mediathek.controller.starter;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.gui.messages.ButtonStartEvent;
import mediathek.gui.messages.DownloadProgressChangedEvent;
import mediathek.gui.messages.StartEvent;
import mediathek.mac.SpotlightCommentWriter;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.Datum;
import mediathek.tool.notification.thrift.MessageType;
import mediathek.tool.notification.thrift.NotificationMessage;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;
import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.concurrent.TimeUnit;

public class StarterClass {
    private static final Logger logger = LogManager.getLogger(StarterClass.class);
    private final Daten daten;
    private final Starten starten;
    private boolean pause = false;

    public StarterClass(Daten daten) {
        this.daten = daten;
        starten = new Starten();
        starten.start();
    }

    static boolean pruefen(Daten daten, DatenDownload datenDownload, Start start) {
        //prüfen ob der Download geklappt hat und die Datei existiert und eine min. Größe hat
        boolean ret = false;
        if (start != null) {
            if (start.percent > -1 && start.percent < 995) {
                // Prozent werden berechnet und es wurde vor 99,5% abgebrochen
                logger.error("Download fehlgeschlagen: 99,5% wurden nicht erreicht: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);

                return false;
            }
        }
        File file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        if (!file.exists()) {
            logger.error("Download fehlgeschlagen, Datei existiert nicht: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        } else if (file.length() < Konstanten.MIN_FILM_FILE_SIZE_KB) {
            logger.error("Download fehlgeschlagen, Datei zu klein:{}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        } else {
            if (datenDownload.isFromAbo()) {
                daten.getAboHistoryController().zeileSchreiben(datenDownload.arr[DatenDownload.DOWNLOAD_THEMA],
                        datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
                        datenDownload.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
            }
            ret = true;
        }
        return ret;
    }

    /**
     * Delete the file if filesize is less that a constant value.
     *
     * @param path The file which is to be deleted.
     */
    static void deleteIfEmpty(Path path) {
        try {
            if (Files.exists(path)) {
                // zum Wiederstarten/Aufräumen die leer/zu kleine Datei löschen, alles auf Anfang
                if (Files.size(path) < Konstanten.MIN_FILM_FILE_SIZE_KB)
                    Files.delete(path);
            }
        } catch (IOException ex) {
            logger.error("Fehler beim Löschen: {}", path.toAbsolutePath().toString());
        }
    }

    private static final FastDateFormat formatter = FastDateFormat.getInstance("HH:mm:ss");

    static void startmeldung(DatenDownload datenDownload, Start start) {
        ArrayList<String> text = new ArrayList<>();
        boolean abspielen = datenDownload.quelle == DatenDownload.QUELLE_BUTTON;
        if (abspielen) {
            text.add("Film abspielen");
        } else {
            if (start.startcounter > 1) {
                text.add("Download starten - Restart (Summe Starts: " + start.startcounter + ')');
            } else {
                text.add("Download starten");
            }
            text.add("Programmset: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET]);
            text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        text.add("Startzeit: " + formatter.format(start.startZeit));
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
            text.add(DatenDownload.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            text.add("Programmaufruf[]: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        }
        logger.info(text);
    }

    private static void makeBeep() {
        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DOWNLOAD_SOUND_BEEP, false)) {
            Toolkit.getDefaultToolkit().beep();
        }
    }

    private static void fertigmeldung(final DatenDownload datenDownload, final Start start, boolean abgebrochen) {
        makeBeep();

        ArrayList<String> text = new ArrayList<>();
        if (abgebrochen) {
            text.add("Download wurde abgebrochen");
        } else if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON) {
            text.add("Film fertig");
        } else {
            if (start.stoppen) {
                text.add("Download abgebrochen");
            } else if (start.status == Start.STATUS_FERTIG) {
                // dann ists gut
                text.add("Download ist fertig und hat geklappt");
            } else if (start.status == Start.STATUS_ERR) {
                text.add("Download ist fertig und war fehlerhaft");
            }
            if (datenDownload.isDownloadManager()) {
                text.add("Programm ist ein Downloadmanager");
            }
            text.add("Programmset: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET]);
            text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }

        text.add("Startzeit: " + formatter.format(start.startZeit));
        text.add("Endzeit: " + formatter.format(new Date().getTime()));
        text.add("Restarts: " + start.countRestarted);
        text.add("Dauer: " + start.startZeit.diffInSekunden() + " s");
        long dauer = start.startZeit.diffInMinuten();
        if (dauer == 0) {
            text.add("Dauer: <1 Min.");
        } else {
            text.add("Dauer: " + start.startZeit.diffInMinuten() + " Min");
        }
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
            if (start.mVBandwidthCountingInputStream != null) {
                text.add("Bytes gelesen: " + FileUtils.byteCountToDisplaySize(start.mVBandwidthCountingInputStream.getSumByte()));
                text.add("Bandbreite: " + DatenDownload.getTextBandbreite(start.mVBandwidthCountingInputStream.getSumBandwidth()));
            }
        }
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
            text.add(DatenDownload.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            text.add("Programmaufruf[]: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        }
        logger.info(text);
        if (!start.stoppen && !abgebrochen) {
            if (datenDownload.quelle != DatenDownload.QUELLE_BUTTON) {
                addNotification(datenDownload, start.status != Start.STATUS_ERR);
            }
        }
    }

    /**
     * Post a notification dialog whether download was successful or not.
     */
    private static void addNotification(DatenDownload datenDownload, boolean erfolgreich) {
        final String[] m = {
                "Film:   " + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
                "Sender: " + datenDownload.arr[DatenDownload.DOWNLOAD_SENDER],
                "Größe:  " + FileUtils.byteCountToDisplaySize(datenDownload.mVFilmSize.getSize())
        };

        StringBuilder meldung = new StringBuilder();
        for (String s : m) {
            meldung.append(s).append('\n');
        }

        final NotificationMessage msg = new NotificationMessage();
        msg.setMessage(meldung.toString());
        if (erfolgreich) {
            msg.setType(MessageType.INFO);
            msg.setTitle("Download war erfolgreich");
        }
        else {
            msg.setType(MessageType.ERROR);
            msg.setTitle("Download war fehlerhaft");
        }

        Daten.getInstance().notificationCenter().displayNotification(msg);
    }

    private static void writeSpotlightComment(DatenDownload datenDownload, DirectHttpDownload.HttpDownloadState state) {
        //we don´t write comments if download was cancelled...
        if (state != DirectHttpDownload.HttpDownloadState.CANCEL) {
            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT])) {
                final SpotlightCommentWriter writer = new SpotlightCommentWriter();
                writer.writeComment(datenDownload);
            }
        }
    }

    public static void finalizeDownload(DatenDownload datenDownload, Start start, DirectHttpDownload.HttpDownloadState state) {
        deleteIfEmpty(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]));
        setFileSize(datenDownload);

        if (SystemUtils.IS_OS_MAC_OSX) {
            writeSpotlightComment(datenDownload, state);
        }


        fertigmeldung(datenDownload, start, state == DirectHttpDownload.HttpDownloadState.CANCEL);

        if (state == DirectHttpDownload.HttpDownloadState.CANCEL) {
            datenDownload.resetDownload();
        } else {
            start.restSekunden = -1;
            start.percent = Start.PROGRESS_FERTIG;
            datenDownload.mVFilmSize.setAktSize(-1);
        }
        notifyStartEvent(datenDownload);

        if (SystemUtils.IS_OS_MAC_OSX) {
            Taskbar.getTaskbar().requestUserAttention(true,false);
        }
    }

    /**
     * tatsächliche Dateigröße eintragen
     *
     * @param datenDownload {@link DatenDownload} with the info of the file
     */
    private static void setFileSize(DatenDownload datenDownload) {
        try {
            final File testFile = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
            if (testFile.exists()) {
                final long length = testFile.length();
                if (length > 0) {
                    datenDownload.mVFilmSize.setSize(length);
                }
            }
        } catch (Exception ex) {
            logger.error("Fehler beim Ermitteln der Dateigröße: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

    static void notifyStartEvent(DatenDownload datenDownload) {
        final var messageBus = Daten.getInstance().getMessageBus();

        messageBus.publishAsync(new StartEvent());

        if (datenDownload != null) {
            if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON)
                messageBus.publishAsync(new ButtonStartEvent());
        }
    }

    public synchronized void urlMitProgrammStarten(DatenPset pSet, DatenFilm ersterFilm, String aufloesung) {
        // url mit dem Programm mit der Nr. starten (Button oder TabDownload "rechte Maustaste")
        // Quelle "Button" ist immer ein vom User gestarteter Film, also Quelle_Button!!!!!!!!!!!
        String url = ersterFilm.getUrl();
        if (!url.isEmpty()) {
            DatenDownload d = new DatenDownload(pSet, ersterFilm, DatenDownload.QUELLE_BUTTON, null, "", "", aufloesung);
            d.start = new Start();
            starten.launchDownloadThread(d);
            // gestartete Filme (originalURL des Films) auch in die History eintragen
            try (var historyController = new SeenHistoryController()){
                historyController.writeManualEntry(ersterFilm.getThema(), ersterFilm.getTitle(), d.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
            }

            // falls gemerkt, Film in Merkliste als abgespielt kennzeichnen
            if (ersterFilm.isBookmarked()) {
              ersterFilm.getBookmark().setSeen(true);
            }
            // und jetzt noch in die Downloadliste damit die Farbe im Tab Filme passt
            daten.getListeDownloadsButton().addMitNummer(d);
        }
    }

    public void pause() {
        pause = true;
    }

    private void reStartmeldung(DatenDownload datenDownload) {
        ArrayList<String> text = new ArrayList<>();
        text.add("Fehlerhaften Download neu starten - Restart (Summe Starts: " + datenDownload.start.countRestarted + ')');
        text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        logger.info(text);
    }

    // ********************************************
    // Hier wird dann gestartet
    // Ewige Schleife die die Downloads startet
    // ********************************************
    private class Starten extends Thread {

        public Starten() {
            super();
            setName("StarterClass.Starten Thread");
            setDaemon(true);
        }

        @Override
        public void run() {
            while (!isInterrupted()) {
                try {
                    DatenDownload datenDownload;
                    while ((datenDownload = getNextStart()) != null) {
                        launchDownloadThread(datenDownload);
                        //alle 3 Sekunden einen Download starten
                        TimeUnit.SECONDS.sleep(3);
                    }
                    daten.getListeDownloadsButton().buttonStartsPutzen(); // Button Starts aus der Liste löschen
                    TimeUnit.SECONDS.sleep(3);
                } catch (Exception ex) {
                    logger.error("Fehler in Starten Thread:", ex);
                }
            }
        }

        private synchronized DatenDownload getNextStart() throws InterruptedException {
            // get: erstes passendes Element der Liste zurückgeben oder null
            // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
            if (pause) {
                // beim Löschen der Downloads, kann das Starten etwas "pausiert" werden
                // damit ein zu Löschender Download nicht noch schnell gestartet wird
                TimeUnit.SECONDS.sleep(5);
                pause = false;
            }

            DatenDownload download = daten.getListeDownloads().getNextStart();
            if (download == null) {
                // dann versuchen einen Fehlerhaften nochmal zu starten
                download = daten.getListeDownloads().getRestartDownload();
                if (download != null) {
                    reStartmeldung(download);
                }
            }
            return download;
        }

        /**
         * This will start the download process.
         *
         * @param datenDownload The {@link mediathek.daten.DatenDownload} info object for download.
         */
        private void launchDownloadThread(DatenDownload datenDownload) {
            datenDownload.start.startZeit = new Datum();
            daten.getMessageBus().publishAsync(new DownloadProgressChangedEvent());

            Thread downloadThread;

            switch (datenDownload.art) {
                case DatenDownload.ART_PROGRAMM -> {
                    downloadThread = new ExternalProgramDownload(daten, datenDownload);
                    downloadThread.start();
                }
                case DatenDownload.ART_DOWNLOAD -> {
                    downloadThread = new DirectHttpDownload(daten, datenDownload);
                    downloadThread.start();
                }
                default -> logger.error("StarterClass.Starten - Switch-default");
            }
        }
    }
}
