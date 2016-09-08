/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.starter;

import com.apple.eawt.Application;
import com.jidesoft.utils.SystemInfo;
import java.awt.Toolkit;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.swing.SwingUtilities;
import mSearch.daten.DatenFilm;
import mSearch.tool.Datum;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.MVBandwidthTokenBucket;
import mediathek.controller.MVInputStream;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.gui.dialog.DialogContinueDownload;
import mediathek.gui.dialog.MeldungDownloadfehler;
import mediathek.tool.MVInfoFile;
import mediathek.tool.MVNotification;
import mediathek.tool.MVSubtitle;

public class StarterClass {
    //Tags Filme

    private final Daten daten;
    private Starten starten = null;
    private boolean pause = false;

    //===================================
    // Public
    //===================================
    public StarterClass(Daten daten) {
        this.daten = daten;
        starten = new Starten();
        starten.start();
    }

    public synchronized void urlMitProgrammStarten(DatenPset pSet, DatenFilm ersterFilm, String aufloesung) {
        // url mit dem Programm mit der Nr. starten (Button oder TabDownload "rechte Maustaste")
        // Quelle "Button" ist immer ein vom User gestarteter Film, also Quelle_Button!!!!!!!!!!!
        String url = ersterFilm.arr[DatenFilm.FILM_URL];
        if (!url.equals("")) {
            DatenDownload d = new DatenDownload(pSet, ersterFilm, DatenDownload.QUELLE_BUTTON, null, "", "", aufloesung);
            d.start = new Start();
            starten.startStarten(d);
            // gestartete Filme (originalURL des Films) auch in die History eintragen
            daten.history.zeileSchreiben(ersterFilm.arr[DatenFilm.FILM_THEMA], ersterFilm.arr[DatenFilm.FILM_TITEL], d.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
            Daten.listeFilmeHistory.add(ersterFilm);
            // und jetzt noch in die Downloadliste damit die Farbe im Tab Filme passt
            Daten.listeDownloadsButton.addMitNummer(d);
        }
    }

    public void pause() {
        pause = true;
    }

    private boolean pruefen(DatenDownload datenDownload, Start start) {
        //prüfen ob der Download geklappt hat und die Datei existiert und eine min. Größe hat
        boolean ret = false;
        if (start != null) {
            if (start.percent > -1 && start.percent < 995) {
                // Prozent werden berechnet und es wurde vor 99,5% abgebrochen
                Log.errorLog(696510258, "Download fehlgeschlagen: 99,5% wurden nicht erreicht"
                        + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                return false;
            }
        }
        File file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        if (!file.exists()) {
            Log.errorLog(550236231, "Download fehlgeschlagen: Datei existiert nicht" + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        } else if (file.length() < Konstanten.MIN_DATEI_GROESSE_FILM) {
            Log.errorLog(795632500, "Download fehlgeschlagen: Datei zu klein" + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        } else {
            if (datenDownload.istAbo()) {
                daten.erledigteAbos.zeileSchreiben(datenDownload.arr[DatenDownload.DOWNLOAD_THEMA],
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
     * @param file The file which is to be deleted.
     */
    private void deleteIfEmpty(File file) {
        try {
            if (file.exists()) {
                // zum Wiederstarten/Aufräumen die leer/zu kleine Datei löschen, alles auf Anfang
                if (file.length() == 0) {
                    // zum Wiederstarten/Aufräumen die leer/zu kleine Datei löschen, alles auf Anfang
                    SysMsg.sysMsg(new String[]{"Restart/Aufräumen: leere Datei löschen", file.getAbsolutePath()});
                    if (!file.delete()) {
                        throw new Exception();
                    }
                } else if (file.length() < Konstanten.MIN_DATEI_GROESSE_FILM) {
                    SysMsg.sysMsg(new String[]{"Restart/Aufräumen: Zu kleine Datei löschen", file.getAbsolutePath()});
                    if (!file.delete()) {
                        throw new Exception();
                    }
                }
            }
        } catch (Exception ex) {
            Log.errorLog(795632500, "Fehler beim löschen" + file.getAbsolutePath());
        }
    }

    private void startmeldung(DatenDownload datenDownload, Start start) {
        ArrayList<String> text = new ArrayList<>();
        boolean abspielen = datenDownload.quelle == DatenDownload.QUELLE_BUTTON;
        if (abspielen) {
            text.add("Film abspielen");
        } else {
            if (start.startcounter > 1) {
                text.add("Download starten - Restart (Summe Starts: " + start.startcounter + ")");
            } else {
                text.add("Download starten");
            }
            text.add("Programmset: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET]);
            text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        text.add("Startzeit: " + new SimpleDateFormat("HH:mm:ss").format(start.startZeit));
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
            text.add(DatenDownload.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            text.add("Programmaufruf[]: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        }
        SysMsg.sysMsg(text.toArray(new String[text.size()]));
    }

    private void reStartmeldung(DatenDownload datenDownload, Start start) {
        ArrayList<String> text = new ArrayList<>();
        text.add("Fehlerhaften Download neu starten - Restart (Summe Starts: " + start.countRestarted + ")");
        text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        SysMsg.sysMsg(text.toArray(new String[text.size()]));
    }

    private void fertigmeldung(final DatenDownload datenDownload, final Start start, boolean abgebrochen) {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_DOWNLOAD_BEEP))) {
            try {
                Toolkit.getDefaultToolkit().beep();
            } catch (Exception ignored) {
            }
        }
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
        text.add("Startzeit: " + new SimpleDateFormat("HH:mm:ss").format(start.startZeit));
        text.add("Endzeit: " + new SimpleDateFormat("HH:mm:ss").format(new Datum().getTime()));
        text.add("Dauer: " + start.startZeit.diffInSekunden() + " s");
        long dauer = start.startZeit.diffInMinuten();
        if (dauer == 0) {
            text.add("Dauer: <1 Min.");
        } else {
            text.add("Dauer: " + start.startZeit.diffInMinuten() + " Min");
        }
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
            text.add(DatenDownload.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            text.add("Programmaufruf[]: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        }
        SysMsg.sysMsg(text.toArray(new String[text.size()]));
        if (!start.stoppen && !abgebrochen) {
            if (datenDownload.quelle != DatenDownload.QUELLE_BUTTON) {
                SwingUtilities.invokeLater(() -> MVNotification.addNotification(daten, datenDownload, start.status != Start.STATUS_ERR));
            }
        }
    }

    /**
     * This will write the content of the film description into the OS X Finder Info Comment Field.
     * This enables Spotlight to search for these tags.
     * THIS IS AN OS X specific functionality!
     *
     * @param datenDownload The download information object
     * @param wasCancelled Was the download cancelled?
     */
    private void writeSpotlightComment(final DatenDownload datenDownload, final boolean wasCancelled) {
        //no need to run when not OS X...
        if (wasCancelled || (!SystemInfo.isMacOSX())) {
            return;
        }
        if (datenDownload.film == null) {
            // kann bei EinmalDownloads nach einem Neuladen der Filmliste/Programmneustart der Fall sein
            return;
        }
        final Path filmPath = Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        if (Files.exists(filmPath)) {
            final String strFilePath = filmPath.toString();
            String strComment = datenDownload.film.arr[DatenFilm.FILM_BESCHREIBUNG];
            if (strComment != null) {
                //no need to write spotlight data when there is no description...
                if (strComment.isEmpty()) {
                    return;
                }

                //replace quotation marks...
                strComment = strComment.replace("\"", "\\\"");

                final String script = "tell application \"Finder\"\n"
                        + "set my_file to POSIX file \"" + strFilePath + "\" as alias\n"
                        + "set comment of my_file to \"" + strComment + "\"\n"
                        + "end tell\n";
                try {
                    ScriptEngineManager mgr = new ScriptEngineManager();
                    ScriptEngine engine = mgr.getEngineByName("AppleScript");
                    engine.eval(script);
                } catch (Exception ex) {
                    Log.errorLog(915263987, "Fehler beim Spotlight schreiben" + filmPath.toString());
                    //AppleScript may not be available if user does not use the official MacApp.
                    //We need to log that as well if there are error reports.
                    try {
                        if (!System.getProperty("OSX_OFFICIAL_APP").equalsIgnoreCase("true")) {
                            logUnofficialMacAppUse();
                        }
                    } catch (NullPointerException ignored) {
                        logUnofficialMacAppUse();
                    }
                }
            }
        }
    }

    /**
     * Log that MV wasn´t used via the official mac app.
     * This is relevant to know for bug reports.
     */
    private void logUnofficialMacAppUse() {
        Log.errorLog(915263987, "MV wird NICHT über die offizielle Mac App genutzt.");
    }

    private void finalizeDownload(DatenDownload datenDownload, Start start /* wegen "datenDownload.start=null" beim stoppen */, HttpDownloadState state) {
        deleteIfEmpty(new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]));
        setFileSize(datenDownload);

        if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT])) {
            writeSpotlightComment(datenDownload, state == HttpDownloadState.CANCEL);
        }

        fertigmeldung(datenDownload, start, state == HttpDownloadState.CANCEL);
        switch (state) {
            case CANCEL:
                datenDownload.resetDownload();
                break;
            default:
                start.restSekunden = -1;
                start.percent = Start.PROGRESS_FERTIG;
                datenDownload.mVFilmSize.setAktSize(-1);
                break;
        }
        notifyStartEvent(datenDownload);

        if (SystemInfo.isMacOSX() && Daten.mediathekGui != null) {
            Application.getApplication().requestUserAttention(false);
        }
    }

    /**
     * tatsächliche Dateigröße eintragen
     *
     * @param DatenDownload with the info of the file
     */
    private void setFileSize(DatenDownload datenDownload) {
        try {
            if (new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]).exists()) {
                long l = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]).length();
                if (l > 0) {
                    datenDownload.mVFilmSize.setSize(l);
                }
            }
        } catch (Exception ex) {
            Log.errorLog(461204780, "Fehler beim Ermitteln der Dateigröße: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

    private void notifyStartEvent(DatenDownload datenDownload) {
        Listener.notify(Listener.EREIGNIS_START_EVENT, StarterClass.class.getSimpleName());
        if (datenDownload != null) {
            if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON) {
                Listener.notify(Listener.EREIGNIS_START_EVENT_BUTTON, StarterClass.class.getSimpleName());
            }
        }
    }

    private enum HttpDownloadState {

        CANCEL, ERROR, DOWNLOAD
    }

    // ********************************************
    // Hier wird dann gestartet
    // Ewige Schleife die die Downloads startet
    // ********************************************
    private class Starten extends Thread {

        private DatenDownload datenDownload;
        /**
         * The only {@link java.util.Timer} used for all {@link mediathek.tool.MVInputStream.BandwidthCalculationTask}
         * calculation tasks.
         */
        private java.util.Timer bandwidthCalculationTimer;

        public Starten() {
            super();
            setName("DownloadStarter Daemon Thread");
            setDaemon(true);
            bandwidthCalculationTimer = new java.util.Timer("BandwidthCalculationTimer");
        }

        @Override
        public synchronized void run() {
            while (!isInterrupted()) {
                try {
                    while ((datenDownload = getNextStart()) != null) {
                        startStarten(datenDownload);
                        //alle 5 Sekunden einen Download starten
                        sleep(5 * 1000);
                    }
                    Daten.listeDownloadsButton.buttonStartsPutzen(); // Button Starts aus der Liste löschen
                    sleep(3 * 1000);
                } catch (Exception ex) {
                    Log.errorLog(613822015, ex);
                }
            }
        }

        private synchronized DatenDownload getNextStart() throws InterruptedException {
            // get: erstes passendes Element der Liste zurückgeben oder null
            // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
            if (pause) {
                // beim Löschen der Downloads, kann das Starten etwas "pausiert" werden
                // damit ein zu Löschender Download nicht noch schnell gestartet wird
                sleep(5 * 1000);
                pause = false;
            }

            DatenDownload download = Daten.listeDownloads.getNextStart();
            if (Daten.debug && download == null) {
                // dann versuchen einen Fehlerhaften nochmal zu starten
                download = Daten.listeDownloads.getRestartDownload();
                if (download != null) {
                    reStartmeldung(download, download.start);
                }
            }
            return download;
        }

        /**
         * This will start the download process.
         *
         * @param datenDownload The {@link mediathek.daten.DatenDownload} info object for download.
         */
        private void startStarten(DatenDownload datenDownload) {
            datenDownload.start.startZeit = new Datum();
            Listener.notify(Listener.EREIGNIS_ART_DOWNLOAD_PROZENT, StarterClass.class.getName());
            Thread downloadThread;

            switch (datenDownload.art) {
                case DatenDownload.ART_PROGRAMM:
                    downloadThread = new ExternalProgramDownloadThread(datenDownload);
                    downloadThread.start();
                    break;
                case DatenDownload.ART_DOWNLOAD:
                    downloadThread = new DirectHttpDownloadThread(datenDownload, bandwidthCalculationTimer);
                    downloadThread.start();
                    break;
                default:
                    Log.errorLog(789356001, "StarterClass.Starten - Switch-default");
                    break;
            }
        }
    }

    /**
     * Download files via an external program.
     */
    private class ExternalProgramDownloadThread extends Thread {

        private DatenDownload datenDownload;
        private Start start;
        private RuntimeExec runtimeExec;
        private File file;
        private String exMessage = "";
        private boolean retAbbrechen;
        private boolean dialogAbbrechenIsVis;
        private HttpDownloadState state = HttpDownloadState.DOWNLOAD;

        public ExternalProgramDownloadThread(DatenDownload d) {
            super();
            setName("PROGRAMM DL THREAD: " + d.arr[DatenDownload.DOWNLOAD_TITEL]);

            datenDownload = d;
            start = datenDownload.start;
            start.status = Start.STATUS_RUN;
            file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
            notifyStartEvent(datenDownload);
            try {
                if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI])) {
                    MVInfoFile.writeInfoFile(datenDownload);
                }
                if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE])) {
                    MVSubtitle.writeSubtitle(datenDownload);
                }

                Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
            } catch (IOException ignored) {
            } catch (Exception ex) {
                Log.errorLog(469365281, ex);
            }
        }

        @Override
        public synchronized void run() {
            long filesize = -1;
            final int stat_start = 0;
            final int stat_laufen = 1;
            final int stat_restart = 3;
            final int stat_pruefen = 4;
            // ab hier ist schluss
            final int stat_fertig_ok = 10;
            final int stat_fertig_fehler = 11;
            final int stat_ende = 99;
            int stat = stat_start;
            try {
                if (!cancelDownload()) {
                    while (stat < stat_ende) {
                        switch (stat) {
                            case stat_start:
                                // versuch das Programm zu Starten
                                if (starten()) {
                                    if (datenDownload.isDownloadManager()) {
                                        stat = stat_fertig_ok;
                                    } else {
                                        stat = stat_laufen;
                                    }
                                } else {
                                    stat = stat_restart;
                                }
                                break;
                            case stat_laufen:
                                //hier läuft der Download bis zum Abbruch oder Ende
                                try {
                                    if (start.stoppen) {
                                        stat = stat_fertig_ok;
                                        if (start.process != null) {
                                            start.process.destroy();
                                        }
                                    } else {
                                        int exitV = start.process.exitValue();
                                        if (exitV != 0) {
                                            stat = stat_restart;
                                        } else {
                                            stat = stat_pruefen;
                                        }
                                    }
                                } catch (Exception ex) {
                                    try {
                                        this.wait(2000);
                                    } catch (InterruptedException ignored) {
                                    }
                                }
                                break;
                            case stat_restart:
                                if (!datenDownload.isRestart()) {
                                    // dann wars das
                                    stat = stat_fertig_fehler;
                                } else if (filesize == -1) {
                                    //noch nichts geladen
                                    deleteIfEmpty(file);
                                    if (file.exists()) {
                                        // dann bestehende Datei weitermachen
                                        filesize = file.length();
                                        stat = stat_start;
                                    } else // counter prüfen und bei einem Maxwert cancelDownload, sonst endlos
                                     if (start.startcounter < Start.STARTCOUNTER_MAX) {
                                            // dann nochmal von vorne
                                            stat = stat_start;
                                        } else {
                                            // dann wars das
                                            stat = stat_fertig_fehler;
                                        }
                                } else //jetzt muss das File wachsen, sonst kein Restart
                                 if (!file.exists()) {
                                        // dann wars das
                                        stat = stat_fertig_fehler;
                                    } else if (file.length() > filesize) {
                                        //nur weitermachen wenn die Datei tasächlich wächst
                                        filesize = file.length();
                                        stat = stat_start;
                                    } else {
                                        // dann wars das
                                        stat = stat_fertig_fehler;
                                    }
                                break;
                            case stat_pruefen:
                                if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON || datenDownload.isDownloadManager()) {
                                    //für die direkten Starts mit dem Button und die remote downloads wars das dann
                                    stat = stat_fertig_ok;
                                } else if (pruefen(datenDownload, start)) {
                                    //fertig und OK
                                    stat = stat_fertig_ok;
                                } else {
                                    //fertig und fehlerhaft
                                    stat = stat_fertig_fehler;
                                }
                                break;
                            case stat_fertig_fehler:
                                start.status = Start.STATUS_ERR;
                                stat = stat_ende;
                                break;
                            case stat_fertig_ok:
                                start.status = Start.STATUS_FERTIG;
                                stat = stat_ende;
                                break;
                        }
                    }
                }
            } catch (Exception ex) {
                exMessage = ex.getLocalizedMessage();
                Log.errorLog(395623710, ex);
                SwingUtilities.invokeLater(() -> {
                    if (!Daten.auto) {
                        new MeldungDownloadfehler(Daten.mediathekGui, exMessage, datenDownload).setVisible(true);
                    }
                });
            }
            finalizeDownload(datenDownload, start, state);
        }

        private boolean starten() {
            boolean ret = false;
            // die Reihenfolge: startcounter - startmeldung ist wichtig!
            start.startcounter++;
            startmeldung(datenDownload, start);
            runtimeExec = new RuntimeExec(datenDownload.mVFilmSize, datenDownload.start,
                    datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF], datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
            start.process = runtimeExec.exec(true /*log*/);
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
            if (Daten.auto) {
                // dann mit gleichem Namen und Datei vorher löschen
                try {
                    Files.deleteIfExists(file.toPath());
                    file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                } catch (IOException ex) {
                    // kann nicht gelöscht werden, evtl. klappt ja das Überschreiben
                    Log.errorLog(795623145, ex, "file exists: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                }
                return false; //auf keinen Fall den Dialog starten :)
            }

            dialogAbbrechenIsVis = true;
            retAbbrechen = true;
            if (SwingUtilities.isEventDispatchThread()) {
                retAbbrechen = abbrechen_();
            } else {
                SwingUtilities.invokeLater(() -> {
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
                DialogContinueDownload dialogContinueDownload = new DialogContinueDownload(Daten.mediathekGui, datenDownload, false /*weiterführen*/);
                dialogContinueDownload.setVisible(true);

                switch (dialogContinueDownload.getResult()) {
                    case CANCELLED:
                        // dann wars das
                        state = HttpDownloadState.CANCEL;
                        result = true;
                        break;

                    case CONTINUE:
                        // dann mit gleichem Namen und Datei vorher löschen
                        try {
                            Files.deleteIfExists(file.toPath());
                            file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                        } catch (Exception ex) {
                            // kann nicht gelöscht werden, evtl. klappt ja das Überschreiben
                            Log.errorLog(945120398, ex, "file exists: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                        }
                        break;

                    case RESTART_WITH_NEW_NAME:
                        if (dialogContinueDownload.isNewName()) {
                            // jetzt den Programmaufruf nochmal mit dem geänderten Dateinamen nochmal bauen
                            datenDownload.aufrufBauen();
                            Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
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

    /**
     * Download content directly from HTTP server.
     */
    private class DirectHttpDownloadThread extends Thread {

        private final DatenDownload datenDownload;
        private final Start start;
        private HttpURLConnection conn = null;
        private HttpDownloadState state = HttpDownloadState.DOWNLOAD;
        private long downloaded = 0;
        private File file = null;
        private String responseCode;
        private String exMessage;

        private FileOutputStream fos = null;

        private final java.util.Timer bandwidthCalculationTimer;
        private boolean retAbbrechen;
        private boolean dialogAbbrechenIsVis;

        public DirectHttpDownloadThread(DatenDownload d, java.util.Timer bandwidthCalculationTimer) {
            super();
            this.bandwidthCalculationTimer = bandwidthCalculationTimer;
            datenDownload = d;
            start = datenDownload.start;
            setName("DIRECT DL THREAD_" + d.arr[DatenDownload.DOWNLOAD_TITEL]);
            start.status = Start.STATUS_RUN;
            notifyStartEvent(datenDownload);
        }

        /**
         * Return the content length of the requested Url.
         *
         * @param url {@link java.net.URL} to the specified content.
         * @return Length in bytes or -1 on error.
         */
        private long getContentLength(final URL url) {
            final int TIMEOUT_LENGTH = 5000; //ms, beim Start eines Downloads
            long ret = -1;
            HttpURLConnection connection = null;
            try {
                connection = (HttpURLConnection) url.openConnection();
                connection.setRequestProperty("User-Agent", Daten.getUserAgent());
                connection.setReadTimeout(TIMEOUT_LENGTH);
                connection.setConnectTimeout(TIMEOUT_LENGTH);
                if (connection.getResponseCode() < HttpURLConnection.HTTP_BAD_REQUEST) {
                    ret = connection.getContentLengthLong();
                }
                // alles unter 300k sind Playlisten, ...
                if (ret < 300 * 1000) {
                    ret = -1;
                }
            } catch (Exception ex) {
                ret = -1;
                Log.errorLog(643298301, ex);
            } finally {
                if (connection != null) {
                    connection.disconnect();
                }
            }
            return ret;
        }

        /**
         * Setup the HTTP connection common settings
         *
         * @param conn The active connection.
         */
        private void setupHttpConnection(HttpURLConnection conn) {
            conn.setRequestProperty("Range", "bytes=" + downloaded + "-");
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            conn.setDoInput(true);
            conn.setDoOutput(true);
        }

        /**
         * Start the actual download process here.
         *
         * @throws Exception
         */
        private void downloadContent() throws Exception {
            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI])) {
                MVInfoFile.writeInfoFile(datenDownload);
            }
            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SUBTITLE])) {
                MVSubtitle.writeSubtitle(datenDownload);
            }
            datenDownload.interruptRestart();
            start.mVInputStream = new MVInputStream(conn.getInputStream(), bandwidthCalculationTimer);

            fos = new FileOutputStream(file, (downloaded != 0));

            datenDownload.mVFilmSize.addAktSize(downloaded);
            final byte[] buffer = new byte[MVBandwidthTokenBucket.DEFAULT_BUFFER_SIZE];
            long p, pp = 0, startProzent = -1;
            int len;
            long aktBandwidth, aktSize = 0;
            boolean melden = false;

            while ((len = start.mVInputStream.read(buffer)) != -1 && (!start.stoppen)) {
                downloaded += len;
                fos.write(buffer, 0, len);
                datenDownload.mVFilmSize.addAktSize(len);

                //für die Anzeige prüfen ob sich was geändert hat
                if (aktSize != datenDownload.mVFilmSize.getAktSize()) {
                    aktSize = datenDownload.mVFilmSize.getAktSize();
                    melden = true;
                }
                if (datenDownload.mVFilmSize.getSize() > 0) {
                    p = (aktSize * (long) 1000) / datenDownload.mVFilmSize.getSize();
                    if (startProzent == -1) {
                        startProzent = p;
                    }
                    // p muss zwischen 1 und 999 liegen
                    if (p == 0) {
                        p = Start.PROGRESS_GESTARTET;
                    } else if (p >= 1000) {
                        p = 999;
                    }
                    start.percent = (int) p;
                    if (p != pp) {
                        pp = p;
                        // Restzeit ermitteln
                        if (p > 2 && p > startProzent) {
                            // sonst macht es noch keinen Sinn
                            int diffZeit = start.startZeit.diffInSekunden();
                            int restProzent = 1000 - (int) p;
                            start.restSekunden = (diffZeit * restProzent / (p - startProzent));
                            // anfangen zum Schauen kann man, wenn die Restzeit kürzer ist
                            // als die bereits geladene Speilzeit des Films
                            bereitsAnschauen(datenDownload);
                        }
                        melden = true;
                    }
                }
                aktBandwidth = start.mVInputStream.getBandwidth(); // bytes per second
                if (aktBandwidth != start.bandbreite) {
                    start.bandbreite = aktBandwidth;
                    melden = true;
                }
                if (melden) {
                    Listener.notify(Listener.EREIGNIS_ART_DOWNLOAD_PROZENT, StarterClass.class.getName());
                    melden = false;
                }
            }
            SysMsg.sysMsg(start.mVInputStream.toString());
            if (!start.stoppen) {
                if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON) {
                    // direkter Start mit dem Button
                    start.status = Start.STATUS_FERTIG;
                } else if (pruefen(datenDownload, start)) {
                    //Anzeige ändern - fertig
                    start.status = Start.STATUS_FERTIG;
                } else {
                    //Anzeige ändern - bei Fehler fehlt der Eintrag
                    start.status = Start.STATUS_ERR;
                }
            }
        }

        @Override
        public synchronized void run() {
            startmeldung(datenDownload, start);
            try {
                Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]));
            } catch (IOException ignored) {
            }

            try {
                final URL url = new URL(datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
                datenDownload.mVFilmSize.setSize(getContentLength(url));
                datenDownload.mVFilmSize.setAktSize(0);
                conn = (HttpURLConnection) url.openConnection();
                conn.setConnectTimeout(1000 * MVConfig.getInt(MVConfig.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN, MVConfig.PARAMETER_TIMEOUT_SEKUNDEN));
                conn.setReadTimeout(1000 * MVConfig.getInt(MVConfig.SYSTEM_PARAMETER_DOWNLOAD_TIMEOUT_SEKUNDEN, MVConfig.PARAMETER_TIMEOUT_SEKUNDEN));
                file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
                if (!cancelDownload()) {
                    setupHttpConnection(conn);
                    conn.connect();
                    final int httpResponseCode = conn.getResponseCode();
                    if (httpResponseCode >= HttpURLConnection.HTTP_BAD_REQUEST) {
                        //Range passt nicht, also neue Verbindung versuchen...
                        if (httpResponseCode == 416) {
                            conn.disconnect();
                            //Get a new connection and reset download param...
                            conn = (HttpURLConnection) url.openConnection();
                            downloaded = 0;
                            setupHttpConnection(conn);
                            conn.connect();
                            //hier war es dann nun wirklich...
                            if (conn.getResponseCode() >= HttpURLConnection.HTTP_BAD_REQUEST) {
                                state = HttpDownloadState.ERROR;
                            } else {
                                state = HttpDownloadState.DOWNLOAD;
                            }
                        } else {
                            // ==================================
                            // dann wars das
                            responseCode = "Responsecode: " + conn.getResponseCode() + "\n" + conn.getResponseMessage();
                            Log.errorLog(915236798, "HTTP-Fehler: " + conn.getResponseCode() + " " + conn.getResponseMessage());
                            SwingUtilities.invokeLater(() -> {
                                if (!Daten.auto) {
                                    new MeldungDownloadfehler(Daten.mediathekGui, "URL des Films:\n"
                                            + datenDownload.arr[DatenDownload.DOWNLOAD_URL] + "\n\n"
                                            + responseCode + "\n", datenDownload).setVisible(true);
                                }
                            });
                            state = HttpDownloadState.ERROR;
                        }
                    }
                }
                switch (state) {
                    case DOWNLOAD:
                        downloadContent();
                        break;
                    case CANCEL:
//                        start.status = Start.STATUS_RUN; // bei "init" wird er sonst nochmal gestartet
                        break;
                    case ERROR:
                        start.status = Start.STATUS_ERR;
                        break;
                }
            } catch (Exception ex) {
                exMessage = ex.getLocalizedMessage();
                Log.errorLog(316598941, ex, "Fehler");
                start.status = Start.STATUS_ERR;
                SwingUtilities.invokeLater(() -> {
                    if (!Daten.auto) {
                        new MeldungDownloadfehler(Daten.mediathekGui, exMessage, datenDownload).setVisible(true);
                    }
                });
            }

            try {
                if (start.mVInputStream != null) {
                    start.mVInputStream.close();
                }
                if (fos != null) {
                    fos.close();
                }
                if (conn != null) {
                    conn.disconnect();
                }
            } catch (Exception ignored) {
            }

            finalizeDownload(datenDownload, start, state);
        }

        private boolean cancelDownload() {
            if (!file.exists()) {
                // dann ist alles OK
                return false;
            }
            if (Daten.auto) {
                return false; // immer überschreiben, keine GUI!!!
            }

            dialogAbbrechenIsVis = true;
            retAbbrechen = true;
            if (SwingUtilities.isEventDispatchThread()) {
                retAbbrechen = abbrechen_();
            } else {
                SwingUtilities.invokeLater(() -> {
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
                DialogContinueDownload dialogContinueDownload = new DialogContinueDownload(Daten.mediathekGui, datenDownload, true /*weiterführen*/);
                dialogContinueDownload.setVisible(true);

                switch (dialogContinueDownload.getResult()) {
                    case CANCELLED:
                        // dann wars das
                        state = HttpDownloadState.CANCEL;
                        result = true;
                        break;

                    case CONTINUE:
                        downloaded = file.length();
                        break;

                    case RESTART_WITH_NEW_NAME:
                        if (dialogContinueDownload.isNewName()) {
                            Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
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

        private void bereitsAnschauen(DatenDownload datenDownload) {
            if (datenDownload.film != null && datenDownload.start != null) {
                if (datenDownload.film.dauerL > 0
                        && datenDownload.start.restSekunden > 0
                        && datenDownload.mVFilmSize.getAktSize() > 0
                        && datenDownload.mVFilmSize.getSize() > 0) {
                    // macht nur dann Sinn
                    final long zeitGeladen = datenDownload.film.dauerL * datenDownload.mVFilmSize.getAktSize() / datenDownload.mVFilmSize.getSize();
                    if (zeitGeladen > (datenDownload.start.restSekunden * 1.1 /* plus 10% zur Sicherheit*/)) {
                        datenDownload.start.beginnAnschauen = true;
                    }
                }
            }
        }
    }
}
