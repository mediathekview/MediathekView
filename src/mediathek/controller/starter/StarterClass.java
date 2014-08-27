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

import com.jidesoft.utils.SystemInfo;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.gui.dialog.DialogContinueDownload;
import mediathek.gui.dialog.DialogDownloadfehler;
import mediathek.tool.Datum;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVInfoFile;
import mediathek.tool.MVInputStream;
import mediathek.tool.MVNotification;
import msearch.daten.DatenFilm;

public class StarterClass {
    //Tags Filme

    private Daten daten;
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
        String url = ersterFilm.arr[DatenFilm.FILM_URL_NR];
        if (!url.equals("")) {
            DatenDownload d = new DatenDownload(pSet, ersterFilm, Start.QUELLE_BUTTON, null, "", "", aufloesung);
            d.start = new Start();
            starten.startStarten(d);
            // gestartete Filme (originalURL des Films) auch in die History eintragen
            daten.history.zeileSchreiben(ersterFilm.arr[DatenFilm.FILM_THEMA_NR], ersterFilm.arr[DatenFilm.FILM_TITEL_NR], d.arr[DatenDownload.DOWNLOAD_HISTORY_URL_NR]);
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
                Log.fehlerMeldung(696510258,  "StartetClass.pruefen-3", "Download fehlgeschlagen: 99,5% wurden nicht erreicht"
                        + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
                return false;
            }
        }
        File file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        if (!file.exists()) {
            Log.fehlerMeldung(550236231,"StartetClass.pruefen-1", "Download fehlgeschlagen: Datei existiert nicht" + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        } else if (file.length() < Konstanten.MIN_DATEI_GROESSE_FILM) {
            Log.fehlerMeldung(795632500,"StartetClass.pruefen-2", "Download fehlgeschlagen: Datei zu klein" + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        } else {
            if (datenDownload.istAbo()) {
                daten.erledigteAbos.zeileSchreiben(datenDownload.arr[DatenDownload.DOWNLOAD_THEMA_NR],
                        datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR],
                        datenDownload.arr[DatenDownload.DOWNLOAD_HISTORY_URL_NR]);
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
                    Log.systemMeldung(new String[]{"Restart/Aufräumen: leere Datei löschen", file.getAbsolutePath()});
                    if (!file.delete()) {
                        throw new Exception();
                    }
                } else if (file.length() < Konstanten.MIN_DATEI_GROESSE_FILM) {
                    Log.systemMeldung(new String[]{"Restart/Aufräumen: Zu kleine Datei löschen", file.getAbsolutePath()});
                    if (!file.delete()) {
                        throw new Exception();
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(795632500,  "StartetClass.deleteIfEmpty", "Fehler beim löschen" + file.getAbsolutePath());
        }
    }

    private void startmeldung(DatenDownload datenDownload, Start start) {
        ArrayList<String> text = new ArrayList<>();
        boolean abspielen = datenDownload.getQuelle() == Start.QUELLE_BUTTON;
        if (abspielen) {
            text.add("Film abspielen");
        } else {
            if (start.startcounter > 1) {
                text.add("Download starten - Restart (Summe Starts: " + start.startcounter + ")");
            } else {
                text.add("Download starten");
            }
            text.add("Programmset: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET_NR]);
            text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        }
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
        text.add("Startzeit: " + new SimpleDateFormat("HH:mm:ss").format(start.startZeit));
        if (datenDownload.getArt() == Start.ART_DOWNLOAD) {
            text.add(Start.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);
        }
        Log.systemMeldung(text.toArray(new String[text.size()]));
    }

    private void fertigmeldung(final DatenDownload datenDownload, final Start start, boolean abgebrochen) {
        ArrayList<String> text = new ArrayList<>();
        if (abgebrochen) {
            text.add("Download wurde abgebrochen");
        } else if (datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
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
            text.add("Programmset: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET_NR]);
            text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
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
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
        if (datenDownload.getArt() == Start.ART_DOWNLOAD) {
            text.add(Start.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);
        }
        Log.systemMeldung(text.toArray(new String[text.size()]));
        if (!start.stoppen && !abgebrochen) {
            if (datenDownload.getQuelle() != Start.QUELLE_BUTTON) {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        MVNotification.addNotification(daten, datenDownload, start.status != Start.STATUS_ERR);
                    }
                });
            }
        }
    }

    private final static String GERMANY_ONLY_TRIM_TEXT = "+++ Aus rechtlichen Gründen ist dieses Video nur innerhalb von Deutschland abrufbar. +++";

    /**
     * Removes - if existing - the Germany only notice in descriptions.
     * @param strInput The original film description.
     * @return The stripped string.
     */
    private String removeGermanyOnlyMessage(final String strInput)
    {
        if (strInput.contains(GERMANY_ONLY_TRIM_TEXT)) {
            final int inputLength = strInput.length();
            final int messageLength = GERMANY_ONLY_TRIM_TEXT.length();

            String newComment = String.copyValueOf(strInput.toCharArray(),messageLength,(inputLength - messageLength));
            newComment = newComment.trim();
            System.out.println("STRIPPED COMMENT: " + newComment);

            return newComment;
        } else {
            return strInput;
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

        final Path filmPath = Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        if (Files.exists(filmPath)) {
            final String strFilePath = filmPath.toString();
            String strComment = datenDownload.film.arr[DatenFilm.FILM_BESCHREIBUNG_NR];
            if (strComment != null) {
                //no need to write spotlight data when there is no description...
                if (strComment.isEmpty()) {
                    return;
                }

                strComment = removeGermanyOnlyMessage(strComment);

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
                    Log.fehlerMeldung(915263987,  "StarterClass.writeSpotlightComment", "Fehler beim Spotlight schreiben" + filmPath.toString());
                    //AppleScript may not be available if user does not use the official MacApp.
                    //We need to log that as well if there are error reports.
                    if (!System.getProperty("OSX_OFFICIAL_APP").equalsIgnoreCase("true")) {
                        Log.fehlerMeldung(915263987, "StarterClass.writeSpotlightComment", "MV wird NICHT über die offizielle Mac App genutzt.");
                    }
                }
            }
        }
    }

    private void notifyStartEvent(DatenDownload datenDownload) {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, StarterClass.class.getSimpleName());
        if (datenDownload != null) {
            if (datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT_BUTTON, StarterClass.class.getSimpleName());
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
                    Log.fehlerMeldung(613822015,  "StarterClass.Starten.run", ex);
                }
            } //while(true)
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
            return Daten.listeDownloads.getNextStart();
        }

        /**
         * This will start the download process.
         *
         * @param datenDownload The {@link mediathek.daten.DatenDownload} info object for download.
         */
        private void startStarten(DatenDownload datenDownload) {
            datenDownload.start.startZeit = new Datum();
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, StarterClass.class.getName());
            Thread downloadThread;

            switch (datenDownload.getArt()) {
                case Start.ART_PROGRAMM:
                    downloadThread = new ExternalProgramDownloadThread(datenDownload);
                    downloadThread.start();
                    break;
                case Start.ART_DOWNLOAD:
                    downloadThread = new DirectHttpDownloadThread(datenDownload, bandwidthCalculationTimer);
                    downloadThread.start();
                    break;
                default:
                    Log.fehlerMeldung(789356001,  "StartetClass.startStarten", "StarterClass.Starten - Switch-default");
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

        public ExternalProgramDownloadThread(DatenDownload d) {
            super();
            setName("PROGRAMM DL THREAD: " + d.arr[DatenDownload.DOWNLOAD_TITEL_NR]);

            datenDownload = d;
            start = datenDownload.start;
            start.status = Start.STATUS_RUN;
            file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
            notifyStartEvent(datenDownload);
            try {
                if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI_NR])) {
                    MVInfoFile.writeInfoFile(datenDownload);
                }

                Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]));
            } catch (IOException ignored) {
            } catch (Exception ex) {
                Log.fehlerMeldung(469365281, "StarterClass.StartenProgramm-1", ex);
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
                while (stat < stat_ende) {
                    switch (stat) {
                        case stat_start:
                            // versuch das Programm zu Starten
                            if (starten()) {
                                stat = stat_laufen;
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
                            } else {
                                if (filesize == -1) {
                                    //noch nichts geladen
                                    deleteIfEmpty(file);
                                    if (file.exists()) {
                                        // dann bestehende Datei weitermachen
                                        filesize = file.length();
                                        stat = stat_start;
                                    } else {
                                        // counter prüfen und bei einem Maxwert cancelDownload, sonst endlos
                                        if (start.startcounter < Start.STARTCOUNTER_MAX) {
                                            // dann nochmal von vorne
                                            stat = stat_start;
                                        } else {
                                            // dann wars das
                                            stat = stat_fertig_fehler;
                                        }
                                    }
                                } else {
                                    //jetzt muss das File wachsen, sonst kein Restart
                                    if (!file.exists()) {
                                        // dann wars das
                                        stat = stat_fertig_fehler;
                                    } else {
                                        if (file.length() > filesize) {
                                            //nur weitermachen wenn die Datei tasächlich wächst
                                            filesize = file.length();
                                            stat = stat_start;
                                        } else {
                                            // dann wars das
                                            stat = stat_fertig_fehler;
                                        }
                                    }
                                }
                            }
                            break;
                        case stat_pruefen:
                            if (datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
                                //für die direkten Starts mit dem Button wars das dann
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
            } catch (Exception ex) {
                exMessage = ex.getLocalizedMessage();
                Log.fehlerMeldung(395623710, "StarterClass.StartenProgramm-2", ex);
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        new DialogDownloadfehler(daten.mediathekGui, exMessage, datenDownload).setVisible(true);
                    }
                });
            }
            deleteIfEmpty(file);

            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT_NR])) {
                writeSpotlightComment(datenDownload, false);
            }

            fertigmeldung(datenDownload, start, false);
            start.restSekunden = -1;
            start.percent = Start.PROGRESS_FERTIG;
            notifyStartEvent(datenDownload);

            if (SystemInfo.isMacOSX()) {
                daten.mediathekGui.getOsxApplicationAdapter().requestUserAttention(false);
            }
        }

        private boolean starten() {
            boolean ret = false;
            // die Reihenfolge: startcounter - startmeldung ist wichtig!
            start.startcounter++;
            startmeldung(datenDownload, start);
            runtimeExec = new RuntimeExec(datenDownload);
            start.process = runtimeExec.exec();
            if (start.process != null) {
                ret = true;
            }
            return ret;
        }
    }

    /**
     * Download content directly from HTTP server.
     */
    private class DirectHttpDownloadThread extends Thread {

        private DatenDownload datenDownload;
        private Start start;
        private HttpURLConnection conn = null;
        private HttpDownloadState state = HttpDownloadState.DOWNLOAD;
        private int downloaded = 0;
        private File file = null;
        private String responseCode;
        private String exMessage;

        private FileOutputStream fos = null;

        private java.util.Timer bandwidthCalculationTimer;
        private boolean retAbbrechen;
        private boolean dialogAbbrechenIsVis;

        public DirectHttpDownloadThread(DatenDownload d, java.util.Timer bandwidthCalculationTimer) {
            super();
            this.bandwidthCalculationTimer = bandwidthCalculationTimer;
            datenDownload = d;
            start = datenDownload.start;
            setName("DIRECT DL THREAD_" + d.arr[DatenDownload.DOWNLOAD_TITEL_NR]);
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
            final int TIMEOUT = 5000; //ms, beim Start eines Downloads
            long ret = -1;
            HttpURLConnection conn = null;
            try {
                conn = (HttpURLConnection) url.openConnection();
                conn.setRequestProperty("User-Agent", Daten.getUserAgent());
                conn.setReadTimeout(TIMEOUT);
                conn.setConnectTimeout(TIMEOUT);
                if (conn.getResponseCode() < 400) {
                    ret = conn.getContentLengthLong();
                }
                // alles unter 300k sind Playlisten, ...
                if (ret < 300 * 1024) {
                    ret = -1;
                }
            } catch (Exception ex) {
                ret = -1;
                Log.fehlerMeldung(643298301,  "StarterClass.StartenDownload.getContentLength", ex);
            } finally {
                if (conn != null) {
                    conn.disconnect();
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
            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_INFODATEI_NR])) {
                MVInfoFile.writeInfoFile(datenDownload);
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
                aktBandwidth = start.mVInputStream.getBandwidth();
                if (aktBandwidth != start.bandbreite) {
                    start.bandbreite = aktBandwidth;
                    melden = true;
                }
                if (melden) {
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, StarterClass.class.getName());
                    melden = false;
                }
            }
            Log.systemMeldung(start.mVInputStream.toString());
            if (!start.stoppen) {
                if (datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
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
        public void run() {
            startmeldung(datenDownload, start);
            try {
                Files.createDirectories(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]));
            } catch (IOException ignored) {
            }

            try {
                final URL url = new URL(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                datenDownload.mVFilmSize.setSize(getContentLength(url));
                datenDownload.mVFilmSize.setAktSize(0);
                conn = (HttpURLConnection) url.openConnection();
                file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
                if (!cancelDownload()) {
                    setupHttpConnection(conn);
                    conn.connect();
                    final int httpResponseCode = conn.getResponseCode();
                    if (httpResponseCode >= 400) {
                        //Range passt nicht, also neue Verbindung versuchen...
                        if (httpResponseCode == 416) {
                            conn.disconnect();
                            //Get a new connection and reset download param...
                            conn = (HttpURLConnection) url.openConnection();
                            downloaded = 0;
                            setupHttpConnection(conn);
                            conn.connect();
                            //hier war es dann nun wirklich...
                            if (conn.getResponseCode() >= 400) {
                                state = HttpDownloadState.ERROR;
                            } else {
                                state = HttpDownloadState.DOWNLOAD;
                            }
                        } else {
                            // ==================================
                            // dann wars das
                            responseCode = "Responsecode: " + conn.getResponseCode() + "\n" + conn.getResponseMessage();
                            Log.fehlerMeldung(915236798, "StartetClass.StartenDownload", "HTTP-Fehler: " + conn.getResponseCode() + " " + conn.getResponseMessage());
                            SwingUtilities.invokeLater(new Runnable() {
                                @Override
                                public void run() {
                                    new DialogDownloadfehler(daten.mediathekGui, "URL des Films:\n"
                                            + datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR] + "\n\n"
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
                Log.fehlerMeldung(316598941,  "StartetClass.StartenDownload", ex, "Fehler");
                start.status = Start.STATUS_ERR;
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        new DialogDownloadfehler(daten.mediathekGui, exMessage, datenDownload).setVisible(true);
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

            finalizeDownload();
        }

        private void finalizeDownload() {
            deleteIfEmpty(new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]));

            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT_NR])) {
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

            if (SystemInfo.isMacOSX()) {
                daten.mediathekGui.getOsxApplicationAdapter().requestUserAttention(false);
            }
        }

        private boolean cancelDownload() {
            if (Daten.auto) {
                return false;
            }

            dialogAbbrechenIsVis = true;
            retAbbrechen = true;
            if (SwingUtilities.isEventDispatchThread()) {
                retAbbrechen = abbrechen_();
            } else {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        retAbbrechen = abbrechen_();
                        dialogAbbrechenIsVis = false;
                    }
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
                DialogContinueDownload dialogContinueDownload = new DialogContinueDownload(daten.mediathekGui, datenDownload);
                dialogContinueDownload.setVisible(true);

                switch (dialogContinueDownload.getResult()) {
                    case CANCELLED:
                        // dann wars das
                        state = HttpDownloadState.CANCEL;
                        result = true;
                        break;

                    case CONTINUE:
                        downloaded = (int) file.length();
                        break;

                    case RESTART_WITH_NEW_NAME:
                        if (dialogContinueDownload.isNewName()) {
                            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
                            try {
                                Files.createDirectory(Paths.get(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]));
                            } catch (IOException ignored) {
                            }
                            file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
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
                    long zeitGeladen = datenDownload.film.dauerL * datenDownload.mVFilmSize.getAktSize() / datenDownload.mVFilmSize.getSize();
                    if (zeitGeladen > (datenDownload.start.restSekunden * 1.1 /* plus 10% zur Sicherheit*/)) {
                        datenDownload.start.beginnAnschauen = true;
                    }
                }
            }
        }
    }
}
