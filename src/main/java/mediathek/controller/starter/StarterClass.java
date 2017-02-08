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
import mSearch.daten.DatenFilm;
import mSearch.tool.Datum;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.mac.SpotlightCommentWriter;
import mediathek.tool.FormatterUtil;
import mediathek.tool.MVFilmSize;
import mediathek.tool.MVNotification;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.util.ArrayList;

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
        if (!url.isEmpty()) {
            DatenDownload d = new DatenDownload(pSet, ersterFilm, DatenDownload.QUELLE_BUTTON, null, "", "", aufloesung);
            d.start = new Start();
            starten.startStarten(d);
            // gestartete Filme (originalURL des Films) auch in die History eintragen
            daten.history.zeileSchreiben(ersterFilm.arr[DatenFilm.FILM_THEMA], ersterFilm.arr[DatenFilm.FILM_TITEL], d.arr[DatenDownload.DOWNLOAD_HISTORY_URL]);
            daten.getListeFilmeHistory().add(ersterFilm);
            // und jetzt noch in die Downloadliste damit die Farbe im Tab Filme passt
            daten.getListeDownloadsButton().addMitNummer(d);
        }
    }

    public void pause() {
        pause = true;
    }

    static boolean pruefen(Daten daten, DatenDownload datenDownload, Start start) {
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
    static void deleteIfEmpty(File file) {
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
        text.add("Startzeit: " + FormatterUtil.FORMATTER_HHmmss.format(start.startZeit));
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
            text.add(DatenDownload.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF]);
            text.add("Programmaufruf[]: " + datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY]);
        }
        SysMsg.sysMsg(text.toArray(new String[text.size()]));
    }

    private void reStartmeldung(DatenDownload datenDownload) {
        ArrayList<String> text = new ArrayList<>();
        text.add("Fehlerhaften Download neu starten - Restart (Summe Starts: " + datenDownload.start.countRestarted + ')');
        text.add("Ziel: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        text.add("URL: " + datenDownload.arr[DatenDownload.DOWNLOAD_URL]);
        SysMsg.sysMsg(text.toArray(new String[text.size()]));
    }

    private static void fertigmeldung(final DatenDownload datenDownload, final Start start, boolean abgebrochen) {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_BEEP))) {
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
        text.add("Startzeit: " + FormatterUtil.FORMATTER_HHmmss.format(start.startZeit));
        text.add("Endzeit: " + FormatterUtil.FORMATTER_HHmmss.format(new Datum().getTime()));
        text.add("Restarts: " + start.countRestarted);
        text.add("Dauer: " + start.startZeit.diffInSekunden() + " s");
        long dauer = start.startZeit.diffInMinuten();
        if (dauer == 0) {
            text.add("Dauer: <1 Min.");
        } else {
            text.add("Dauer: " + start.startZeit.diffInMinuten() + " Min");
        }
        if (datenDownload.art == DatenDownload.ART_DOWNLOAD) {
            if (start.mVInputStream != null) {
                text.add("Bytes gelesen: " + MVFilmSize.humanReadableByteCount(start.mVInputStream.getSumByte(), true));
                text.add("Bandbreite: " + DatenDownload.getTextBandbreite(start.mVInputStream.getSumBandwidth()));
            }
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
                SwingUtilities.invokeLater(() -> MVNotification.addNotification(datenDownload, start.status != Start.STATUS_ERR));
            }
        }
    }

    static void finalizeDownload(DatenDownload datenDownload, Start start /* wegen "datenDownload.start=null" beim stoppen */, HttpDownloadState state) {
        deleteIfEmpty(new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]));
        setFileSize(datenDownload);

        if (SystemInfo.isMacOSX() && state != HttpDownloadState.CANCEL) {
            //we don´t write comments if download was cancelled...
            if (Boolean.parseBoolean(datenDownload.arr[DatenDownload.DOWNLOAD_SPOTLIGHT])) {
                final SpotlightCommentWriter writer = new SpotlightCommentWriter();
                writer.writeComment(datenDownload);
            }
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

        if (SystemInfo.isMacOSX() && Daten.getInstance().getMediathekGui() != null) {
            Application.getApplication().requestUserAttention(false);
        }
    }

    /**
     * tatsächliche Dateigröße eintragen
     *
     * @param datenDownload {@link DatenDownload} with the info of the file
     */
    static void setFileSize(DatenDownload datenDownload) {
        try {
            final File testFile = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
            if (testFile.exists()) {
                final long length = testFile.length();
                if (length > 0) {
                    datenDownload.mVFilmSize.setSize(length);
                }
            }
        } catch (Exception ex) {
            Log.errorLog(461204780, "Fehler beim Ermitteln der Dateigröße: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]);
        }
    }

    static void notifyStartEvent(DatenDownload datenDownload) {
        Listener.notify(Listener.EREIGNIS_START_EVENT, StarterClass.class.getSimpleName());
        if (datenDownload != null) {
            if (datenDownload.quelle == DatenDownload.QUELLE_BUTTON) {
                Listener.notify(Listener.EREIGNIS_START_EVENT_BUTTON, StarterClass.class.getSimpleName());
            }
        }
    }

    // ********************************************
    // Hier wird dann gestartet
    // Ewige Schleife die die Downloads startet
    // ********************************************
    private class Starten extends Thread {

        private DatenDownload datenDownload;
        /**
         * The only {@link java.util.Timer} used for all {@link mediathek.controller.MVInputStream.BandwidthCalculationTask}
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
                    daten.getListeDownloadsButton().buttonStartsPutzen(); // Button Starts aus der Liste löschen
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
        private void startStarten(DatenDownload datenDownload) {
            datenDownload.start.startZeit = new Datum();
            Listener.notify(Listener.EREIGNIS_ART_DOWNLOAD_PROZENT, StarterClass.class.getName());
            Thread downloadThread;

            switch (datenDownload.art) {
                case DatenDownload.ART_PROGRAMM:
                    downloadThread = new ExternalProgramDownload(daten, datenDownload);
                    downloadThread.start();
                    break;
                case DatenDownload.ART_DOWNLOAD:
                    downloadThread = new DirectHttpDownload(daten, datenDownload, bandwidthCalculationTimer);
                    downloadThread.start();
                    break;
                default:
                    Log.errorLog(789356001, "StarterClass.Starten - Switch-default");
                    break;
            }
        }
    }
}
