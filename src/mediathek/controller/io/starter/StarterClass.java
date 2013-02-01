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
package mediathek.controller.io.starter;

import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.LinkedList;
import mediathek.daten.DDaten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.TModel;

public class StarterClass {

    private DDaten ddaten;
    private ListeStarts listeStarts;
    private Starten starten = null;

    //===================================
    // Public
    //===================================
    public StarterClass(DDaten d) {
        ddaten = d;
        init();
    }

    public synchronized Start urlStarten(DatenPset pSet, DatenFilm ersterFilm) {
        // url mit dem Programm mit der Nr. starten (Button oder Doppelklick)
        // Quelle "Button" ist immer ein vom User gestarteter Film, also Quelle_Button!!!!!!!!!!!
        Start s = null;
        String url = ersterFilm.arr[DatenFilm.FILM_URL_NR];
        if (!url.equals("")) {
            s = new Start(new DatenDownload(pSet, ersterFilm, Start.QUELLE_BUTTON, null, "", ""));
            starten.startStarten(s);
            addStarts(s);
        }
        return s;
    }

    public Start urlVorziehen(String url) {
        // Starts mit der URL wird vorgezogen und startet als nächster
        return listeStarts.urlVorziehen(url);
    }

    public synchronized LinkedList<Start> getStarts(int quelle) {
        return listeStarts.getStarts(quelle);
    }

    public synchronized int getDownloadsWarten() {
        return listeStarts.getDownloadsWarten();
    }

    public synchronized int getDownloadsLaufen() {
        return listeStarts.getDownloadsLaufen();
    }

    public synchronized int getStartsWaiting() {
        // für "Auto", wenn alle abgearbeitet sind, dann fertig
        return listeStarts.getmax();
    }

    public synchronized TModel getModellStarts(TModel model) {
        return listeStarts.getModelStarts(model);

    }

    public synchronized void addStarts(Start start) {
        //add: Neues Element an die Liste anhängen
        listeStarts.addStarts(start);
    }

    public synchronized Start getStart(String url) {
        return listeStarts.getStart(url);
    }

    public synchronized void aufraeumen() {
        listeStarts.aufraeumen();
    }

    public synchronized void delAllStart() {
        // Alle Downloads werden abgebrochen
        listeStarts.delAllStart();
    }

    public synchronized void filmLoeschen(String url) {
        listeStarts.delStart(url);
    }

    // ===================================
    // Private
    // ===================================
    private void init() {
        listeStarts = new ListeStarts(ddaten);
        starten = new Starten();
        Thread startenThread = new Thread(starten);
        startenThread.setDaemon(true);
        startenThread.start();
    }

    private void notifyStartEvent() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, StarterClass.class.getSimpleName());
    }

    private void buttonStartsPutzen() {
        // Starts durch Button die fertig sind, löschen
        listeStarts.buttonStartsPutzen();
    }

    private Start getListe() {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        return listeStarts.getListe();
    }

    // ********************************************
    // Hier wird dann gestartet
    // Ewige Schleife die die Downloads startet
    // ********************************************
    private class Starten implements Runnable {

        Start start;

        @Override
        public synchronized void run() {
            while (true) {
                try {
                    while ((start = getListe()) != null) {
                        startStarten(start);
                        //alle 5 Sekunden einen Download starten
                        this.wait(5000);
                    }
                    buttonStartsPutzen(); // Button Starts aus der Liste löschen
                    this.wait(3000);
                } catch (Exception ex) {
                    Log.fehlerMeldung(613822015, Log.FEHLER_ART_PROG, "StarterClass.Starten.run", ex);
                }
            } //while(true)
        }

        private void startStarten(Start start) {
            start.datenDownload.startMelden(DatenDownload.PROGRESS_GESTARTET);
            switch (start.datenDownload.getArt()) {
                case Start.ART_PROGRAMM:
                    StartenProgramm startenProgrammn = new StartenProgramm(start);
                    new Thread(startenProgrammn).start();
                    break;
                case Start.ART_DOWNLOAD:
                    StartenDownload startenDownload = new StartenDownload(start);
                    new Thread(startenDownload).start();
                    break;
                default:
                    Log.fehlerMeldung(789356001, Log.FEHLER_ART_PROG, "StartetClass.startStarten", "StarterClass.Starten - Switch-default");
                    break;
            }
        }
    }

    private class StartenProgramm implements Runnable {

        Start start;
        RuntimeExec runtimeExec;
        File file;

        public StartenProgramm(Start s) {
            start = s;
            start.status = Start.STATUS_RUN;
            file = new File(start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
            notifyStartEvent();
            try {
                new File(start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]).mkdirs();
            } catch (Exception ex) {
                Log.fehlerMeldung(469365281, Log.FEHLER_ART_PROG, "StarterClass.StartenProgramm-1", ex);
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
                                } catch (InterruptedException e) {
                                }
                            }
                            break;
                        case stat_restart:
                            if (!start.datenDownload.isRestart()) {
                                // dann wars das
                                stat = stat_fertig_fehler;
                            } else {
                                if (filesize == -1) {
                                    //noch nichts geladen
                                    leeresFileLoeschen(file);
                                    if (file.exists()) {
                                        // dann bestehende Datei weitermachen
                                        filesize = file.length();
                                        stat = stat_start;
                                    } else {
                                        // counter prüfen und bei einem Maxwert abbrechen, sonst endlos
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
                            if (start.datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
                                //für die direkten Starts mit dem Button wars das dann
                                stat = stat_fertig_ok;
                            } else if (pruefen(start)) {
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
                Log.fehlerMeldung(395623710, Log.FEHLER_ART_PROG, "StarterClass.StartenProgramm-2", ex);
            }
            leeresFileLoeschen(file);
            fertigmeldung(start);
            start.datenDownload.startMelden(DatenDownload.PROGRESS_FERTIG);
            notifyStartEvent();
        }

        private boolean starten() {
            boolean ret = false;
            // die Reihenfolge: startcounter - startmeldung ist wichtig!
            start.startcounter++;
            startmeldung(start);
            runtimeExec = new RuntimeExec(start);
            start.process = runtimeExec.exec();
            if (start.process != null) {
                ret = true;
            }
            return ret;
        }
    }

    private class StartenDownload implements Runnable {

        Start start;

        public StartenDownload(Start s) {
            start = s;
            start.status = Start.STATUS_RUN;
            notifyStartEvent();
        }

        @Override
        public void run() {
            startmeldung(start);
            InputStream input;
            OutputStream destStream;
            try {
                int len;
                new File(start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]).mkdirs();
                URL feedUrl = new URL(start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                int maxLen = laenge(start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                int downLen = 0;
                input = feedUrl.openStream();
                byte[] buffer = new byte[1024];
                long p, pp = 0;
                destStream = new FileOutputStream(start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
                while ((len = input.read(buffer)) != -1 && !start.stoppen) {
                    downLen += len;
                    if (maxLen > 0) {
                        p = (downLen * (long) 1000) / maxLen;
                        // p muss zwischen 1 und 999 liegen
                        if (p == 0) {
                            p = DatenDownload.PROGRESS_GESTARTET;
                        }
                        if (p >= 1000) {
                            p = 999;
                        }
                        if (p != pp) {
                            start.datenDownload.startMelden((int) p);
                            pp = p;
                        }
                    }
                    destStream.write(buffer, 0, len);
                }
                input.close();
                destStream.close();
            } catch (Exception ex) {
                Log.fehlerMeldung(502039078, Log.FEHLER_ART_PROG, "StarterClass.StartenDownload-1", ex);
            }
            if (!start.stoppen) {
                if (start.datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
                    // direkter Start mit dem Button
                    start.status = Start.STATUS_FERTIG;
                } else if (pruefen(start)) {
                    //Anzeige ändern - fertig
                    start.status = Start.STATUS_FERTIG;
                } else {
                    //Anzeige ändern - bei Fehler fehlt der Eintrag
                    start.status = Start.STATUS_ERR;
                }
            }
            leeresFileLoeschen(new File(start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]));
            fertigmeldung(start);
            start.datenDownload.startMelden(DatenDownload.PROGRESS_FERTIG);
            notifyStartEvent();
        }
    }

    private void programmBeenden() {
        if (ddaten.nachDownloadShutDown) {
            // Sicherheitsabfrage, dann beenden
        }
    }

    private int laenge(String url) {
        int ret;
        try {
            URL u = new URL(url);
            ret = u.openConnection().getContentLength();
        } catch (Exception ex) {
            ret = -1;
            Log.fehlerMeldung(643298301, Log.FEHLER_ART_PROG, "StarterClass.StartenDownload.laenge", ex);
        }
        if (ret < 100) {
            // dann wars nix
            ret = -1;
        }
        return ret;
    }

    private boolean pruefen(Start start) {
        //prüfen ob der Downoad geklappt hat und die Datei existiert und eine min. Grüße hat
        boolean ret = false;
        File file = new File(start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        if (!file.exists()) {
            Log.fehlerMeldung(550236231, Log.FEHLER_ART_PROG, "StartetClass.pruefen-1", "Download fehlgeschlagen: Datei existiert nicht" + start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        } else if (file.length() < Konstanten.MIN_DATEI_GROESSE_KB * 1024) {
            Log.fehlerMeldung(795632500, Log.FEHLER_ART_PROG, "StartetClass.pruefen-2", "Download fehlgeschlagen: Datei zu klein" + start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        } else {
            if (start.datenDownload.istAbo()) {
                ddaten.erledigteAbos.zeileSchreiben(start.datenDownload.arr[DatenDownload.DOWNLOAD_THEMA_NR],
                        start.datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR],
                        start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
            }
            ret = true;
        }
        return ret;
    }

    private void leeresFileLoeschen(File file) {
        //prüfen ob die Datei existiert und eine min. Grüße hat, wenn nicht, dann löschen
        try {
            if (file.exists()) {
                // zum Wiederstarten/Aufräumen die leer/zu kleine Datei löschen, alles auf Anfang
                if (file.length() == 0) {
                    // zum Wiederstarten/Aufräumen die leer/zu kleine Datei löschen, alles auf Anfang
                    Log.systemMeldung(new String[]{"Restart/Aufräumen: leere Datei löschen", file.getAbsolutePath()});
                    file.delete();
                } else if (file.length() < Konstanten.MIN_DATEI_GROESSE_KB * 1024) {
                    Log.systemMeldung(new String[]{"Restart/Aufräumen: Zu kleine Datei löschen", file.getAbsolutePath()});
                    file.delete();

                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(795632500, Log.FEHLER_ART_PROG, "StartetClass.leeresFileLoeschen", "Fehler beim löschen" + file.getAbsolutePath());
        }
    }

    private void startmeldung(Start start) {
        ArrayList<String> text = new ArrayList<String>();
        boolean abspielen = start.datenDownload.getQuelle() == Start.QUELLE_BUTTON;
        if (abspielen) {
            text.add("Film starten");
        } else {
            if (start.startcounter > 1) {
                text.add("Download starten - Restart (Summe Starts: " + start.startcounter + ")");
            } else {
                text.add("Download starten");
            }
            text.add("Programmset: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET_NR]);
            text.add("Ziel: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        }
        text.add("URL: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
        if (start.datenDownload.getArt() == Start.ART_DOWNLOAD) {
            text.add(Start.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);
        }
        Log.systemMeldung(text.toArray(new String[]{}));
    }

    private void fertigmeldung(Start start) {
        ArrayList<String> text = new ArrayList<String>();
        boolean abspielen = start.datenDownload.getQuelle() == Start.QUELLE_BUTTON;
        if (abspielen) {
            text.add("Film fertig");
        } else {
            if (start.stoppen) {
                text.add("Download abgebrochen");
            } else if (start.status != Start.STATUS_ERR) {
                // dann ists gut
                text.add("Download ist fertig und hat geklappt");
            } else {
                text.add("Download ist fertig und war fehlerhaft");
            }
            text.add("Programmset: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET_NR]);
            text.add("Ziel: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        }
        text.add("URL: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
        if (start.datenDownload.getArt() == Start.ART_DOWNLOAD) {
            text.add(Start.ART_DOWNLOAD_TXT);
        } else {
            text.add("Programmaufruf: " + start.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_NR]);
        }
        Log.systemMeldung(text.toArray(new String[]{}));
    }
}
