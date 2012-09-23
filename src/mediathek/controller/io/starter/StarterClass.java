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
import java.util.Iterator;
import java.util.LinkedList;
import javax.swing.event.EventListenerList;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.TModel;

public class StarterClass {

    private DDaten ddaten;
    private boolean allesStop = false;
    private ListeStarts listeStarts;
    private Starten starten = null;
    private EventListenerList listeners = new EventListenerList();

    //===================================
    // Public
    //===================================
    /**
     * Neue Starter-Klasse inizialisieren
     *
     * @param d
     */
    public StarterClass(DDaten d) {
        ddaten = d;
        init();
    }

    private void init() {
        listeners = new EventListenerList();
        listeStarts = new ListeStarts(ddaten);
        starten = new Starten();
        Thread startenThread = new Thread(starten);
        startenThread.setDaemon(true);
        startenThread.start();
    }

    /**
     * @param open
     * @param ersterFilm
     * @return
     */
    public synchronized Starts urlStarten(DatenPset pSet, DatenFilm ersterFilm) {
        // url mit dem Programm mit der Nr. starten (Button oder Doppelklick)
        // Quelle ist immer ein vom User gestarteter Film, also Quelle_Button!!!!!!!!!!!
        Starts s = null;
        String url = ersterFilm.arr[DatenFilm.FILM_URL_NR];
        if (!url.equals("")) {
            s = new Starts(new DatenDownload(pSet, ersterFilm, Starts.QUELLE_BUTTON, null, "", ""));
            this.starten.startStarten(s);
            addStarts(s);
        }
        return s;
    }

    public synchronized Starts urlVorziehen(String url) {
        // Starts mit der URL wird vorgezogen und startet als nächster
        Starts s = null;
        Iterator<Starts> it = listeStarts.getIt();
        while (it.hasNext()) {
            s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                if (s.status < Starts.STATUS_RUN) {
                    // sonst bringts nichts mehr
                    it.remove();
                    listeStarts.addFirst(s);
                }
                break;
            }
        }
        return s;
    }

    public synchronized LinkedList<Starts> getStarts(int quelle) {
        LinkedList<Starts> ret = new LinkedList<Starts>();
        Iterator<Starts> it = listeStarts.getIt();
        while (it.hasNext()) {
            Starts s = it.next();
            if (s.datenDownload.getQuelle() == quelle || quelle == Starts.QUELLE_ALLE) {
                ret.add(s);
            }
        }
        return ret;
    }

    public synchronized int getDownloadsWarten() {
        int ret = 0;
        Iterator<Starts> it = listeStarts.getIt();
        while (it.hasNext()) {
            Starts s = it.next();
            if (s.datenDownload.getQuelle() == Starts.QUELLE_ABO || s.datenDownload.getQuelle() == Starts.QUELLE_DOWNLOAD) {
                if (s.status == Starts.STATUS_INIT) {
                    ++ret;
                }
            }
        }
        return ret;
    }

    public synchronized int getDownloadsLaufen() {
        int ret = 0;
        Iterator<Starts> it = listeStarts.getIt();
        while (it.hasNext()) {
            Starts s = it.next();
            if (s.datenDownload.getQuelle() == Starts.QUELLE_ABO || s.datenDownload.getQuelle() == Starts.QUELLE_DOWNLOAD) {
                if (s.status == Starts.STATUS_RUN) {
                    ++ret;
                }
            }
        }
        return ret;
    }

    public synchronized TModel getStarterModell(TModel model) {
        return listeStarts.getModel(model);

    }

    public synchronized void addStarts(Starts starts) {
        //add: Neues Element an die Liste anhängen
        allesStop = false;
        if (starts != null) {
            if (!listeStarts.contain(starts)) {
                listeStarts.add(starts);
                // gestartete Filme auch in die History eintragen
                ddaten.history.add(starts.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
            }
        }
        notifyStartEvent();
    }

    public synchronized int getState(String url) {
        int ret = 0;
        Iterator<Starts> it = listeStarts.getIt();
        while (it.hasNext()) {
            Starts s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                ret = s.status;
                break;
            }
        }
        return ret;
    }

    public synchronized Starts getStart(String url) {
        Starts ret = null;
        Iterator<Starts> it = listeStarts.getIt();
        while (it.hasNext()) {
            Starts s = it.next();
            if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                ret = s;
                break;
            }
        }
        return ret;
    }

    public synchronized void delStart(String url) {
        listeStarts.delStart(url);
    }

    public synchronized int getStartsWaiting() {
        // für "Auto", wenn alle abgearbeitet sind, dann fertig
        return listeStarts.getmax();
    }

    public synchronized void aufraeumen() {
        listeStarts.aufraeumen();
        notifyStartEvent();
    }

    /** Alle Downloads werden abgebrochen */
    public synchronized void abbrechen() {
        allesStop = true;
        listeStarts.delAlle();
        notifyStartEvent();
    }

    /** Alle Downloads die nicht laufen, löschen: Es wird nach dem laufenden Download abgebrochen */
    public synchronized void abbrechenNachFilm() {
        listeStarts.delRest();
        notifyStartEvent();
    }

    public synchronized void filmLoeschen(String url) {
        listeStarts.delStart(url);
        notifyStartEvent();
    }

    //===================================
    // Private
    //===================================
    private void notifyStartEvent() {
        Daten.notifyMediathekListener(ListenerMediathekView.EREIGNIS_START_EVENT, StarterClass.class.getSimpleName());
    }

    private Starts getListe() {
        /* get: erstes passendes Element der Liste zurückgeben oder null */
        /* versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird */
        Iterator<Starts> it;
        Starts ret = null;
        if (allesStop) {
            // nur die laufenden Starts löschen, damit die Anzeige der Podcasts stimmt
            it = listeStarts.getIt();
            while (it.hasNext()) {
                Starts s = it.next();
                if (s.status < Starts.STATUS_FERTIG) {
                    it.remove();
                }
            }
        } else {
            if (listeStarts.size() >= 0
                    && listeStarts.getDown() < Integer.parseInt(DDaten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR])) {
                Starts s = naechsterStart();
                if (s != null) {
                    if (s.status == Starts.STATUS_INIT) {
                        ret = s;
                    }
                }
            }
        }
        return ret;
    }

    private void buttonStartsPutzen() {
        // Starts durch Button die fertig sind, löschen
        boolean habsGetan = false;
        Iterator<Starts> it = listeStarts.getIt();
        while (it.hasNext()) {
            Starts s = it.next();
            if (s.datenDownload.getQuelle() == Starts.QUELLE_BUTTON) {
                if (s.status != Starts.STATUS_RUN) {
                    // dann ist er fertig oder abgebrochen
                    it.remove();
                    habsGetan = true;
                }
            }
        }
        if (habsGetan) {
            notifyStartEvent(); // und dann bescheid geben
        }
    }

    private Starts naechsterStart() {
        Starts s = null;
        Iterator<Starts> it = listeStarts.getIt();
        //erster Versuch, Start mit einem anderen Sender
        while (it.hasNext()) {
            s = it.next();
            if (s.status == Starts.STATUS_INIT) {
                if (!maxSenderLaufen(s, 1)) {
                    return s;
                }
            }
        }
        if (Konstanten.MAX_SENDER_FILME_LADEN == 1) {
            //dann wars dass
            return null;
        }
        //zweiter Versuch, Start mit einem passenden Sender
        it = listeStarts.getIt();
        while (it.hasNext()) {
            s = it.next();
            if (s.status == Starts.STATUS_INIT) {
                //int max = s.film.arr[Konstanten.FILM_SENDER_NR].equals(Konstanten.SENDER_PODCAST) ? Konstanten.MAX_PODCAST_FILME_LADEN : Konstanten.MAX_SENDER_FILME_LADEN;
                if (!maxSenderLaufen(s, Konstanten.MAX_SENDER_FILME_LADEN)) {
                    return s;
                }
            }
        }
        return null;
    }

    private boolean maxSenderLaufen(Starts s, int max) {
        //true wenn bereits die maxAnzahl pro Sender läuft
        try {
            int counter = 0;
            Starts start = null;
            String host = getHost(s);
            Iterator<Starts> it = listeStarts.getIt();
            while (it.hasNext()) {
                start = it.next();
                if (start.status == Starts.STATUS_RUN
                        && getHost(start).equalsIgnoreCase(host)) {
                    counter++;
                    if (counter >= max) {
                        return true;
                    }
                }
            }
            return false;
        } catch (Exception ex) {
            return false;
        }
    }

    private String getHost(Starts s) {
        String host = "";
        try {
            try {
                String uurl = s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR];
                // die funktion "getHost()" kann nur das Protokoll "http" ??!??
                if (uurl.startsWith("rtmpt:")) {
                    uurl = uurl.toLowerCase().replace("rtmpt:", "http:");
                }
                if (uurl.startsWith("rtmp:")) {
                    uurl = uurl.toLowerCase().replace("rtmp:", "http:");
                }
                if (uurl.startsWith("mms:")) {
                    uurl = uurl.toLowerCase().replace("mms:", "http:");
                }
                URL url = new URL(uurl);
                String tmp = url.getHost();
                if (tmp.contains(".")) {
                    host = tmp.substring(tmp.lastIndexOf("."));
                    tmp = tmp.substring(0, tmp.lastIndexOf("."));
                    if (tmp.contains(".")) {
                        host = tmp.substring(tmp.lastIndexOf(".") + 1) + host;
                    } else if (tmp.contains("/")) {
                        host = tmp.substring(tmp.lastIndexOf("/") + 1) + host;
                    } else {
                        host = "host";
                    }
                }
            } catch (Exception ex) {
                // für die Hosts bei denen das nicht klappt
                // Log.systemMeldung("getHost 1: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                host = "host";
            } finally {
                if (host == null) {
                    // Log.systemMeldung("getHost 2: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                    host = "host";
                }
                if (host.equals("")) {
                    // Log.systemMeldung("getHost 3: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                    host = "host";
                }
            }
        } catch (Exception ex) {
            // Log.systemMeldung("getHost 4: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
            host = "exception";
        }
        return host;
    }

    private class Starten implements Runnable {
        /* Ewige Schleife die die Downloads startet */

        Starts starts;

        @Override
        public synchronized void run() {
            while (true) {
                try {
                    while ((starts = getListe()) != null) {
                        startStarten(starts);
                        //alle 5 Sekunden einen Download starten
                        this.wait(5000);
                    }
                    buttonStartsPutzen(); // Button Starts aus der Liste löschen
                    this.wait(3000);
                } catch (Exception ex) {
                    Log.fehlerMeldung(613822015, "StarterClass.Starten.run", ex);
                }
            } //while(true)
        }

        public void startStarten(Starts starts) {
            starts.datenDownload.startMelden(DatenDownload.PROGRESS_GESTARTET);
            switch (starts.datenDownload.getArt()) {
                case Starts.ART_PROGRAMM:
                    StartenProgramm zdfStarten = new StartenProgramm(starts);
                    new Thread(zdfStarten).start();
                    break;
                case Starts.ART_DOWNLOAD:
                    StartenDonwnload podderStart = new StartenDonwnload(starts);
                    new Thread(podderStart).start();
                    break;
                default:
                    Log.fehlerMeldung(789356001, "StartetClass.startStarten", "StarterClass.Starten - Switch-default");
                    break;
            }
        }
    }

    private class StartenProgramm implements Runnable {

        Starts starts;
        RuntimeExec runtimeExec;

        public StartenProgramm(Starts s) {
            starts = s;
            starts.status = Starts.STATUS_RUN;
            notifyStartEvent();
            try {
                new File(starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]).mkdirs();
            } catch (Exception ex) {
                Log.fehlerMeldung(469365281, "StarterClass.StartenProgramm-1", ex);
            }
        }

        @Override
        public synchronized void run() {
            int k = 0;
            long filesize = -1;
            boolean restart = false;
            boolean startOk = false;
            try {
                if (starten()) {
                    restart = true; //los gehts
                }
                while (restart && !starts.stoppen) {
                    startOk = false;
                    restart = false;
                    while (!allesStop && !starts.stoppen) {
                        //hier läuft der Download bis zum Abbruch oder Ende
                        try {
                            k = starts.process.exitValue();
                            //fertig und tschüss
                            break;
                        } catch (Exception ex) {
                            try {
                                this.wait(2000);
                            } catch (InterruptedException e) {
                            }
                        }
                    }
                    if (allesStop || starts.stoppen) {
                        if (starts.process != null) {
                            starts.process.destroy();
                            //Anzeige ändern - fertig
                            if (starts.datenDownload.getQuelle() == Starts.QUELLE_BUTTON) {
                                //für die direkten Starts mit dem Button
                                starts.status = Starts.STATUS_FERTIG;
                            } else {
                                starts.status = Starts.STATUS_INIT;
                            }
                            //mit dem flvstreamer könnte man weitermachen, wennd das File noch da wäre
                            //new File(starts.film.arr[Konstanten.FILM_ZIEL_PFAD_DATEI_NR]).delete();
                        }
                    } else { //Exitvalue vom Prozess prüfen und ggf. neu Starten
                        if (k != 0) {
                            if (starts.datenDownload.isRestart()) {
                                //Download wieder starten
                                if (filesize == -1) {
                                    //erstes Mal
                                    File file = new File(starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
                                    if (file.exists()) {
                                        filesize = file.length();
                                        startOk = true;
                                    } else if (starts.startcounter < Starts.STARTCOUNTER_MAX) {
                                        //counter prüfen und bei einem Maxwert abbrechen, sonst endlos
                                        startOk = true;
                                    }
                                } else {
                                    //jetzt muss das File wachsen, sonst kein Restart
                                    File file = new File(starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
                                    if (file.exists()) {
                                        if (file.length() > filesize) {
                                            //nur weitermachen wenn die Datei tasächlich wächst
                                            startOk = true;
                                            filesize = file.length();
                                        }
                                    }
                                }
                                if (startOk && starten()) {
                                    restart = true;
                                } else {
                                    //Anzeige ändern - fertig mit Fehler
                                    starts.status = Starts.STATUS_ERR;
                                }
                            } else {
                                //Anzeige ändern - fertig
                                starts.status = Starts.STATUS_ERR;
                            }
                        } else if (starts.datenDownload.getQuelle() == Starts.QUELLE_BUTTON) {
                            //für die direkten Starts mit dem Button
                            starts.status = Starts.STATUS_FERTIG;
                        } else if (pruefen(starts)) {
                            //Anzeige ändern - fertig
                            starts.status = Starts.STATUS_FERTIG;
                        } else {
                            //Anzeige ändern - fehler
                            starts.status = Starts.STATUS_ERR;
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(395623710, "StarterClass.StartenProgramm-2", ex);
            }
            starts.datenDownload.startMelden(DatenDownload.PROGRESS_FERTIG);
            notifyStartEvent();
        }

        private boolean starten() {
            boolean ret = true;
            runtimeExec = new RuntimeExec(starts);
            starts.process = runtimeExec.exec();
            if (starts.process == null) {
                //Anzeige ändern - fehler - Programm wurde nicht gestartet
                starts.status = Starts.STATUS_ERR;
                ret = false;
            } else {
                starts.startcounter++;
            }
            return ret;
        }
    }

    private class StartenDonwnload implements Runnable {

        Starts starts;

        public StartenDonwnload(Starts s) {
            starts = s;
            starts.status = Starts.STATUS_RUN;
            notifyStartEvent();
        }

        @Override
        public void run() {
            Log.systemMeldung("----------------------------------------------------------------------------");
            Log.systemMeldung("| Download starten");
            Log.systemMeldung("| Programmset: " + starts.datenDownload.arr[DatenDownload.DOWNLOAD_PROGRAMMSET_NR]);
            Log.systemMeldung("| direkter Download");
            Log.systemMeldung("| URL: " + starts.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
            Log.systemMeldung("| Zieldatei: " + starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
            Log.systemMeldung("----------------------------------------------------------------------------");
            InputStream input;
            OutputStream destStream;
            try {
                int len;
                new File(starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]).mkdirs();
                URL feedUrl = new URL(starts.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                int maxLen = laenge(starts.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                int downLen = 0;
                input = feedUrl.openStream();
                byte[] buffer = new byte[1024];
                destStream = new FileOutputStream(starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
                while ((len = input.read(buffer)) != -1) {
                    downLen += buffer.length;
                    if (maxLen > 0) {
                        int p = (downLen * 100) / maxLen;
                        // p muss zwischen 1 und 99 liegen
                        if (p == 0) {
                            p = 1;
                        }
                        if (p >= 100) {
                            p = 99;
                        }
                        starts.datenDownload.startMelden(p);
                    }
                    destStream.write(buffer, 0, len);
                    if (allesStop || starts.stoppen) {
                        break;
                    }
                }
                input.close();
                destStream.close();
            } catch (Exception ex) {
                Log.fehlerMeldung(502039078, "StarterClass.StartenDonwnload-1", ex);
            }
            try {
                if (allesStop || starts.stoppen) {
                    //mit dem flvstreamer könnte man weitermachen, wennd das File noch da wäre
                    //new File(starts.film.arr[Konstanten.FILM_ZIEL_PFAD_DATEI_NR]).delete();
                } else {
                    if (starts.datenDownload.getQuelle() == Starts.QUELLE_BUTTON) {
                        // direkter Start mit dem Button
                        starts.status = Starts.STATUS_FERTIG;
                    } else if (pruefen(starts)) {
                        //Anzeige ändern - fertig
                        starts.status = Starts.STATUS_FERTIG;
                    } else {
                        //Anzeige ändern - bei Fehler fehlt der Eintrag
                        starts.status = Starts.STATUS_ERR;
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(904685832, "StarterClass.StartenDonwnload-2", ex);
            }
            starts.datenDownload.startMelden(DatenDownload.PROGRESS_FERTIG);
            notifyStartEvent();
        }
    }

    private int laenge(String url) {
        int ret = -1;
        try {
            URL u = new URL(url);
            ret = u.openConnection().getContentLength();
        } catch (Exception ex) {
            ret = -1;
            Log.fehlerMeldung(643298301, "StarterClass.StartenDonwnload.laenge", ex);
        }
        if (ret < 100) {
            // dann wars nix
            ret = -1;
        }
        return ret;
    }

    private boolean pruefen(Starts starts) {
        //prüfen ob der Downoad geklappt hat und die Datei existiert und eine min. Grüße hat
        boolean ret = false;
        File file = new File(starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        if (!file.exists()) {
            Log.fehlerMeldung(550236231, "StartetClass.pruefen-1", "Download fehlgeschlagen: " + starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        } else if (file.length() < Konstanten.MIN_DATEI_GROESSE_KB * 1024) {
            Log.fehlerMeldung(795632500, "StartetClass.pruefen-2", "Download fehlgeschlagen: " + starts.datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        } else {
            if (starts.datenDownload.istAbo()) {
                ddaten.erledigteAbos.zeileSchreiben(starts.datenDownload.arr[DatenDownload.DOWNLOAD_THEMA_NR],
                        starts.datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR],
                        starts.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
            }
            ret = true;
        }
        return ret;
    }
}
