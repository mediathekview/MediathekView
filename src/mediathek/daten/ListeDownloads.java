/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import java.awt.Frame;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import javax.swing.JOptionPane;
import mediathek.controller.starter.Start;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.TModel;
import mediathek.tool.TModelDownload;
import msearch.daten.DatenFilm;

public class ListeDownloads extends LinkedList<DatenDownload> {

    private final Daten daten;
    private final LinkedList<DatenDownload> aktivDownloads = new LinkedList<>();

    public ListeDownloads(Daten daten_) {
        this.daten = daten_;
    }

    //===================================
    // public
    //===================================
    public void sort() {
        Collections.sort(this);
    }

    public synchronized boolean addMitNummer(DatenDownload e) {
        boolean ret = super.add(e);
        listeNummerieren();
        return ret;
    }

    public synchronized void zurueckgestellteWiederAktivieren() {
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            it.next().arr[DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR] = Boolean.FALSE.toString();
        }
    }

    public synchronized void filmEintragen() {
        // bei einmal Downloads nach einem Programmstart/Neuladen der Filmliste
        // den Film wieder eintragen
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.film == null) {
                d.film = Daten.listeFilme.getFilmByUrl_klein_hoch_hd(d.arr[DatenDownload.DOWNLOAD_URL_NR]);
            }
        }
    }

    public synchronized void listePutzen() {
        // fertige Downloads löschen
        // fehlerhafte zurücksetzen
        boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.start == null) {
                continue;
            }
            if (d.start.status == Start.STATUS_FERTIG) {
                // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                it.remove();
                gefunden = true;
            } else if (d.start.status == Start.STATUS_ERR) {
                // fehlerhafte werden zurückgesetzt
                d.resetDownload();
                gefunden = true;
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized void listePutzen(DatenDownload datenDownload) {
        // fertigen Download löschen
        boolean gefunden = false;
        if (datenDownload.start != null) {
            if (datenDownload.start.status == Start.STATUS_FERTIG) {
                // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                remove(datenDownload);
                gefunden = true;
            } else if (datenDownload.start.status == Start.STATUS_ERR) {
                // fehlerhafte werden zurückgesetzt
                datenDownload.resetDownload();
                gefunden = true;
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized void abosPutzen() {
        // fehlerhafte und nicht gestartete löschen
        boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.isInterrupted()) {
                // guter Rat teuer was da besser wäre??
                // wird auch nach dem Neuladen der Filmliste aufgerufen: also Finger weg
                continue;
            }
            if (!d.istAbo()) {
                continue;
            }
            if (d.start == null) {
                // noch nicht gestartet
                it.remove();
                gefunden = true;
            } else if (d.start.status == Start.STATUS_ERR) {
                // fehlerhafte
                d.resetDownload();
                gefunden = true;
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized int nochNichtFertigeDownloads() {
        // es wird nach noch nicht fertigen gestarteten Downloads gesucht
        int ret = 0;
        for (DatenDownload download : this) {
            if (download.start != null) {
                if (download.start.status < Start.STATUS_FERTIG) {
                    ++ret;
                }
            }
        }
        return ret;
    }

    public synchronized void downloadsVorziehen(ArrayList<DatenDownload> download) {
        for (DatenDownload datenDownload : download) {
            this.remove(datenDownload);
            this.addFirst(datenDownload);
        }
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_REIHENFOLGE_DOWNLOAD, this.getClass().getSimpleName());
    }

    public synchronized DatenDownload getDownloadByUrl(String url) {
        DatenDownload ret = null;
        for (DatenDownload download : this) {
            if (download.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                ret = download;
                break;
            }
        }
        return ret;
    }

    public synchronized void delDownloadButton(String url) {
        ListIterator<DatenDownload> it = this.listIterator();
        DatenDownload datenDownload;
        while (it.hasNext()) {
            datenDownload = it.next();
            if (datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                if (datenDownload.start != null) {
                    if (datenDownload.start.status < Start.STATUS_FERTIG) {
                        datenDownload.start.stoppen = true;
                    }
                }
                datenDownload.mVFilmSize.reset();
                datenDownload.start = null;
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
                break;
            }
        }
    }

    public synchronized void downloadAbbrechen(ArrayList<DatenDownload> download) {
        boolean gefunden = false;
        if (download != null) {
            for (DatenDownload down : download) {
                if (this.contains(down)) {
                    // nur dann ist er in der Liste
                    if (down.start != null) {
                        if (down.start.status < Start.STATUS_FERTIG) {
                            down.start.stoppen = true;
                        }
                        if (down.start.status == Start.STATUS_RUN) {
                            down.interrupt();
                        }
                    }
                    down.resetDownload();
                    gefunden = true;
                }
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, this.getClass().getSimpleName());
        }
    }

    public synchronized void downloadLoeschen(ArrayList<DatenDownload> download) {
        boolean gefunden = false;
        if (download != null) {
            for (DatenDownload down : download) {
                if (down.start != null) {
                    if (down.start.status < Start.STATUS_FERTIG) {
                        down.start.stoppen = true;
                    }
                }
                if (remove(down)) {
                    gefunden = true;
                }
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized DatenDownload getDownloadUrlFilm(String urlFilm) {
        for (DatenDownload datenDownload : this) {
            if (datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL_NR].equals(urlFilm)) {
                return datenDownload;
            }
        }
        return null;
    }

    public synchronized void getModel(TModelDownload tModel, boolean abos, boolean downloads) {
        Object[] object;
        tModel.setRowCount(0);
        for (DatenDownload download : this) {
            if (download.istZurueckgestellt()) {
                continue;
            }
            boolean istAbo = download.istAbo();
            if (abos && istAbo || downloads && !istAbo) {
                object = new Object[DatenDownload.MAX_ELEM];
                for (int i = 0; i < DatenDownload.MAX_ELEM; ++i) {
                    if (i == DatenDownload.DOWNLOAD_NR_NR) {
                        object[i] = download.nr;
                    } else if (i == DatenDownload.DOWNLOAD_FILM_NR_NR) {
                        if (download.film != null) {
                            object[i] = download.film.nr;
                        } else {
                            object[i] = 0;
                        }
                    } else if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR
                            || i == DatenDownload.DOWNLOAD_UNTERBROCHEN_NR
                            || i == DatenDownload.DOWNLOAD_SPOTLIGHT_NR
                            || i == DatenDownload.DOWNLOAD_INFODATEI_NR
                            || i == DatenDownload.DOWNLOAD_SUBTITLE_NR
                            || i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR) {
                        object[i] = "";
                    } else if (i == DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER_NR) {
                    	object[i] = download.isDownloadManager();
                    } else if (i == DatenDownload.DOWNLOAD_DATUM_NR) {
                        object[i] = download.datumFilm;
                    } else if (i == DatenDownload.DOWNLOAD_RESTZEIT_NR) {
                        object[i] = download.getTextRestzeit();
                    } else if (i == DatenDownload.DOWNLOAD_BANDBREITE_NR) {
                        object[i] = download.getTextBandbreite();
                    } else if (i == DatenDownload.DOWNLOAD_PROGRESS_NR) {
                        object[i] = setProgress(download);
                    } else if (i == DatenDownload.DOWNLOAD_GROESSE_NR) {
                        object[i] = download.mVFilmSize;
                    } else if (i == DatenDownload.DOWNLOAD_REF_NR) {
                        object[i] = download;
                    } else if (i != DatenDownload.DOWNLOAD_URL_NR && !DatenDownload.anzeigen(i)) {
                        // Filmnr und URL immer füllen, egal ob angezeigt
                        object[i] = "";
                    } else {
                        object[i] = download.arr[i];
                    }
                }
                tModel.addRow(object);
            }
        }
    }

    private String setProgress(DatenDownload download) {
        if (download.start != null) {
            if (1 < download.start.percent && download.start.percent < Start.PROGRESS_FERTIG) {
                String s = Double.toString(download.start.percent / 10.0) + "%";
                while (s.length() < 5) {
                    s = "0" + s;
                }
                return s;
            } else {
                return Start.getTextProgress(download);
            }
        } else {
            return "";
        }
    }

    @SuppressWarnings("unchecked")
    public synchronized void setModelProgress(TModelDownload tModel) {
        ListIterator<List> it = tModel.getDataVector().listIterator();
        int row = 0;
        while (it.hasNext()) {
            List l = it.next();
            DatenDownload datenDownload = (DatenDownload) l.get(DatenDownload.DOWNLOAD_REF_NR);
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_RUN) {
                    tModel.setValueAt(datenDownload.getTextRestzeit(), row, DatenDownload.DOWNLOAD_RESTZEIT_NR);
                    tModel.setValueAt(datenDownload.getTextBandbreite(), row, DatenDownload.DOWNLOAD_BANDBREITE_NR);
                    tModel.setValueAt(setProgress(datenDownload), row, DatenDownload.DOWNLOAD_PROGRESS_NR);
                    tModel.setValueAt(datenDownload.mVFilmSize, row, DatenDownload.DOWNLOAD_GROESSE_NR);
                }
            }
            ++row;
        }
    }

    @SuppressWarnings("unchecked")
    public synchronized void setModelProgressAlleStart(TModelDownload tModel) {
        for (List l : (Iterable<List>) tModel.getDataVector()) {
            DatenDownload datenDownload = (DatenDownload) l.get(DatenDownload.DOWNLOAD_REF_NR);
            if (datenDownload.start != null) {
                l.set(DatenDownload.DOWNLOAD_RESTZEIT_NR, datenDownload.getTextRestzeit());
                l.set(DatenDownload.DOWNLOAD_BANDBREITE_NR, datenDownload.getTextBandbreite());
                l.set(DatenDownload.DOWNLOAD_PROGRESS_NR, setProgress(datenDownload));
                l.set(DatenDownload.DOWNLOAD_GROESSE_NR, datenDownload.mVFilmSize);
                l.set(DatenDownload.DOWNLOAD_UNTERBROCHEN_NR, null);
            }
        }
    }

    public synchronized void abosSuchen(Frame parent) {
        // in der Filmliste nach passenden Filmen suchen und 
        // in die Liste der Downloads eintragen
        boolean gefunden = false;
        DatenFilm film;
        DatenAbo abo;
        ListIterator<DatenFilm> itFilm;
        // prüfen ob in "alle Filme" oder nur "nach Blacklist" gesucht werden soll
        boolean checkWithBlackList = Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_BLACKLIST_AUCH_ABO));
        itFilm = Daten.listeFilme.listIterator();
        while (itFilm.hasNext()) {
            film = itFilm.next();
            abo = Daten.listeAbo.getAboFuerFilm_schnell(film, true /*auch die Länge überprüfen*/);
            if (abo == null) {
                // dann gibts dafür kein Abo
                continue;
            }
            if (!abo.aboIstEingeschaltet()) {
                continue;
            }
            if (checkWithBlackList) {
                //Blacklist auch bei Abos anwenden
                if (!Daten.listeBlacklist.checkBlackOkFilme_Downloads(film)) {
                    continue;
                }
            }
            if (daten.erledigteAbos.urlPruefen(film.getUrlHistory())) {
                // ist schon mal geladen worden
                continue;
            }
            DatenPset pSet = Daten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET_NR]);
            if (pSet != null) {
                // mit der tatsächlichen URL prüfen, ob die URL schon in der Downloadliste ist
                String urlDownload = film.getUrlFuerAufloesung(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG_NR]);
                if (checkUrlExists(urlDownload)) {
                    // haben wir schon in der Downloadliste
                    continue;
                }
                //diesen Film in die Downloadliste eintragen
                abo.arr[DatenAbo.ABO_DOWN_DATUM_NR] = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
                if (!abo.arr[DatenAbo.ABO_PSET_NR].equals(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR])) {
                    // nur den Namen anpassen, falls geändert
                    abo.arr[DatenAbo.ABO_PSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
                }
                //dann in die Liste schreiben
                add(new DatenDownload(pSet, film, DatenDownload.QUELLE_ABO, abo, "", "", "" /*Aufloesung*/));
                gefunden = true;
            } else if (parent != null) {
                // sonst sind wir evtl. nur in einer Konsole ohne X
                MVMessageDialog.showMessageDialog(parent, "Im Menü unter \"Datei->Einstellungen->Set bearbeiten\" ein Programm zum Aufzeichnen für Abos festlegen.",
                        "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                break;
            }
        }
        if (gefunden) {
            listeNummerieren();
        }
    }

    public synchronized void listeNummerieren() {
        int i = 1;
        for (DatenDownload datenDownload : this) {
            datenDownload.nr = i++;
        }
    }

    public synchronized int[] getStarts() {
        // liefert die Anzahl Starts die:
        // Anzahl, Anz-Abo, Anz-Down, nicht gestarted, laufen, fertig OK, fertig fehler
        // Downloads und Abos

        int[] ret = new int[]{0, 0, 0, 0, 0, 0, 0};
        for (DatenDownload download : this) {
            if (!download.istZurueckgestellt()) {
                ++ret[0];
            }
            if (download.istAbo()) {
                ++ret[1];
            } else {
                ++ret[2];
            }
            if (download.start != null) {
                //final int quelle = download.getQuelle();
                if (download.quelle == DatenDownload.QUELLE_ABO || download.quelle == DatenDownload.QUELLE_DOWNLOAD) {
                    switch (download.start.status) {
                        case Start.STATUS_INIT:
                            ++ret[3];
                            break;

                        case Start.STATUS_RUN:
                            ++ret[4];
                            break;

                        case Start.STATUS_FERTIG:
                            ++ret[5];
                            break;

                        case Start.STATUS_ERR:
                            ++ret[6];
                            break;
                    }
                }
            }
        }
        return ret;
    }

    /**
     * Return the number of Starts, which are queued in state INIT or RUN.
     *
     * @return number of queued Starts.
     */
    public synchronized int getNumberOfStartsNotFinished() {
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.status < Start.STATUS_FERTIG) {
                    return this.size();
                }
            }
        }
        return 0;
    }

    /**
     * Return the maximum time of all running starts until finish.
     *
     * @return The time in SECONDS.
     */
    public synchronized long getMaximumFinishTimeOfRunningStarts() {
        long rem = 0;
        for (DatenDownload d : this) {
            Start s = d.start;
            if (s != null) {
                if (s.status < Start.STATUS_FERTIG) {
                    rem = Math.max(rem, s.restSekunden);
                }
            }
        }

        return rem;
    }

    /**
     * Return a List of all not yet finished downloads.
     *
     * @param quelle Use QUELLE_XXX constants from {@link mediathek.controller.starter.Start}.
     * @return A list with all download objects.
     */
    public synchronized LinkedList<DatenDownload> getListOfStartsNotFinished(int quelle) {
        aktivDownloads.clear();
        for (DatenDownload download : this) {
            if (download.start != null) {
                if (download.start.status < Start.STATUS_FERTIG) {
                    if (quelle == DatenDownload.QUELLE_ALLE || download.quelle == quelle) {
                        aktivDownloads.add(download);
                    }
                }
            }
        }
        return aktivDownloads;
    }

    public synchronized TModel getModelStarts(TModel model) {
        model.setRowCount(0);
        Object[] object;

        if (!this.isEmpty()) {
            Iterator<DatenDownload> iterator = iterator();
            final int objLen = DatenDownload.MAX_ELEM + 1;
            object = new Object[objLen];
            while (iterator.hasNext()) {
                DatenDownload datenDownload = iterator.next();
                if (datenDownload.start != null) {
                    for (int k = 0; k < objLen; ++k) {
                        if (k < DatenDownload.MAX_ELEM) {
                            object[k] = datenDownload.arr[k];
                        } else {
                            if (datenDownload.istAbo()) {
                                object[k] = "Abo";
                            } else {
                                object[k] = "";
                            }
                        }
                    }
                }
                model.addRow(object);
            }
        }
        return model;
    }

    public synchronized void buttonStartsPutzen() {
        // Starts durch Button die fertig sind, löschen
        boolean gefunden = false;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.start != null) {
                if (d.quelle == DatenDownload.QUELLE_BUTTON) {
                    if (d.start.status >= Start.STATUS_FERTIG) {
                        // dann ist er fertig oder abgebrochen
                        it.remove();
                        gefunden = true;
                    }
                }
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT_BUTTON, this.getClass().getSimpleName());
        }
    }

    public synchronized DatenDownload getNextStart() {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        DatenDownload ret = null;
        if (this.size() > 0 && getDown(Integer.parseInt(Daten.mVConfig.get(MVConfig.SYSTEM_MAX_DOWNLOAD)))) {
            DatenDownload datenDownload = naechsterStart();
            if (datenDownload != null) {
                if (datenDownload.start != null) {
                    if (datenDownload.start.status == Start.STATUS_INIT) {
                        ret = datenDownload;
                    }
                }
            }
        }
        return ret;
    }

    // ################################################################
    // private
    // ################################################################
    private boolean getDown(int max) {
        int count = 0;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.status == Start.STATUS_RUN) {
                    ++count;
                    if (count >= max) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    private DatenDownload naechsterStart() {
        Iterator<DatenDownload> it = iterator();
        //erster Versuch, Start mit einem anderen Sender
        while (it.hasNext()) {
            DatenDownload datenDownload = it.next();
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_INIT) {
                    if (!maxSenderLaufen(datenDownload, 1)) {
                        return datenDownload;
                    }
                }
            }
        }

        int maxProSender = Konstanten.MAX_SENDER_FILME_LADEN;
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_MAX_1_DOWNLOAD_PRO_SERVER))) {
            // dann darf nur ein Download pro Server gestartet werden
            maxProSender = 1;
        }

        //zweiter Versuch, Start mit einem passenden Sender
        it = iterator();
        while (it.hasNext()) {
            DatenDownload datenDownload = it.next();
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_INIT) {
                    if (!maxSenderLaufen(datenDownload, maxProSender)) {
                        return datenDownload;
                    }
                }
            }
        }
        return null;
    }

    private boolean maxSenderLaufen(DatenDownload d, int max) {
        //true wenn bereits die maxAnzahl pro Sender läuft
        try {
            int counter = 0;
            String host = getHost(d);
            for (DatenDownload download : this) {
                if (download.start != null) {
                    if (download.start.status == Start.STATUS_RUN
                            && getHost(download).equalsIgnoreCase(host)) {
                        counter++;
                        if (counter >= max) {
                            return true;
                        }
                    }
                }
            }
            return false;
        } catch (Exception ex) {
            return false;
        }
    }

    private String getHost(DatenDownload datenDownload) {
        String host = "";
        try {
            try {
                String uurl = datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR];
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
                    host = tmp.substring(tmp.lastIndexOf('.'));
                    tmp = tmp.substring(0, tmp.lastIndexOf('.'));
                    if (tmp.contains(".")) {
                        host = tmp.substring(tmp.lastIndexOf('.') + 1) + host;
                    } else if (tmp.contains("/")) {
                        host = tmp.substring(tmp.lastIndexOf('/') + 1) + host;
                    } else {
                        host = "host";
                    }
                }
            } catch (Exception ex) {
                // für die Hosts bei denen das nicht klappt
                // Log.systemMeldung("getHost 1: " + s.download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                host = "host";
            } finally {
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

    private synchronized boolean checkUrlExists(String url) {
        //prüfen, ob der Film schon in der Liste ist, (manche Filme sind in verschiedenen Themen)
        for (DatenDownload download : this) {
            if (download.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                return true;
            }
        }
        return false;
    }
}
