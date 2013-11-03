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

import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import mediathek.controller.starter.Start;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.TModel;
import mediathek.tool.TModelDownload;
import msearch.daten.DatenFilm;

public class ListeDownloads extends LinkedList<DatenDownload> {

    private Daten ddaten;

    /**
     *
     * @param ddaten
     */
    public ListeDownloads(Daten ddaten) {
        this.ddaten = ddaten;
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BANDBREITE, ListeDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                setBanbreite();
            }
        });
    }

    //===================================
    // public
    //===================================
    public void sort() {
        Collections.<DatenDownload>sort(this);
    }

    public boolean addMitNummer(DatenDownload e) {
        boolean ret = super.add(e);
        listeNummerieren();
        return ret;
    }

    @Override
    public boolean add(DatenDownload d) {
        d.init();
        return super.add(d);
    }

    public synchronized void zurueckgestellteWiederAktivieren() {
        DatenDownload d = null;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            it.next().arr[DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR] = Boolean.FALSE.toString();
        }
    }

    public void reorder(int fromIndex, int toIndex) {
        // die Reihenfolge in der Liste ändern
        DatenDownload d = this.remove(fromIndex);
        this.add(toIndex, d);
    }

    public void addDatenDownloads(int index, LinkedList<DatenDownload> liste) {
        if (index > this.size()) {
            index = this.size();
        }
        for (DatenDownload d : liste) {
            this.add(index++, d);
        }
    }

    public void addDatenDownloads(LinkedList<DatenDownload> liste) {
        for (DatenDownload d : liste) {
            this.add(d);
        }
    }

    public synchronized void listePutzen() {
        // beim Programmende fertige Downloads löschen
        boolean gefunden = false;
        LinkedList<Start> s = Daten.listeDownloads.getStarts(Start.QUELLE_ALLE);
        Iterator<Start> it = s.iterator();
        while (it.hasNext()) {
            Start start = it.next();
            if (start.status >= Start.STATUS_FERTIG) {
                gefunden = true;
                delDownloadByUrl(start.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
        startsAufraeumen();
    }

    public synchronized boolean nochNichtFertigeDownloads() {
        // es wird nach noch nicht fertigen gestarteten Downloads gesucht
        boolean gefunden = false;
        LinkedList<Start> s = Daten.listeDownloads.getStarts(Start.QUELLE_ALLE);
        Iterator<Start> it = s.iterator();
        while (it.hasNext()) {
            Start start = it.next();
            if (start.status < Start.STATUS_FERTIG) {
                gefunden = true;
                break;
            }
        }
        return gefunden;
    }

    public synchronized DatenDownload downloadVorziehen(String url) {
        DatenDownload d = null;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            d = it.next();
            if (d.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                it.remove();
                this.addFirst(d);
                break;
            }
        }
        listeNummerieren();
        return d;
    }

    public synchronized DatenDownload getDownloadByUrl(String url) {
        DatenDownload ret = null;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                ret = d;
                break;
            }
        }
        return ret;
    }

    public synchronized DatenDownload getDownloadByNr(String nr) {
        DatenDownload ret = null;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.arr[DatenDownload.DOWNLOAD_NR_NR].equals(nr)) {
                ret = d;
                break;
            }
        }
        return ret;
    }

    public synchronized boolean delDownloadByUrl(String url) {
        boolean ret = false;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                it.remove();
                ret = true;
                break;
            }
        }
        if (ret) {
            listeNummerieren();
        }
        return ret;
    }

    public synchronized void getModel(TModelDownload tModel, boolean abos, boolean downloads) {
        tModel.setRowCount(0);
        Object[] object;
        DatenDownload download;
        if (this.size() > 0) {
            ListIterator<DatenDownload> iterator = this.listIterator(0);
            while (iterator.hasNext()) {
                download = iterator.next();
                if (download.istZurueckgestellt()) {
                    continue;
                }
                boolean istAbo = download.istAbo();
                if (abos && istAbo || downloads && !istAbo) {
                    object = new Object[DatenDownload.MAX_ELEM];
                    for (int i = 0; i < DatenDownload.MAX_ELEM; ++i) {
                        Start s = Daten.listeDownloads.getStart(download.arr[DatenDownload.DOWNLOAD_URL_NR]);
                        if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                            object[i] = "";
                        } else if (i == DatenDownload.DOWNLOAD_DATUM_NR) {
                            object[i] = download.datumFilm;
                        } else if (i == DatenDownload.DOWNLOAD_BANDBREITE_NR || i == DatenDownload.DOWNLOAD_PROGRESS_NR || i == DatenDownload.DOWNLOAD_RESTZEIT_NR) {
                            object[i] = s;
                        } else if (i == DatenDownload.DOWNLOAD_GROESSE_NR) {
                            object[i] = download.mVFilmSize;
                        } else if (i != DatenDownload.DOWNLOAD_FILM_NR_NR && i != DatenDownload.DOWNLOAD_URL_NR && !DatenDownload.anzeigen(i)) {
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
    }

    public synchronized void setModelProgress(TModelDownload tModel) {
        for (int i = 0; i < tModel.getRowCount(); ++i) {
            String nr = tModel.getValueAt(i, DatenDownload.DOWNLOAD_NR_NR).toString();
            DatenDownload d = Daten.listeDownloads.getDownloadByNr(nr);
            Start s = Daten.listeDownloads.getStart(d.arr[DatenDownload.DOWNLOAD_URL_NR]);
            if (s != null) {
                if (s.status == Start.STATUS_RUN) {
                    // wichtig ist nur "s", die anderen nur, damit sie geändert werden, werden im Cellrenderer berechnet
                    tModel.setValueAt(s, i, DatenDownload.DOWNLOAD_PROGRESS_NR);
                    tModel.setValueAt(s, i, DatenDownload.DOWNLOAD_BANDBREITE_NR);
                    tModel.setValueAt(s, i, DatenDownload.DOWNLOAD_RESTZEIT_NR);
                    tModel.setValueAt(d.mVFilmSize, i, DatenDownload.DOWNLOAD_GROESSE_NR);
                }
            }
        }
    }

    public synchronized void abosSuchen() {
        // in der Filmliste nach passenden Filmen suchen und 
        // in die Liste der Downloads eintragen
        boolean gefunden = false;
        DatenFilm film;
        DatenAbo abo;
        ListIterator<DatenFilm> itFilm;
        // prüfen ob in "alle Filme" oder nur "nach Blacklist" gesucht werden soll
        boolean checkWithBlackList = !Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET_NR])
                && Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUCH_ABO_NR]);
        if (checkWithBlackList) {
            itFilm = Daten.listeFilmeNachBlackList.listIterator();
        } else {
            itFilm = Daten.listeFilme.listIterator();
        }
        while (itFilm.hasNext()) {
            film = itFilm.next();
            abo = ddaten.listeAbo.getAboFuerFilm_schnell(film, true /*auch die Länge überprüfen*/);
            if (abo == null) {
                continue;
            }
            if (!abo.aboIstEingeschaltet()) {
                continue;
            }
            if (ddaten.erledigteAbos.urlPruefen(film.arr[DatenFilm.FILM_URL_NR])) {
                // ist schon im Logfile, weiter
                continue;
            }
            if (checkUrlExists(film.arr[DatenFilm.FILM_URL_NR])) {
                // haben wir schon in der Downloadliste
                continue;
            }
            //diesen Film in die Downloadliste eintragen
            abo.arr[DatenAbo.ABO_DOWN_DATUM_NR] = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
            //wenn nicht doppelt, dann in die Liste schreiben
            DatenPset pSet = ddaten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET_NR]);
            if (pSet != null) {
                if (!abo.arr[DatenAbo.ABO_PSET_NR].equals(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR])) {
                    abo.arr[DatenAbo.ABO_PSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
                }
                add(new DatenDownload(pSet, film, Start.QUELLE_ABO, abo, "", "", "" /*Aufloesung*/));
                gefunden = true;
            }
        }
//        ListIterator<DatenFilm> itFilm = Daten.listeFilme.listIterator();
//        while (itFilm.hasNext()) {
//            film = itFilm.next();
//            abo = ddaten.listeAbo.getAboFuerFilm(film, true /*auch die Länge überprüfen*/);
//            if (abo == null) {
//                continue;
//            }
//            if (!abo.aboIstEingeschaltet()) {
//                continue;
//            }
//            if (ddaten.erledigteAbos.urlPruefen(film.arr[DatenFilm.FILM_URL_NR])) {
//                // ist schon im Logfile, weiter
//                continue;
//            }
//            if (checkListe(film.arr[DatenFilm.FILM_URL_NR])) {
//                // haben wir schon in der Downloadliste
//                continue;
//            }
//            if (checkWithBlackList) {
//                if (!ddaten.listeBlacklist.checkBlackOkFilme_Downloads(film)) { // wenn Blacklist auch für Abos, dann ers mal da schauen
//                    continue;
//                }
//            }
//            //diesen Film in die Downloadliste eintragen
//            abo.arr[DatenAbo.ABO_DOWN_DATUM_NR] = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
//            //wenn nicht doppelt, dann in die Liste schreiben
//            DatenPset pSet = ddaten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET_NR]);
//            if (pSet != null) {
//                if (!abo.arr[DatenAbo.ABO_PSET_NR].equals(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR])) {
//                    abo.arr[DatenAbo.ABO_PSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
//                }
//                add(new DatenDownload(pSet, film, Start.QUELLE_ABO, abo, "", "", "" /*Aufloesung*/));
//                gefunden = true;
//            }
//
//        } //while
        if (gefunden) {
            listeNummerieren();
        }
    }

    public synchronized void abosLoschenWennNochNichtGestartet() {
        // es werden alle Abos (DIE NOCH NICHT GESTARTET SIND) aus der Liste gelöscht
        boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.istAbo()) {
                Start s = Daten.listeDownloads.getStart(d.arr[DatenDownload.DOWNLOAD_URL_NR]);
                if (s == null) {
                    // ansonsten läuft er schon
                    it.remove();
                    gefunden = true;
                }
            }
        }
        if (gefunden) {
            listeNummerieren();
        }
    }

    public void listeNummerieren() {
        int i = 0;
        ListIterator<DatenDownload> it = listIterator();
        while (it.hasNext()) {
            String str = String.valueOf(i++);
            while (str.length() < 3) {
                str = "0" + str;
            }
            it.next().arr[DatenDownload.DOWNLOAD_NR_NR] = str;
        }
    }

    public synchronized boolean checkUrlExists(String url) {
        //prüfen, ob der Film schon in der Liste ist, (manche Filme sind in verschiedenen Themen)
        boolean ret = false;
        DatenDownload d;
        ListIterator<DatenDownload> it = listIterator();
        while (it.hasNext()) {
            d = it.next();
            if (url.equals(d.arr[DatenDownload.DOWNLOAD_URL_NR])) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    // ###############################################################
    // Starts
    // ###############################################################
    public synchronized int getDownloadsWarten() {
        int ret = 0;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.datenDownload.getQuelle() == Start.QUELLE_ABO || s.datenDownload.getQuelle() == Start.QUELLE_DOWNLOAD) {
                    if (s.status == Start.STATUS_INIT) {
                        ++ret;
                    }
                }
            }
        }
        return ret;
    }

    public synchronized int getDownloadsLaufen() {
        int ret = 0;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.datenDownload.getQuelle() == Start.QUELLE_ABO || s.datenDownload.getQuelle() == Start.QUELLE_DOWNLOAD) {
                    if (s.status == Start.STATUS_RUN) {
                        ++ret;
                    }
                }
            }
        }
        return ret;
    }

    public synchronized int getStartsWaiting() {
        // liefert die Listengröße wenn noch nicht alle fertig
        // sonst wenn alle fertig: 0
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (it.next().start.status < Start.STATUS_FERTIG) {
                    return this.size();
                }
            }
        }
        return 0;
    }

    public synchronized Start getStart(String url) {
        Start ret = null;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                    ret = s;
                    break;
                }
            }
        }
        return ret;
    }

    public synchronized LinkedList<Start> getStarts(int quelle) {
        LinkedList<Start> ret = new LinkedList<Start>();
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.datenDownload.getQuelle() == quelle || quelle == Start.QUELLE_ALLE) {
                    ret.add(s);
                }
            }
        }
        return ret;
    }

    public synchronized Start getStartOrgUrl(String orgUrl) {
        Start ret = null;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL_NR].equals(orgUrl)) {
                    ret = s;
                    break;
                }
            }
        }
        return ret;
    }

    private synchronized void startsAufraeumen() {
        boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            Start start = it.next().start;
            if (start != null) {
                if (start.status >= Start.STATUS_FERTIG) {
                    it.remove();
                    gefunden = true;
                }
            }
        }
        if (gefunden) {
            notifyStartEvent();
        }
    }

    public synchronized void delAllStart() {
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                s.stoppen = true; /////s löschen????
            }
        }
        notifyStartEvent();
    }

    public synchronized void delStart(String url) {
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                    s.stoppen = true; /////s löschen????
                    notifyStartEvent();
                    break;
                }
            }
        }
    }

    public synchronized void delStart(ArrayList<String> url) {
        ListIterator<DatenDownload> it;
        if (url != null) {
            for (String u : url) {
                it = this.listIterator(0);
                while (it.hasNext()) {
                    Start s = it.next().start;
                    if (s != null) {
                        if (s.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(u)) {
                            s.stoppen = true; /////s löschen????
                            break;
                        }
                    }
                }
            }
        }
        notifyStartEvent();
    }

    public TModel getModelStarts(TModel model) {
        model.setRowCount(0);
        Object[] object;
        Start start;
        if (this.size() > 0) {
            Iterator<DatenDownload> iterator = iterator();
            int objLen = DatenDownload.MAX_ELEM + 1;
            object = new Object[objLen];
            while (iterator.hasNext()) {
                start = iterator.next().start;
                if (start != null) {
                    for (int k = 0; k < objLen; ++k) {
                        if (k < DatenDownload.MAX_ELEM) {
                            object[k] = start.datenDownload.arr[k];
                        } else {
                            if (start.datenDownload.istAbo()) {
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

    public synchronized void setBanbreite() {
        Start s;
        Iterator<DatenDownload> it = iterator();
        //erster Versuch, Start mit einem anderen Sender
        while (it.hasNext()) {
            s = it.next().start;
            if (s != null) {
                if (s.mVInputStream != null) {
                    s.mVInputStream.setBandbreite();
                }
            }
        }
    }

    public synchronized void buttonStartsPutzen() {
        // Starts durch Button die fertig sind, löschen
        boolean gefunden = false;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.datenDownload.getQuelle() == Start.QUELLE_BUTTON) {
                    if (s.status != Start.STATUS_RUN) {
                        // dann ist er fertig oder abgebrochen
                        it.remove();
                        gefunden = true;
                    }
                }
            }
        }
        if (gefunden) {
            notifyStartEvent(); // und dann bescheid geben
        }
    }

   public synchronized Start getNextStart() {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        Start ret = null;
        if (size() >= 0 && getDown() < Integer.parseInt(Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR])) {
            Start s = naechsterStart();
            if (s != null) {
                if (s.status == Start.STATUS_INIT) {
                    ret = s;
                }
            }
        }
        return ret;
    }

    private int getDown() {
        int ret = 0;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            Start s = it.next().start;
            if (s != null) {
                if (s.status == Start.STATUS_RUN) {
                    ++ret;
                }
            }
        }
        return ret;
    }

    private Start naechsterStart() {
        Start s;
        Iterator<DatenDownload> it = iterator();
        //erster Versuch, Start mit einem anderen Sender
        while (it.hasNext()) {
            s = it.next().start;
            if (s != null) {
                if (s.status == Start.STATUS_INIT) {
                    if (!maxSenderLaufen(s, 1)) {
                        return s;
                    }
                }
            }
        }
        if (Konstanten.MAX_SENDER_FILME_LADEN == 1) {
            //dann wars dass
            return null;
        }
        //zweiter Versuch, Start mit einem passenden Sender
        it = iterator();
        while (it.hasNext()) {
            s = it.next().start;
            if (s != null) {
                if (s.status == Start.STATUS_INIT) {
                    //int max = s.film.arr[Konstanten.FILM_SENDER_NR].equals(Konstanten.SENDER_PODCAST) ? Konstanten.MAX_PODCAST_FILME_LADEN : Konstanten.MAX_SENDER_FILME_LADEN;
                    if (!maxSenderLaufen(s, Konstanten.MAX_SENDER_FILME_LADEN)) {
                        return s;
                    }
                }
            }
        }
        return null;
    }

    private boolean maxSenderLaufen(Start s, int max) {
        //true wenn bereits die maxAnzahl pro Sender läuft
        try {
            int counter = 0;
            Start start;
            String host = getHost(s);
            Iterator<DatenDownload> it = iterator();
            while (it.hasNext()) {
                start = it.next().start;
                if (start != null) {
                    if (start.status == Start.STATUS_RUN
                            && getHost(start).equalsIgnoreCase(host)) {
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

    private String getHost(Start s) {
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

    private void notifyStartEvent() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_START_EVENT, this.getClass().getSimpleName());
    }
    // ################################################################
    // private
    // ################################################################
}
