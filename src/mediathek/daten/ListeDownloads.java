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
import java.util.ListIterator;
import javax.swing.JOptionPane;
import mediathek.controller.starter.Start;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVMessageDialog;
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

    public synchronized boolean addMitNummer(DatenDownload e) {
        boolean ret = super.add(e);
        listeNummerieren();
        return ret;
    }

//    public void addDatenDownloads(int index, LinkedList<DatenDownload> liste) {
//        if (index > this.size()) {
//            index = this.size();
//        }
//        for (DatenDownload d : liste) {
//            this.add(index++, d);
//        }
//    }
    public synchronized void addDatenDownloads(LinkedList<DatenDownload> liste) {
        for (DatenDownload d : liste) {
            this.add(d);
        }
    }

    public synchronized void zurueckgestellteWiederAktivieren() {
        DatenDownload d = null;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            it.next().arr[DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR] = Boolean.FALSE.toString();
        }
    }

//    public void reorder(int fromIndex, int toIndex) {
//        // die Reihenfolge in der Liste ändern
//        DatenDownload d = this.remove(fromIndex);
//        this.add(toIndex, d);
//    }
    public synchronized void listePutzen() {
        // fertige Downloads löschen
        boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.start != null) {
                if (d.start.status >= Start.STATUS_FERTIG) {
                    gefunden = true;
                    it.remove();
                }
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
            if (datenDownload.start.status >= Start.STATUS_FERTIG) {
                gefunden = true;
                this.remove(datenDownload);
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized boolean nochNichtFertigeDownloads() {
        // es wird nach noch nicht fertigen gestarteten Downloads gesucht
        boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload datenDownload = it.next();
            if (datenDownload.start != null) {
                if (datenDownload.start.status < Start.STATUS_FERTIG) {
                    gefunden = true;
                    break;
                }
            }
        }
        return gefunden;
    }

    public synchronized void downloadsVorziehen(String[] urls) {
        DatenDownload d;
        ListIterator<DatenDownload> it;
        for (String url : urls) {
            it = this.listIterator();
            while (it.hasNext()) {
                d = it.next();
                if (d.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                    it.remove();
                    this.addFirst(d);
                    break;
                }
            }
        }
        listeNummerieren();
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_REIHENFOLGE_DOWNLOAD, this.getClass().getSimpleName());
    }

//    public synchronized void downloadsNachTabelleSortieren(MVJTable mVJTable) {
//        if (mVJTable.getRowCount() == 0) {
//            return;
//        }
//        for (int i = 0; i < mVJTable.getRowCount(); ++i) {
//            String url = mVJTable.getModel().getValueAt(i, DatenDownload.DOWNLOAD_URL_NR).toString();
//            DatenDownload datenDownload = Daten.listeDownloads.getDownloadByUrl(url);
//            this.remove(datenDownload);
//            this.add(datenDownload);
//        }
//        listeNummerieren();
//        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_REIHENFOLGE_DOWNLOAD, this.getClass().getSimpleName());
//    }
    public synchronized DatenDownload getDownloadByUrl(String url) {
        DatenDownload ret = null;
        ListIterator<DatenDownload> it = this.listIterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                ret = d;
                break;
            }
        }
        return ret;
    }

    public synchronized void delDownloadByUrl(String url, boolean nurStart) {
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
                if (nurStart) {
                    datenDownload.mVFilmSize.reset();
                    datenDownload.start = null;
                } else {
                    it.remove();
                }
                listeNummerieren();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
                break;
            }
        }
    }

    public synchronized void delDownloadByUrl(ArrayList<String> url, boolean nurStart) {
        ListIterator<DatenDownload> it;
        boolean gefunden = false;
        if (url != null) {
            for (String u : url) {
                it = this.listIterator();
                while (it.hasNext()) {
                    DatenDownload datenDownload = it.next();
                    if (datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR].equals(u)) {
                        if (datenDownload.start != null) {
                            if (datenDownload.start.status < Start.STATUS_FERTIG) {
                                datenDownload.start.stoppen = true;
                            }
                        }
                        if (nurStart) {
                            datenDownload.mVFilmSize.reset();
                            datenDownload.start = null;
                        } else {
                            it.remove();
                        }
                        gefunden = true;
                        break;
                    }
                }
            }
        }
        if (gefunden) {
            listeNummerieren();
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized DatenDownload getDownloadUrlFilm(String urlFilm) {
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            DatenDownload datenDownload = it.next();
            if (datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL_NR].equals(urlFilm)) {
                return datenDownload;
            }
        }
        return null;
    }

    public synchronized void getModel(TModelDownload tModel, boolean abos, boolean downloads) {
        tModel.setRowCount(0);
        Object[] object;
        DatenDownload download;
        if (this.size() > 0) {
            ListIterator<DatenDownload> iterator = this.listIterator();
            while (iterator.hasNext()) {
                download = iterator.next();
                if (download.istZurueckgestellt()) {
                    continue;
                }
                boolean istAbo = download.istAbo();
                if (abos && istAbo || downloads && !istAbo) {
                    object = new Object[DatenDownload.MAX_ELEM];
                    for (int i = 0; i < DatenDownload.MAX_ELEM; ++i) {
                        if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                            object[i] = "";
                        } else if (i == DatenDownload.DOWNLOAD_DATUM_NR) {
                            object[i] = download.datumFilm;
                        } else if (i == DatenDownload.DOWNLOAD_RESTZEIT_NR) {
                            object[i] = download.getTextRestzeit();
                        } else if (i == DatenDownload.DOWNLOAD_BANDBREITE_NR) {
                            object[i] = download.getTextBandbreite();
                        } else if (i == DatenDownload.DOWNLOAD_PROGRESS_NR) {
                            object[i] = null;
                        } else if (i == DatenDownload.DOWNLOAD_GROESSE_NR) {
                            object[i] = download.mVFilmSize;
                        } else if (i == DatenDownload.DOWNLOAD_REF_NR) {
                            object[i] = download;
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
        for (int row = 0; row < tModel.getRowCount(); ++row) {
            DatenDownload datenDownload = (DatenDownload) tModel.getValueAt(row, DatenDownload.DOWNLOAD_REF_NR);
//            String url = tModel.getValueAt(i, DatenDownload.DOWNLOAD_URL_NR).toString();
//            DatenDownload datenDownload = Daten.listeDownloads.getDownloadByUrl(url);
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_RUN) {
                    // wichtig ist nur "s", die anderen nur, damit sie geändert werden, werden im Cellrenderer berechnet
                    tModel.setValueAt(datenDownload.getTextBandbreite(), row, DatenDownload.DOWNLOAD_BANDBREITE_NR);
                    tModel.setValueAt(datenDownload.getTextRestzeit(), row, DatenDownload.DOWNLOAD_RESTZEIT_NR);
                    tModel.setValueAt(null, row, DatenDownload.DOWNLOAD_PROGRESS_NR);
                    tModel.setValueAt(datenDownload.mVFilmSize, row, DatenDownload.DOWNLOAD_GROESSE_NR);
                }
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
        boolean checkWithBlackList = !Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET_NR])
                && Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUCH_ABO_NR]);
        if (checkWithBlackList) {
            itFilm = Daten.listeFilmeNachBlackList.listIterator();
        } else {
            itFilm = Daten.listeFilme.listIterator();
        }
        while (itFilm.hasNext()) {
            film = itFilm.next();
            abo = Daten.listeAbo.getAboFuerFilm_schnell(film, true /*auch die Länge überprüfen*/);
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
            } else if (parent != null) {
                // sonst sind wir evtl. nur in einer Konsole ohne X
                MVMessageDialog.showMessageDialog(parent, "Im Menü unter \"Datei->Einstellungen->Aufzeichnen und Abspielen\" ein Programm zum Aufzeichnen für Abos festlegen.",
                        "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                break;
            }
        }
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
                if (d.start == null) {
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

    public synchronized void listeNummerieren() {
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

    // ###############################################################
    // Starts
    // ###############################################################
    public synchronized int[] getStarts() {
        // liefert die Anzahl Starts die:
        // Anzahl, nicht gestarted sind, die laufen, die fertig sind: OK, die fertig sind: fehler
        // Downloads und Abos
        int[] ret = new int[]{0, 0, 0, 0, 0};
        DatenDownload datenDownload;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            datenDownload = it.next();
            if (!datenDownload.istZurueckgestellt()) {
                ++ret[0];
            }
            if (datenDownload.start != null) {
                if (datenDownload.getQuelle() == Start.QUELLE_ABO || datenDownload.getQuelle() == Start.QUELLE_DOWNLOAD) {
                    if (datenDownload.start.status == Start.STATUS_INIT) {
                        ++ret[1];
                    } else if (datenDownload.start.status == Start.STATUS_RUN) {
                        ++ret[2];
                    } else if (datenDownload.start.status == Start.STATUS_FERTIG) {
                        ++ret[3];
                    } else if (datenDownload.start.status == Start.STATUS_ERR) {
                        ++ret[4];
                    }
                }
            }
        }
        return ret;
    }

    public String getInfo() {
        String textLinks;
        // Text links: Zeilen Tabelle
        // nicht gestarted, laufen, fertig OK, fertig fehler
        int[] starts = getStarts();
        if (starts[0] == 1) {
            textLinks = "1 Download";
        } else {
            textLinks = starts[0] + " Downloads";
        }
        boolean print = false;
        for (int ii = 1; ii < starts.length; ++ii) {
            if (starts[ii] > 0) {
                print = true;
                break;
            }
        }
        if (print) {
            textLinks += ": ";
            if (starts[2] == 1) {
                textLinks += "1 läuft";
            } else {
                textLinks += starts[2] + " laufen";
            }
            if (starts[1] == 1) {
                textLinks += ", 1 wartet";
            } else {
                textLinks += ", " + starts[1] + " warten";
            }
            if (starts[3] > 0) {
                if (starts[3] == 1) {
                    textLinks += ", 1 fertig";
                } else {
                    textLinks += ", " + starts[3] + " fertig";
                }
            }
            if (starts[4] > 0) {
                if (starts[3] == 1) {
                    textLinks += ", 1 fehlerhaft";
                } else {
                    textLinks += ", " + starts[4] + " fehlerhaft";
                }
            }
            //textLinks += ")";
        }
        return textLinks;
    }

//    public synchronized int getStartsNotStarted() {
//        // liefert die Anzahl Starts die noch nicht gestarted sind, Status: init
//        // Downloads und Abos
//        int ret = 0;
//        DatenDownload datenDownload;
//        Iterator<DatenDownload> it = iterator();
//        while (it.hasNext()) {
//            datenDownload = it.next();
//            if (datenDownload.start != null) {
//                if (datenDownload.getQuelle() == Start.QUELLE_ABO || datenDownload.getQuelle() == Start.QUELLE_DOWNLOAD) {
//                    if (datenDownload.start.status == Start.STATUS_INIT) {
//                        ++ret;
//                    }
//                }
//            }
//        }
//        return ret;
//    }
//
//    public synchronized int getStartsRun() {
//        // liefert die Anzahl Starts die laufen, Status: run
//        // Downloads und Abos
//        int ret = 0;
//        DatenDownload datenDownload;
//        Iterator<DatenDownload> it = iterator();
//        while (it.hasNext()) {
//            datenDownload = it.next();
//            if (datenDownload.start != null) {
//                if (datenDownload.getQuelle() == Start.QUELLE_ABO || datenDownload.getQuelle() == Start.QUELLE_DOWNLOAD) {
//                    if (datenDownload.start.status == Start.STATUS_RUN) {
//                        ++ret;
//                    }
//                }
//            }
//        }
//        return ret;
//    }
    public synchronized int getStartsNotFinished() {
        // liefert die Anzahl Starts die noch anstehen, Status: "init" oder "run"
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

    public synchronized LinkedList<DatenDownload> getListteStartsNotFinished(int quelle) {
        // liefert alle gestarteten Downloads einer Quelle zB: Button
        LinkedList<DatenDownload> ret = new LinkedList<>();
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            DatenDownload datenDownload = it.next();
            if (datenDownload.start != null) {
                if (datenDownload.start.status < Start.STATUS_FERTIG) {
                    if (datenDownload.getQuelle() == quelle || quelle == Start.QUELLE_ALLE) {
                        ret.add(datenDownload);
                    }
                }
            }
        }
        return ret;
    }

    public synchronized TModel getModelStarts(TModel model) {
        model.setRowCount(0);
        Object[] object;
        if (this.size() > 0) {
            Iterator<DatenDownload> iterator = iterator();
            int objLen = DatenDownload.MAX_ELEM + 1;
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
                if (d.getQuelle() == Start.QUELLE_BUTTON) {
                    if (d.start.status >= Start.STATUS_FERTIG) {
                        // dann ist er fertig oder abgebrochen
                        it.remove();
                        gefunden = true;
                    }
                }
            }
        }
        if (gefunden) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized DatenDownload getNextStart() {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        DatenDownload ret = null;
        if (this.size() > 0 && getDown(Integer.parseInt(Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR]))) {
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
        if (Konstanten.MAX_SENDER_FILME_LADEN == 1) {
            //dann wars dass
            return null;
        }
        //zweiter Versuch, Start mit einem passenden Sender
        it = iterator();
        while (it.hasNext()) {
            DatenDownload datenDownload = it.next();
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_INIT) {
                    if (!maxSenderLaufen(datenDownload, Konstanten.MAX_SENDER_FILME_LADEN)) {
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
            Iterator<DatenDownload> it = iterator();
            while (it.hasNext()) {
                DatenDownload datenDownload = it.next();
                if (datenDownload.start != null) {
                    if (datenDownload.start.status == Start.STATUS_RUN
                            && getHost(datenDownload).equalsIgnoreCase(host)) {
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

    private synchronized void setBanbreite() {
        Start s;
        Iterator<DatenDownload> it = iterator();
        while (it.hasNext()) {
            s = it.next().start;
            if (s != null) {
                if (s.mVInputStream != null) {
                    s.mVInputStream.setBandbreite();
                }
            }
        }
    }

    private synchronized boolean checkUrlExists(String url) {
        //prüfen, ob der Film schon in der Liste ist, (manche Filme sind in verschiedenen Themen)
        ListIterator<DatenDownload> it = listIterator();
        while (it.hasNext()) {
            if (it.next().arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                return true;
            }
        }
        return false;
    }
}
