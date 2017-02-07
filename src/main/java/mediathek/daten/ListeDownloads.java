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

import mSearch.daten.DatenFilm;
import mSearch.tool.Listener;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.controller.starter.Start;
import mediathek.gui.dialog.DialogAboNoSet;
import mediathek.tool.FormatterUtil;
import mediathek.tool.TModel;
import mediathek.tool.TModelDownload;

import javax.swing.*;
import java.net.URL;
import java.util.*;
import java.util.stream.Collectors;

@SuppressWarnings("serial")
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

    //    public synchronized void zurueckgestellteWiederAktivieren() {
    //        this.parallelStream().forEach(d -> d.arr[DatenDownload.DOWNLOAD_ZURUECKGESTELLT] = Boolean.FALSE.toString());
    //    }
    public synchronized void filmEintragen() {
        // bei einmal Downloads nach einem Programmstart/Neuladen der Filmliste
        // den Film wieder eintragen
        SysMsg.sysMsg("Filme in Downloads eintragen");
        this.stream().filter(d -> d.film == null)
                .forEach(d ->
                {
                    d.film = daten.getListeFilme().getFilmByUrl_klein_hoch_hd(d.arr[DatenDownload.DOWNLOAD_URL]);
                    d.setGroesseFromFilm();
                });
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
            Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
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
            Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized void abosAuffrischen() {
        // fehlerhafte und nicht gestartete löschen, wird nicht gemeldet ob was gefunden wurde
        //boolean gefunden = false;
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.isInterrupted()) {
                // guter Rat teuer was da besser wäre??
                // wird auch nach dem Neuladen der Filmliste aufgerufen: also Finger weg
                d.setGroesseFromFilm();//bei den Abgebrochenen wird die tatsächliche Dateigröße angezeigt
                continue;
            }
            if (!d.istAbo()) {
                continue;
            }
            if (d.start == null) {
                // noch nicht gestartet
                it.remove();
                //gefunden = true;
            } else if (d.start.status == Start.STATUS_ERR) {
                // fehlerhafte
                d.resetDownload();
                //gefunden = true;
            }
        }
//        if (gefunden) {
//            Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
//        }
        this.forEach(d -> d.arr[DatenDownload.DOWNLOAD_ZURUECKGESTELLT] = Boolean.FALSE.toString());
    }

    public synchronized int nochNichtFertigeDownloads() {
        // es wird nach noch nicht fertigen gestarteten Downloads gesucht
        int ret = 0;
        for (DatenDownload download : this) {
            if (download.runNotFinished()) {
                ++ret;
            }
        }
        return ret;
    }

    public synchronized void downloadsVorziehen(ArrayList<DatenDownload> download) {
        for (DatenDownload datenDownload : download) {
            this.remove(datenDownload);
            this.addFirst(datenDownload);
        }
        Listener.notify(Listener.EREIGNIS_REIHENFOLGE_DOWNLOAD, this.getClass().getSimpleName());
    }

    public synchronized DatenDownload getDownloadByUrl(String url) {
        DatenDownload ret = null;
        for (DatenDownload download : this) {
            if (download.arr[DatenDownload.DOWNLOAD_URL].equals(url)) {
                ret = download;
                break;
            }
        }
        return ret;
    }

    public synchronized void delDownloadButton(String url) {
        Iterator<DatenDownload> it = this.iterator();
        DatenDownload datenDownload;
        while (it.hasNext()) {
            datenDownload = it.next();
            if (datenDownload.arr[DatenDownload.DOWNLOAD_URL].equals(url)) {
                if (datenDownload.start != null) {
                    if (datenDownload.start.status < Start.STATUS_FERTIG) {
                        datenDownload.start.stoppen = true;
                    }
                }
                datenDownload.mVFilmSize.reset();
                datenDownload.start = null;
                Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
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
            Listener.notify(Listener.EREIGNIS_START_EVENT, this.getClass().getSimpleName());
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
            Listener.notify(Listener.EREIGNIS_LISTE_DOWNLOADS, this.getClass().getSimpleName());
        }
    }

    public synchronized DatenDownload getDownloadUrlFilm(String urlFilm) {
        for (DatenDownload datenDownload : this) {
            if (datenDownload.arr[DatenDownload.DOWNLOAD_FILM_URL].equals(urlFilm)) {
                return datenDownload;
            }
        }
        return null;
    }

    public synchronized void getModel(TModelDownload tModel, boolean onlyAbos, boolean onlyDownloads,
                                      boolean onlyNotStarted, boolean onlyStarted, boolean onlyWaiting, boolean onlyRun, boolean onlyFinished) {
        Object[] object;
        tModel.setRowCount(0);
        for (DatenDownload download : this) {
            if (download.istZurueckgestellt()) {
                continue;
            }

            boolean istAbo = download.istAbo();
            if (onlyAbos && !istAbo) {
                continue;
            }
            if (onlyDownloads && istAbo) {
                continue;
            }

            if (onlyNotStarted && !download.notStarted()) {
                continue;
            }
            if (onlyStarted && download.notStarted()) {
                continue;
            }

            if (onlyWaiting && !download.isWaiting()) {
                continue;
            }
            if (onlyRun && !download.running()) {
                continue;
            }
            if (onlyFinished && !download.isFinished()) {
                continue;
            }

            object = new Object[DatenDownload.MAX_ELEM];
            for (int i = 0; i < DatenDownload.MAX_ELEM; ++i) {
                if (i == DatenDownload.DOWNLOAD_NR) {
                    object[i] = download.nr;
                } else if (i == DatenDownload.DOWNLOAD_FILM_NR) {
                    if (download.film != null) {
                        object[i] = download.film.nr;
                    } else {
                        object[i] = 0;
                    }
                } else if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART
                        || i == DatenDownload.DOWNLOAD_UNTERBROCHEN
                        || i == DatenDownload.DOWNLOAD_SPOTLIGHT
                        || i == DatenDownload.DOWNLOAD_INFODATEI
                        || i == DatenDownload.DOWNLOAD_SUBTITLE
                        || i == DatenDownload.DOWNLOAD_ZURUECKGESTELLT
                        || i == DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER) {
                    object[i] = "";
                } else if (i == DatenDownload.DOWNLOAD_DATUM) {
                    object[i] = download.datumFilm;
                } else if (i == DatenDownload.DOWNLOAD_RESTZEIT) {
                    object[i] = download.getTextRestzeit();
                } else if (i == DatenDownload.DOWNLOAD_BANDBREITE) {
                    object[i] = download.getTextBandbreite();
                } else if (i == DatenDownload.DOWNLOAD_PROGRESS) {
                    object[i] = setProgress(download);
                } else if (i == DatenDownload.DOWNLOAD_GROESSE) {
                    object[i] = download.mVFilmSize;
                } else if (i == DatenDownload.DOWNLOAD_REF) {
                    object[i] = download;
                } else if (i != DatenDownload.DOWNLOAD_URL && !DatenDownload.anzeigen(i)) {
                    // Filmnr und URL immer füllen, egal ob angezeigt
                    object[i] = "";
                } else {
                    object[i] = download.arr[i];
                }
            }
            tModel.addRow(object);
        }
    }

    private String setProgress(DatenDownload download) {
        if (download.start != null) {
            if (1 < download.start.percent && download.start.percent < Start.PROGRESS_FERTIG) {
                String s = Double.toString(download.start.percent / 10.0) + '%';
                while (s.length() < 5) {
                    s = '0' + s;
                }
                return s;
            } else {
                return Start.getTextProgress(download.isDownloadManager(), download.start);
            }
        } else {
            return "";
        }
    }

    @SuppressWarnings("unchecked")
    public synchronized void setModelProgress(TModelDownload tModel) {
        Iterator<List<?>> it = tModel.getDataVector().iterator();
        int row = 0;
        while (it.hasNext()) {
            List<?> l = it.next();
            DatenDownload datenDownload = (DatenDownload) l.get(DatenDownload.DOWNLOAD_REF);
            if (datenDownload.start != null) {
                if (datenDownload.start.status == Start.STATUS_RUN) {
                    tModel.setValueAt(datenDownload.getTextRestzeit(), row, DatenDownload.DOWNLOAD_RESTZEIT);
                    tModel.setValueAt(datenDownload.getTextBandbreite(), row, DatenDownload.DOWNLOAD_BANDBREITE);
                    tModel.setValueAt(setProgress(datenDownload), row, DatenDownload.DOWNLOAD_PROGRESS);
                    tModel.setValueAt(datenDownload.mVFilmSize, row, DatenDownload.DOWNLOAD_GROESSE);
                }
            }
            ++row;
        }
    }

    @SuppressWarnings("unchecked")
    public synchronized void setModelProgressAlleStart(TModelDownload tModel) {
        for (List<Object> l : (Iterable<List<Object>>) tModel.getDataVector()) {
            DatenDownload datenDownload = (DatenDownload) l.get(DatenDownload.DOWNLOAD_REF);
            if (datenDownload.start != null) {
                l.set(DatenDownload.DOWNLOAD_RESTZEIT, datenDownload.getTextRestzeit());
                l.set(DatenDownload.DOWNLOAD_BANDBREITE, datenDownload.getTextBandbreite());
                l.set(DatenDownload.DOWNLOAD_PROGRESS, setProgress(datenDownload));
                l.set(DatenDownload.DOWNLOAD_GROESSE, datenDownload.mVFilmSize);
                l.set(DatenDownload.DOWNLOAD_UNTERBROCHEN, null);
            }
        }
    }

    public synchronized void abosSuchen(JFrame parent) {
        // in der Filmliste nach passenden Filmen suchen und 
        // in die Liste der Downloads eintragen
        final HashSet<String> listeUrls = new HashSet<>();
        // mit den bereits enthaltenen URL füllen
        this.forEach((download) -> listeUrls.add(download.arr[DatenDownload.DOWNLOAD_URL]));

        boolean gefunden = false;
        DatenAbo abo;
        // prüfen ob in "alle Filme" oder nur "nach Blacklist" gesucht werden soll
        boolean checkWithBlackList = Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO));
        DatenPset pSet_ = Daten.listePset.getPsetAbo("");
        for (DatenFilm film : daten.getListeFilme()) {
            abo = daten.getListeAbo().getAboFuerFilm_schnell(film, true /*auch die Länge überprüfen*/);
            if (abo == null) {
                // dann gibts dafür kein Abo
                continue;
            }
            if (!abo.aboIstEingeschaltet()) {
                continue;
            }
            if (checkWithBlackList) {
                //Blacklist auch bei Abos anwenden
                if (!daten.getListeBlacklist().checkBlackOkFilme_Downloads(film)) {
                    continue;
                }
            }
            if (daten.erledigteAbos.urlPruefen(film.getUrlHistory())) {
                // ist schon mal geladen worden
                continue;
            }
            DatenPset pSet = abo.arr[DatenAbo.ABO_PSET].isEmpty() ? pSet_ : Daten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET]);
            //DatenPset pSet = Daten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET]);
            if (pSet != null) {

                // mit der tatsächlichen URL prüfen, ob die URL schon in der Downloadliste ist
                String urlDownload = film.getUrlFuerAufloesung(pSet.arr[DatenPset.PROGRAMMSET_AUFLOESUNG]);
                if (listeUrls.contains(urlDownload)) {
                    continue;
                }
                listeUrls.add(urlDownload);
                //                if (checkUrlExists(urlDownload)) {
                //                    // haben wir schon in der Downloadliste
                //                    continue;
                //                }

                //diesen Film in die Downloadliste eintragen
                abo.arr[DatenAbo.ABO_DOWN_DATUM] = FormatterUtil.FORMATTER_ddMMyyyy.format(new Date());
                if (!abo.arr[DatenAbo.ABO_PSET].equals(pSet.arr[DatenPset.PROGRAMMSET_NAME])) {
                    // nur den Namen anpassen, falls geändert
                    abo.arr[DatenAbo.ABO_PSET] = pSet.arr[DatenPset.PROGRAMMSET_NAME];
                }
                //dann in die Liste schreiben
                add(new DatenDownload(pSet, film, DatenDownload.QUELLE_ABO, abo, "", "", "" /*Aufloesung*/));
                gefunden = true;
            } else if (parent != null) {
                // sonst sind wir evtl. nur in einer Konsole ohne X
                new DialogAboNoSet(parent, daten).setVisible(true);
                break;
            }
        }
        if (gefunden) {
            listeNummerieren();
        }
        listeUrls.clear();
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
        for (DatenDownload datenDownload : this) {
            Start s = datenDownload.start;
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
        aktivDownloads.addAll(this.stream()
                .filter(download -> download.start != null)
                .filter(download -> download.start.status < Start.STATUS_FERTIG)
                .filter(download -> quelle == DatenDownload.QUELLE_ALLE || download.quelle == quelle)
                .collect(Collectors.toList()));
        return aktivDownloads;
    }

    public synchronized TModel getModelStarts(TModel model) {
        model.setRowCount(0);
        Object[] object;

        if (!this.isEmpty()) {
            final int objLen = DatenDownload.MAX_ELEM + 1;
            object = new Object[objLen];
            for (DatenDownload datenDownload : this) {
                if (datenDownload.start != null) {
                    for (int k = 0; k < objLen; ++k) {
                        if (k < DatenDownload.MAX_ELEM) {
                            object[k] = datenDownload.arr[k];
                        } else if (datenDownload.istAbo()) {
                            object[k] = "Abo";
                        } else {
                            object[k] = "";
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
            Listener.notify(Listener.EREIGNIS_START_EVENT_BUTTON, this.getClass().getSimpleName());
        }
    }

    public synchronized DatenDownload getNextStart() {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        DatenDownload ret = null;
        if (this.size() > 0 && getDown(Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_MAX_DOWNLOAD)))) {
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

    public DatenDownload getRestartDownload() {
        // Versuch einen Fehlgeschlagenen Download zu finden um ihn wieder zu starten
        // die Fehler laufen aber einzeln, vorsichtshalber
        if (!getDown(1)) {
            return null;
        }
        for (DatenDownload datenDownload : this) {
            if (datenDownload.start == null) {
                continue;
            }

            if (datenDownload.start.status == Start.STATUS_ERR
                    && datenDownload.start.countRestarted < MVConfig.getInt(MVConfig.Configs.SYSTEM_PARAMETER_DOWNLOAD_MAX_RESTART)
                    && !maxSenderLaufen(datenDownload, 1)) {
                int restarted = datenDownload.start.countRestarted;
                if ( /*datenDownload.art == DatenDownload.ART_PROGRAMM && datenDownload.isRestart()   || */datenDownload.art == DatenDownload.ART_DOWNLOAD) {
                    datenDownload.resetDownload();
                    datenDownload.startDownload(daten);
                    datenDownload.start.countRestarted = ++restarted; //datenDownload.start ist neu!!!
                    return datenDownload;
                }
            }
        }
        return null;
    }

    // ################################################################
    // private
    // ################################################################
    private boolean getDown(int max) {
        int count = 0;
        for (DatenDownload datenDownload : this) {
            Start s = datenDownload.start;
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
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MAX_1_DOWNLOAD_PRO_SERVER))) {
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
                String uurl = datenDownload.arr[DatenDownload.DOWNLOAD_URL];
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
                if (host.isEmpty()) {
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

    //    private synchronized boolean checkUrlExists(String url) {
    //        //prüfen, ob der Film schon in der Liste ist, (manche Filme sind in verschiedenen Themen)
    //        for (DatenDownload download : this) {
    //            if (download.arr[DatenDownload.DOWNLOAD_URL].equals(url)) {
    //                return true;
    //            }
    //        }
    //        return false;
    //    }
}
