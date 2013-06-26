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

import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import mediathek.controller.io.starter.Start;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.TModelDownload;

public class ListeDownloads extends LinkedList<DatenDownload> {

    private DDaten ddaten;

    /**
     *
     * @param ddaten
     */
    public ListeDownloads(DDaten ddaten) {
        this.ddaten = ddaten;
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
        d.setWerte();
        return super.add(d);
    }

    public synchronized void zurueckgestellteWiederAktivieren() {
        DatenDownload d = null;
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            it.next().arr[DatenDownload.DOWNLOAD_ZURUECKGESTELLT_NR] = Boolean.FALSE.toString();
        }
    }

    public synchronized void listePutzen() {
        // beim Programmende fertige Downloads löschen
        boolean gefunden = false;
        LinkedList<Start> s = ddaten.starterClass.getStarts(Start.QUELLE_ALLE);
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
        ddaten.starterClass.aufraeumen();
    }

    public synchronized DatenDownload downloadVorziehen(String url) {
        DatenDownload d = null;
        Start s = ddaten.starterClass.urlVorziehen(url);
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
                    DatenDownload datenDownload = download.getCopy();
                    object = new Object[DatenDownload.DOWNLOAD_MAX_ELEM];
                    for (int i = 0; i < DatenDownload.DOWNLOAD_MAX_ELEM; ++i) {
                        if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                            object[i] = "";
                        } else if (i == DatenDownload.DOWNLOAD_DAUER_NR) {
                            object[i] = datenDownload.durationStr;
                        } else if (i == DatenDownload.DOWNLOAD_DATUM_NR) {
                            object[i] = datenDownload.datumFilm;
                        } else {
                            object[i] = datenDownload.arr[i];
                        }
                    }
                    tModel.addRow(object);
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
        boolean checkBlack = !Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUSGESCHALTET_NR])
                && Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_BLACKLIST_AUCH_ABO_NR]);
        ListIterator<DatenFilm> itFilm = Daten.listeFilme.listIterator();
        while (itFilm.hasNext()) {
            film = itFilm.next();
            abo = ddaten.listeAbo.getAboFuerFilm(film);
            if (abo == null) {
                continue;
            } else if (!abo.aboIstEingeschaltet()) {
                continue;
            } else if (ddaten.erledigteAbos.urlPruefen(film.arr[DatenFilm.FILM_URL_NR])) {
                // ist schon im Logfile, weiter
                continue;
            } else if (checkListe(film.arr[DatenFilm.FILM_URL_NR])) {
                // haben wir schon in der Downloadliste
                continue;
            } else if (checkBlack && !ddaten.listeBlacklist.checkBlackOkFilme_Downloads(film)) {
                // wenn Blacklist auch für Abos, dann ers mal da schauen
                continue;
            } else {
                //diesen Film in die Downloadliste eintragen
                abo.arr[DatenAbo.ABO_DOWN_DATUM_NR] = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
                //wenn nicht doppelt, dann in die Liste schreiben
                DatenPset pSet = ddaten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET_NR]);
                if (!abo.arr[DatenAbo.ABO_PSET_NR].equals(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR])) {
                    // abo ändern
                    abo.arr[DatenAbo.ABO_PSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
                }
                if (pSet != null) {
                    add(new DatenDownload(pSet, film, Start.QUELLE_ABO, abo, "", ""));
                    gefunden = true;
                }
            }
        } //while
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
                Start s = ddaten.starterClass.getStart(d.arr[DatenDownload.DOWNLOAD_URL_NR]);
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

    private boolean checkListe(String url) {
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
}
