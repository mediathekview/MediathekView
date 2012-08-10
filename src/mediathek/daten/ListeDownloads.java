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
import java.util.*;
import javax.swing.JOptionPane;
import mediathek.Daten;
import mediathek.controller.filme.filmeImportieren.MediathekListener;
import mediathek.controller.io.starter.Starts;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.tool.DatumZeit;
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

    public synchronized void listePutzen() {
        // beim Programmende fertige Downloads löschen
        LinkedList<Starts> s = ddaten.starterClass.getStarts(Starts.QUELLE_ALLE);
        Iterator<Starts> it = s.iterator();
        while (it.hasNext()) {
            Starts st = it.next();
            if (st != null) {
                if (st.status >= Starts.STATUS_FERTIG) {
                    ddaten.listeDownloads.delDownloadByUrl(st.datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                }
            }
        }
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
        ListIterator<DatenDownload> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().arr[DatenDownload.DOWNLOAD_URL_NR].equals(url)) {
                it.remove();
                return true;
            }
        }
        return false;
    }

    public synchronized void getModel(TModelDownload tModel, boolean abos, boolean downloads) {
        tModel.setRowCount(0);
        Object[] object;
        DatenDownload download;
        if (this.size() > 0) {
            ListIterator<DatenDownload> iterator = this.listIterator(0);
            while (iterator.hasNext()) {
                download = iterator.next();
                boolean istAbo = download.istAbo();
                if (abos && istAbo || downloads && !istAbo) {
                    DatenDownload datenDownload = download.getCopy();
                    object = new Object[DatenDownload.DOWNLOAD_MAX_ELEM];
                    for (int i = 0; i < DatenDownload.DOWNLOAD_MAX_ELEM; ++i) {
                        if (i == DatenDownload.DOWNLOAD_PROGRAMM_RESTART_NR) {
                            object[i] = "";
                        } else if (i == DatenDownload.DOWNLOAD_DATUM_NR) {
                            object[i] = DatumZeit.getDatumForObject(datenDownload);
                        } else {
                            object[i] = datenDownload.arr[i];
                        }
                    }
                    tModel.addRow(object);
                }
            }
        }
    }

    public synchronized void addDownloadTabFilme(DatenFilm film) {
        // Filme in die Liste der Downloads eintragen
        // ist eine URL schon vorhanden,Sender,Thema,Titel aktualisieren,
        // es wird der aktuellere Eintrag verwendet
        if (ddaten.listePset.getListeSpeichern().size() == 0) {
            JOptionPane.showMessageDialog(null, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Aufzeichnen festlegen.",
                    "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
            // Satz mit x, war wohl nix
            return;
        }
        DialogAddDownload dialog = new DialogAddDownload(null, ddaten, film);
        dialog.setVisible(true);
        if (dialog.ok && dialog.pSet != null) {
            // erst mal schauen obs das schon gibt
            Iterator<DatenDownload> it = this.iterator();
            while (it.hasNext()) {
                if (it.next().arr[DatenDownload.DOWNLOAD_URL_NR].equals(film.arr[DatenFilm.FILM_URL_NR])) {
                    // alte Eintrag wird gelöscht und der neue eingetragen
                    it.remove();
                }
            }
            // und dann eintragen
            DatenDownload download = new DatenDownload(dialog.pSet, film, Starts.QUELLE_DOWNLOAD, null);
            this.add(download);
            if (dialog.starten) {
                // und evtl. auch gleich starten
                ddaten.starterClass.addStarts(new Starts(download));
            }
            Daten.notifyMediathekListener(MediathekListener.EREIGNIS_LISTE_DOWNLOADS, ListeDownloads.class.getSimpleName());
        }
    }

    public synchronized void abosEintragen() {
        // in der Filmliste nach passenden Filmen suchen und 
        // in die Liste der Downloads eintragen
        DatenFilm film;
        DatenAbo abo;
        ListIterator<DatenFilm> itFilm = Daten.listeFilme.listIterator();
        while (itFilm.hasNext()) {
            film = itFilm.next();
            abo = ddaten.listeAbo.getAbo(film.arr[DatenFilm.FILM_SENDER_NR],
                    film.arr[DatenFilm.FILM_THEMA_NR],
                    film.arr[DatenFilm.FILM_TITEL_NR]);
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
            } else {
                //diesen Film in die Downloadliste eintragen
                abo.arr[DatenAbo.ABO_DOWN_DATUM_NR] = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
                DDaten.setGeaendert();
                //wenn nicht doppelt, dann in die Liste schreiben
                DatenPset pSet = ddaten.listePset.getPsetAbo(abo.arr[DatenAbo.ABO_PSET_NR]);
                if (!abo.arr[DatenAbo.ABO_PSET_NR].equals(pSet.arr[DatenPset.PROGRAMMSET_NAME_NR])) {
                    // abo ändern
                    abo.arr[DatenAbo.ABO_PSET_NR] = pSet.arr[DatenPset.PROGRAMMSET_NAME_NR];
                }
                if (pSet != null) {
                    this.add(new DatenDownload(pSet, film, Starts.QUELLE_ABO, abo));
                }
            }
        } //while
    }

    public synchronized void abosLoschen() {
        // es werden alle Abos (DIE NOCH NICHT GESTARTET SIND) aus der Liste gelöscht
        Iterator<DatenDownload> it = this.iterator();
        while (it.hasNext()) {
            DatenDownload d = it.next();
            if (d.istAbo()) {
                Starts s = ddaten.starterClass.getStart(d.arr[DatenDownload.DOWNLOAD_URL_NR]);
                if (s == null) {
                    // ansonsten läuft er schon
                    it.remove();
                }
            }
        }
    }

    //===================================
    // private
    //===================================
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
