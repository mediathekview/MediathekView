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

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.SimpleTimeZone;
import java.util.TreeSet;
import mediathek.controller.filmeLaden.FilmeLaden;
import mediathek.controller.filmeLaden.suchen.sender.MediathekArd;
import mediathek.controller.filmeLaden.suchen.sender.MediathekKika;
import mediathek.controller.filmeLaden.suchen.sender.MediathekNdr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekWdr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekZdf;
import mediathek.tool.Filter;
import mediathek.tool.Funktionen;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.TModelFilm;
import org.apache.commons.lang3.StringEscapeUtils;

public class ListeFilme extends LinkedList<DatenFilm> {

    public static final String THEMA_LIVE = "Livestream";
    //Tags Infos Filmliste, erste Zeile der .filme-Datei
    public static final String FILMLISTE = "Filmliste";
    public static final String FILMLISTE_DATUM = "Filmliste-Datum";
    public static final int FILMLISTE_DATUM_NR = 0;
    public static final String FILMLISTE_DATUM_GMT = "Filmliste-Datum-GMT";
    public static final int FILMLISTE_DATUM_GMT_NR = 1;
    public static final String FILMLISTE_VERSION = "Filmliste-Version";
    public static final int FILMLISTE_VERSION_NR = 2;
    public static final String FILMLISTE_PROGRAMM = "Filmliste-Programm";
    public static final int FILMLISTE_PRGRAMM_NR = 3;
    public static final int MAX_ELEM = 4;
    public static final String[] COLUMN_NAMES = {FILMLISTE_DATUM, FILMLISTE_DATUM_GMT, FILMLISTE_VERSION, FILMLISTE_PROGRAMM};
    private int nr = 0;
    public String[] metaDaten;
    private HashSet<String> hashSet = new HashSet<String>();
//    private LinkedList<String> listGetModelOfField = new LinkedList<String>();
    //private TreeSet<String> ts = new TreeSet<String>();
    private TreeSet<String> treeSet = new TreeSet<String>(GermanStringSorter.getInstance());
    private final String DATUM_ZEIT_FORMAT = "dd.MM.yyyy, HH:mm";
    private final String DATUM_ZEIT_FORMAT_REV = "yyyy.MM.dd__HH:mm";

    public ListeFilme() {
        metaDaten = newMetaDaten();
    }

    //===================================
    // public
    //===================================
    public static String[] newMetaDaten() {
        String[] ret = new String[MAX_ELEM];
        for (int i = 0; i < ret.length; ++i) {
            ret[i] = "";
        }
        return ret;
    }

    @Override
    public synchronized void clear() {
        hashSet.clear();
        treeSet.clear();
        nr = 0;
        super.clear();
    }

    public void check() {
        Iterator<DatenFilm> it = this.iterator();
        DatenFilm film;
        while (it.hasNext()) {
            film = it.next();
            film.arr[DatenFilm.FILM_THEMA_NR] = GuiFunktionen.cleanUnicode(film.arr[DatenFilm.FILM_THEMA_NR], "!!!!!!!!!!!!!");
            film.arr[DatenFilm.FILM_TITEL_NR] = GuiFunktionen.cleanUnicode(film.arr[DatenFilm.FILM_TITEL_NR], "!!!!!!!!!!!!!");
            if (film.arr[DatenFilm.FILM_URL_NR].contains(" ")) {
                System.out.println(film.arr[DatenFilm.FILM_URL_NR]);
            }
        }
    }

    public void sort() {
        Collections.<DatenFilm>sort(this);
        // und jetzt noch die Nummerierung in Ordnung bringen
        Iterator<DatenFilm> it = this.iterator();
        int i = 0;
        while (it.hasNext()) {
            it.next().arr[DatenFilm.FILM_NR_NR] = getNr(i++);
        }
    }

    public synchronized void setMeta(String[] mmeta) {
        for (int i = 0; i < MAX_ELEM; ++i) {
            metaDaten[i] = mmeta[i].toString();
        }
    }

    public synchronized boolean addFilmVomSender(DatenFilm film) {
        // Filme die beim Sender gesucht wurden (und nur die) hier eintragen
        // nur für die MediathekReader
        // ist eine URL,Sender,Thema,Titel schon vorhanden, wird sie verworfen, die aktuellste bleibt erhalten
        // Thema
        film.arr[DatenFilm.FILM_THEMA_NR] = StringEscapeUtils.unescapeXml(film.arr[DatenFilm.FILM_THEMA_NR].trim());
        film.arr[DatenFilm.FILM_THEMA_NR] = StringEscapeUtils.unescapeHtml4(film.arr[DatenFilm.FILM_THEMA_NR].trim());

        // Beschreibung
        film.arr[DatenFilm.FILM_BESCHREIBUNG_NR] = StringEscapeUtils.unescapeXml(film.arr[DatenFilm.FILM_BESCHREIBUNG_NR].trim());
        film.arr[DatenFilm.FILM_BESCHREIBUNG_NR] = StringEscapeUtils.unescapeHtml4(film.arr[DatenFilm.FILM_BESCHREIBUNG_NR].trim());

        // Titel
        film.arr[DatenFilm.FILM_TITEL_NR] = StringEscapeUtils.unescapeXml(film.arr[DatenFilm.FILM_TITEL_NR].trim());
        film.arr[DatenFilm.FILM_TITEL_NR] = StringEscapeUtils.unescapeHtml4(film.arr[DatenFilm.FILM_TITEL_NR].trim());
        // erst mal schauen obs das schon gibt
        DatenFilm f;
        String idx = film.getIndex();
        Iterator<DatenFilm> it = this.iterator();
        while (it.hasNext()) {
            f = it.next();
            if (f.getIndex().equals(idx)) {
                return false;
            }
        }
        return addInit(film);
    }

    public synchronized void updateListe(ListeFilme liste, boolean index /* Vergleich über Index, sonst nur URL */) {
        // in eine vorhandene Liste soll eine andere Filmliste einsortiert werden
        // es werden nur Filme die noch nicht vorhanden sind, einsortiert
        DatenFilm film;
        HashSet<String> hashSet = new HashSet<String>();
        Iterator<DatenFilm> it = this.iterator();
        while (it.hasNext()) {
            if (index) {
                hashSet.add(it.next().getIndex());
            } else {
                hashSet.add(it.next().arr[DatenFilm.FILM_URL_NR]);
            }
        }
        it = liste.iterator();
        while (it.hasNext()) {
            film = it.next();
            if (index) {
                if (!hashSet.contains(film.getIndex())) {
                    addInit(film);
                }
            } else {
                if (!hashSet.contains(film.arr[DatenFilm.FILM_URL_NR])) {
                    addInit(film);
                }
            }
        }
        hashSet.clear();
    }

    public synchronized void nurDoppelteAnzeigen(DDaten ddaten, boolean index) {
        // zum Debuggen: URLs die doppelt sind, in die History eintragen
        // damit sie markiert werden
        DatenFilm film;
        HashSet<String> hashDoppelt = new HashSet<String>();
        HashSet<String> hashSet = new HashSet<String>();
        Iterator<DatenFilm> it = this.iterator();
        while (it.hasNext()) {
            film = it.next();
            if (index) {
                if (!hashSet.contains(film.getIndex())) {
                    hashSet.add(film.getIndex());
                } else {
                    // dann ist er mind. doppelt in der Liste
                    hashDoppelt.add(film.arr[DatenFilm.FILM_URL_NR]);
                }
            } else {
                if (!hashSet.contains(film.arr[DatenFilm.FILM_URL_NR])) {
                    hashSet.add(film.arr[DatenFilm.FILM_URL_NR]);
                } else {
                    // dann ist er mind. doppelt in der Liste
                    hashDoppelt.add(film.arr[DatenFilm.FILM_URL_NR]);
                }
            }
        }
        it = this.iterator();
        while (it.hasNext()) {
            if (!hashDoppelt.contains(it.next().arr[DatenFilm.FILM_URL_NR])) {
                it.remove();
            }
        }
        hashSet.clear();
        hashDoppelt.clear();
    }

    public boolean addInit(DatenFilm film) {
        film.init();
        return add(film);
    }

    public synchronized boolean addWithNr(DatenFilm film) {
        // hier nur beim Laden von der Filmliste
        film.arr[DatenFilm.FILM_NR_NR] = getNr(nr++);
        return addInit(film);
    }

    private String getNr(int nr) {
        final int MAX_STELLEN = 5;
        final String FUELL_ZEICHEN = "0";
        String str = String.valueOf(nr);
        while (str.length() < MAX_STELLEN) {
            str = FUELL_ZEICHEN + str;
        }
        return str;
    }

    public synchronized int countSender(String sender) {
        int ret = 0;
        ListIterator<DatenFilm> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(sender)) {
                ++ret;
            }
        }
        return ret;
    }

    public synchronized void delSender(String sender) {
        // alle Filme VOM SENDER löschen
        DatenFilm film;
        ListIterator<DatenFilm> it = this.listIterator(0);
        while (it.hasNext()) {
            film = it.next();
            if (film.arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(sender)) {
                it.remove();
            }
        }
    }

    public void liveStreamEintragen() {
        // Live-Stream eintragen
        //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
        addFilmVomSender(new DatenFilm(MediathekNdr.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekNdr.SENDER + " " + THEMA_LIVE,
                "http://www.ndr.de/resources/metadaten/ndr_fs_nds_hi_wmv.asx", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        addFilmVomSender(new DatenFilm(MediathekWdr.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekWdr.SENDER + " " + THEMA_LIVE,
                "http://www.wdr.de/wdrlive/media/wdr-fernsehen_web-l.asx", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        // die neuen Livestreams ARD
        addFilmVomSender(new DatenFilm(MediathekArd.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekArd.SENDER + " Small " + THEMA_LIVE,
                "rtsp://daserste.edges.wowza.gl-systemhaus.de/live/mp4:daserste_int_320", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        addFilmVomSender(new DatenFilm(MediathekArd.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekArd.SENDER + " Medium " + THEMA_LIVE,
                "rtsp://daserste.edges.wowza.gl-systemhaus.de/live/mp4:daserste_int_576", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        addFilmVomSender(new DatenFilm(MediathekArd.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekArd.SENDER + " Big " + THEMA_LIVE,
                "rtsp://daserste.edges.wowza.gl-systemhaus.de/live/mp4:daserste_int_1600", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        // ZDF
        addFilmVomSender(new DatenFilm(MediathekZdf.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekZdf.SENDER + " " + THEMA_LIVE,
                "rtsp://3gp-livestreaming1.zdf.de/liveedge2/de10_v1_710.sdp", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        addFilmVomSender(new DatenFilm(MediathekZdf.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekZdf.SENDER + ".info " + THEMA_LIVE,
                "rtsp://3gp-livestreaming1.zdf.de/liveedge2/de08_v1_710.sdp", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        addFilmVomSender(new DatenFilm(MediathekZdf.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekZdf.SENDER + ".kultur " + THEMA_LIVE,
                "rtsp://3gp-livestreaming1.zdf.de/liveedge2/de07_v1_710.sdp", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        addFilmVomSender(new DatenFilm(MediathekZdf.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekZdf.SENDER + ".neo " + THEMA_LIVE,
                "rtsp://3gp-livestreaming1.zdf.de/liveedge2/de09_v1_710.sdp", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
        // KIKA
        addFilmVomSender(new DatenFilm(MediathekKika.SENDER, THEMA_LIVE, ""/* urlThema */,
                MediathekKika.SENDER + " " + THEMA_LIVE,
                "http://kikaplus.net/clients/kika/player/myplaylist.php?channel=1&programm=1&videoid=1", ""/*rtmpURL*/, ""/* datum */, ""/* zeit */, 0, "", "", "", new String[]{""}));
    }

    public synchronized TModelFilm getModelTabFilme(DDaten ddaten, TModelFilm modelFilm__,
            String filterSender, String filterThema, String filterTitel, String filterThemaTitel, String filterIrgendwo,
            int laenge, boolean keineAbos, boolean kGesehen, boolean nurHd, boolean live) {
        // Model für die Tabelle Filme zusammenbauen
        TModelFilm modelFilm = new TModelFilm(new Object[][]{}, DatenFilm.COLUMN_NAMES);
        if (this.size() == 0) {
            return modelFilm;
        }
        if (filterSender.equals("") && filterThema.equals("") && filterTitel.equals("") && filterThemaTitel.equals("") && filterIrgendwo.equals("") && laenge == 0
                && keineAbos == false && kGesehen == false && nurHd == false && live == false) {
            // wenn ganze Liste
            addObjectDataTabFilme(modelFilm);
        } else {
            // Titel
            String[] arrTitel;
            if (Filter.isPattern(filterTitel)) {
                arrTitel = new String[]{filterTitel};
            } else {
                arrTitel = filterTitel.split(",");
                for (int i = 0; i < arrTitel.length; ++i) {
                    arrTitel[i] = arrTitel[i].trim();
                }
            }
            // ThemaTitel
            String[] arrThemaTitel;
            if (Filter.isPattern(filterThemaTitel)) {
                arrThemaTitel = new String[]{filterThemaTitel};
            } else {
                arrThemaTitel = filterThemaTitel.split(",");
                for (int i = 0; i < arrThemaTitel.length; ++i) {
                    arrThemaTitel[i] = arrThemaTitel[i].trim();
                }
            }
            // Irgendwo
            String[] arrIrgendwo;
            if (Filter.isPattern(filterIrgendwo)) {
                arrIrgendwo = new String[]{filterIrgendwo};
            } else {
                arrIrgendwo = filterIrgendwo.split(",");
                for (int i = 0; i < arrIrgendwo.length; ++i) {
                    arrIrgendwo[i] = arrIrgendwo[i].trim();
                }
            }
            DatenFilm film;
            Iterator<DatenFilm> it = this.iterator();
            while (it.hasNext()) {
                film = it.next();
                if (live) {
                    if (!film.arr[DatenFilm.FILM_THEMA_NR].equals(ListeFilme.THEMA_LIVE)) {
                        continue;
                    }
                }
                if (nurHd) {
                    if (film.arr[DatenFilm.FILM_URL_HD_NR].isEmpty()) {
                        continue;
                    }
                }
                if (keineAbos) {
                    if (!film.arr[DatenFilm.FILM_ABO_NAME_NR].isEmpty()) {
                        continue;
                    }
                }
                if (kGesehen) {
                    if (ddaten.history.contains(film.arr[DatenFilm.FILM_URL_NR])) {
                        continue;
                    }
                }
                if (Filter.filterAufFilmPruefen(filterSender, filterThema, arrTitel, arrThemaTitel, arrIrgendwo, laenge, film, true /*länge nicht prüfen*/)) {
                    addObjectDataTabFilme(modelFilm, film);
                }
            }
        }
        return modelFilm;
    }

    public synchronized String[] getModelOfFieldThema(String sender) {
        // erstellt ein StringArray der Themen eines Senders oder wenn "sender" leer, aller Sender
        // ist für die Filterfelder im GuiFilme
        // doppelte Einträge (bei der Groß- und Kleinschribung) werden entfernt
        String str, s;
        treeSet.add("");
        DatenFilm film;
        Iterator<DatenFilm> it = this.iterator();
        if (sender.equals("")) {
            //alle Theman
            while (it.hasNext()) {
                str = it.next().arr[DatenFilm.FILM_THEMA_NR];
                //hinzufügen
                s = str.toLowerCase();
                if (!hashSet.contains(s)) {
                    hashSet.add(s);
                    treeSet.add(str);
                }
            }
        } else {
            //nur Theman des Senders
            while (it.hasNext()) {
                film = it.next();
                if (film.arr[DatenFilm.FILM_SENDER_NR].equals(sender)) { // Filterstring ist immer "Sender"
                    //hinzufügen
                    str = film.arr[DatenFilm.FILM_THEMA_NR];
                    s = str.toLowerCase();
                    if (!hashSet.contains(s)) {
                        hashSet.add(s);
                        treeSet.add(str);
                    }
                }
            }
        }
        hashSet.clear();
        String[] a = treeSet.toArray(new String[]{});
        treeSet.clear();
        return a;
    }

    public synchronized String[] getModelOfFieldSender() {
        // erstellt ein StringArray mit den Sendernamen
        String str;
        treeSet.add("");
        Iterator<DatenFilm> it = this.iterator();
        // Sendernamen gibts nur in einer Schreibweise
        int max = DDaten.filmeLaden.getSenderNamen().length; // gibt nur so viele
        while (it.hasNext()) {
            str = it.next().arr[DatenFilm.FILM_SENDER_NR];
            if (!treeSet.contains(str)) {
                treeSet.add(str);
                if (treeSet.size() > max) { // eins mehr wegen Leerzeile
                    break;
                }
            }
        }
        String[] a = treeSet.toArray(new String[]{});
        treeSet.clear();
        return a;
    }

    public synchronized DatenFilm getFilmByUrl(String url) {
        // Problem wegen gleicher URLs
        DatenFilm ret = null;
        ListIterator<DatenFilm> it = this.listIterator(0);
        while (it.hasNext()) {
            DatenFilm f = it.next();
            if (f.arr[DatenFilm.FILM_URL_NR].equals(url)) {
                ret = f;
                break;
            }
        }
        return ret;
    }

    public synchronized DatenFilm getFilmByNr(String nr) {
        DatenFilm ret = null;
        ListIterator<DatenFilm> it = this.listIterator(0);
        while (it.hasNext()) {
            DatenFilm f = it.next();
            if (f.arr[DatenFilm.FILM_NR_NR].equals(nr)) {
                ret = f;
                break;
            }
        }
        return ret;
    }

    //===================================
    // private
    //===================================
    private void addObjectDataTabFilme(TModelFilm model) {
        DatenFilm film;
        //DatenAbo datenAbo;
        if (this.size() > 0) {
            ListIterator<DatenFilm> iterator = this.listIterator(0);
            while (iterator.hasNext()) {
                film = iterator.next();
                addObjectDataTabFilme(model, film);
            }
        }
    }

    private void addObjectDataTabFilme(TModelFilm model, DatenFilm film) {
        Object[] object = new Object[DatenFilm.MAX_ELEM];
        for (int m = 0; m < DatenFilm.MAX_ELEM; ++m) {
            if (m == DatenFilm.FILM_DATUM_NR) {
                object[m] = film.datumFilm;
            } else if (m != DatenFilm.FILM_URL_NR && m != DatenFilm.FILM_NR_NR && !DatenFilm.anzeigen(m)) {
                // Url und Nr immer füllen, egal ob angezeigt
                object[m] = "";
            } else {
                object[m] = film.arr[m];
            }
        }
        model.addRow(object);
    }

    public void abosEintragen(DDaten ddaten) {
        // Aboname in die Filmliste eintragen
        DatenFilm film;
        DatenAbo datenAbo;
        ListIterator<DatenFilm> iterator = this.listIterator(0);
        while (iterator.hasNext()) {
            film = iterator.next();
            datenAbo = ddaten.listeAbo.getAboFuerFilm(film, false);
            if (datenAbo != null) {
                film.arr[DatenFilm.FILM_ABO_NAME_NR] = datenAbo.arr[DatenAbo.ABO_NAME_NR];
                // und jetzt noch die Filmlänge prüfen
                if (!Filter.laengePruefen(datenAbo.mindestdauerMinuten, film.dauerL)) {
                    // dann ist der Film zu kurz
                    film.arr[DatenFilm.FILM_ABO_NAME_NR] = film.arr[DatenFilm.FILM_ABO_NAME_NR] + " [zu kurz]";
                }
            } else {
                film.arr[DatenFilm.FILM_ABO_NAME_NR] = "";
            }
        }
    }

//    public void kleineUrlEintragen() {
//        // für kleine URLs die nach einem Schema gebilget werden
//        // müssen erst in die Liste als Url_klein eingetragen werden
//        DatenFilm film;
//        ListIterator<DatenFilm> iterator = this.listIterator(0);
//        while (iterator.hasNext()) {
//            film = iterator.next();
//            if (film.arr[DatenFilm.FILM_URL_KLEIN_NR].isEmpty()) {
//                // dann nach Schema suchen
//                film.arr[DatenFilm.FILM_URL_KLEIN_NR] = film.getUrlFuerAufloesung(DatenPset.AUFLOESUNG_KLEIN);
//                if (film.arr[DatenFilm.FILM_URL_NR].equals(film.arr[DatenFilm.FILM_URL_KLEIN_NR])) {
//                    //dann gibts keine kleine
//                    film.arr[DatenFilm.FILM_URL_KLEIN_NR] = "";
//                }
//            }
//            if (film.arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR].isEmpty()) {
//                // dann nach Schema suchen
//                film.arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR] = film.getUrlRtmpFuerAufloesung(DatenPset.AUFLOESUNG_KLEIN);
//                if (film.arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR].equals(film.arr[DatenFilm.FILM_URL_RTMP_NR])) {
//                    //dann gibts keine kleine
//                    film.arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR] = "";
//                }
//            }
//        }
//    }

    public String genDate() {
        // Tag, Zeit in lokaler Zeit wann die Filmliste erstellt wurde
        String ret;
        SimpleDateFormat sdf = new SimpleDateFormat(DATUM_ZEIT_FORMAT);
        String date;
        if (metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR].equals("")) {
            // noch eine alte Filmliste
            ret = metaDaten[ListeFilme.FILMLISTE_DATUM_NR];
        } else {
            date = metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR];
            sdf.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"));
            Date filmDate = null;
            try {
                filmDate = sdf.parse(date);
            } catch (ParseException ex) {
            }
            if (filmDate == null) {
                ret = metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR];
            } else {
                SimpleDateFormat formatter = new SimpleDateFormat(DATUM_ZEIT_FORMAT);
                ret = formatter.format(filmDate);
            }
        }
        return ret;
    }

    public String genDateRev() {
        // Tag, Zeit in lokaler Zeit wann die Filmliste erstellt wurde
        // in der Form yyyy.MM.dd__hh:mm
        String ret;
        SimpleDateFormat sdf = new SimpleDateFormat(DATUM_ZEIT_FORMAT);
        String date;
        if (metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR].equals("")) {
            // noch eine alte Filmliste
            ret = metaDaten[ListeFilme.FILMLISTE_DATUM_NR];
        } else {
            date = metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR];
            sdf.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"));
            Date filmDate = null;
            try {
                filmDate = sdf.parse(date);
            } catch (ParseException ex) {
            }
            if (filmDate == null) {
                ret = metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR];
            } else {
                SimpleDateFormat formatter = new SimpleDateFormat(DATUM_ZEIT_FORMAT_REV);
                ret = formatter.format(filmDate);
            }
        }
        return ret;
    }

    public int alterFilmlisteSek() {
        // Alter der Filmliste in Sekunden
        int ret = 0;
        Date jetzt = new Date(System.currentTimeMillis());
        SimpleDateFormat sdf = new SimpleDateFormat(DATUM_ZEIT_FORMAT);
        String date;
        if (!metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR].equals("")) {
            date = metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR];
            sdf.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"));
        } else {
            date = metaDaten[ListeFilme.FILMLISTE_DATUM_NR];
        }
        Date filmDate = null;
        try {
            filmDate = sdf.parse(date);
        } catch (ParseException ex) {
        }
        if (jetzt != null && filmDate != null) {
            ret = Math.round((jetzt.getTime() - filmDate.getTime()) / (1000));
            if (ret < 0) {
                ret = 0;
            }
        }
        return ret;
    }

    public boolean filmlisteZuAlt() {
        if (this.size() == 0) {
            return true;
        }
        // Filmliste ist älter als: FilmeLaden.ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE
        return filmlisteIstAelter(FilmeLaden.ALTER_FILMLISTE_SEKUNDEN_FUER_AUTOUPDATE);
    }

    public boolean filmlisteIstAelter(int sekunden) {
        int ret = alterFilmlisteSek();
        if (ret != 0) {
            Log.systemMeldung("Die Filmliste ist " + ret / 60 + " Minuten alt");
        }
        return ret > sekunden;
    }

    public void metaDatenSchreiben() {
        // FilmlisteMetaDaten
        metaDaten = ListeFilme.newMetaDaten();
        if (!Daten.filmeLaden.getStop() /* löschen */) {
            metaDaten[ListeFilme.FILMLISTE_DATUM_NR] = getJetzt_ddMMyyyy_HHmm();
            metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR] = getJetzt_ddMMyyyy_HHmm_gmt();
        } else {
            metaDaten[ListeFilme.FILMLISTE_DATUM_NR] = "";
            metaDaten[ListeFilme.FILMLISTE_DATUM_GMT_NR] = "";
        }
        metaDaten[ListeFilme.FILMLISTE_VERSION_NR] = Konstanten.VERSION;
        metaDaten[ListeFilme.FILMLISTE_PRGRAMM_NR] = Funktionen.getProgVersionString() + " - Compiled: " + Funktionen.getCompileDate();
    }

    private String getJetzt_ddMMyyyy_HHmm() {
        SimpleDateFormat formatter = new SimpleDateFormat(DATUM_ZEIT_FORMAT);
        return formatter.format(new Date());
    }

    private String getJetzt_ddMMyyyy_HHmm_gmt() {
        SimpleDateFormat formatter = new SimpleDateFormat(DATUM_ZEIT_FORMAT);
        formatter.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"));
        return formatter.format(new Date());
    }
}
