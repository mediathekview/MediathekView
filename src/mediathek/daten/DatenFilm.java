/*
 *   MediathekView
 *   Copyright (C) 2008 W. Xaver
 *   W.Xaver[at]googlemail.com
 *   http://zdfmediathk.sourceforge.net/
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import java.text.SimpleDateFormat;
import java.util.Date;
import mediathek.tool.Datum;
import mediathek.tool.DatumZeit;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.Log;

public class DatenFilm implements Comparable<DatenFilm> {
    //Tags Filme

    public static final String FELD_INFO = "Feldinfo";
    public static final String FILME = "Filme";
    public static final String FILME_ = "X";
    //
    public static final String FILM_NR = "Nr"; // wird vor dem Speichern gelöscht!
    public static final String FILM_NR_ = "a";
    public static final int FILM_NR_NR = 0;
    public static final String FILM_SENDER = "Sender";
    public static final String FILM_SENDER_ = "b";
    public static final int FILM_SENDER_NR = 1;
    public static final String FILM_THEMA = "Thema";
    public static final String FILM_THEMA_ = "c";
    public static final int FILM_THEMA_NR = 2;
    public static final String FILM_TITEL = "Titel";
    public static final String FILM_TITEL_ = "d";
    public static final int FILM_TITEL_NR = 3;
    public static final String FILM_DATUM = "Datum";
    public static final String FILM_DATUM_ = "e";
    public static final int FILM_DATUM_NR = 4;
    public static final String FILM_ZEIT = "Zeit";
    public static final String FILM_ZEIT_ = "f";
    public static final int FILM_ZEIT_NR = 5;
    public static final String FILM_DURATION = "Dauer";
    public static final String FILM_DURATION_ = "m";
    public static final int FILM_DURATION_NR = 6;
    public static final String FILM_DESCRIPTION = "Beschreibung";
    public static final String FILM_DESCRIPTION_ = "n";
    public static final int FILM_DESCRIPTION_NR = 7;
    public static final String FILM_KEYWORDS = "Keywords";
    public static final String FILM_KEYWORDS_ = "q";
    public static final int FILM_KEYWORDS_NR = 8;
    public static final String FILM_URL = "Url";
    public static final String FILM_URL_ = "g";
    public static final int FILM_URL_NR = 9;
    public static final String FILM_WEBSEITE = "Website"; //URL der Website des Films beim Sender
    public static final String FILM_WEBSEITE_ = "k";
    public static final int FILM_WEBSEITE_NR = 10;
    public static final String FILM_ABO_NAME = "Aboname";// wird vor dem Speichern gelöscht!
    public static final String FILM_ABO_NAME_ = "l";
    public static final int FILM_ABO_NAME_NR = 11;
    public static final String FILM_IMAGE_URL = "Bild";
    public static final String FILM_IMAGE_URL_ = "o";
    public static final int FILM_IMAGE_URL_NR = 12;
//    public static final String FILM_THUMBNAIL_URL = "Thumbnail";
//    public static final String FILM_THUMBNAIL_URL_ = "p";
//    public static final int FILM_THUMBNAIL_URL_NR = 13;
    public static final String FILM_URL_RTMP = "UrlRTMP";
    public static final String FILM_URL_RTMP_ = "i";
    public static final int FILM_URL_RTMP_NR = 13;
    public static final String FILM_URL_AUTH = "UrlAuth";
    public static final String FILM_URL_AUTH_ = "j";
    public static final int FILM_URL_AUTH_NR = 14;
    public static final int FILME_MAX_ELEM = 15;
    public static final String[] FILME_COLUMN_NAMES = {FILM_NR, FILM_SENDER, FILM_THEMA, FILM_TITEL, FILM_DATUM, FILM_ZEIT, FILM_DURATION,
        FILM_DESCRIPTION, FILM_KEYWORDS, FILM_URL, FILM_WEBSEITE, FILM_ABO_NAME,
        FILM_IMAGE_URL, FILM_URL_RTMP, FILM_URL_AUTH};
    public static final String[] FILME_COLUMN_NAMES_ = {FILM_NR_, FILM_SENDER_, FILM_THEMA_, FILM_TITEL_, FILM_DATUM_, FILM_ZEIT_, FILM_DURATION_,
        FILM_DESCRIPTION_, FILM_KEYWORDS_, FILM_URL_, FILM_WEBSEITE_, FILM_ABO_NAME_,
        FILM_IMAGE_URL_, FILM_URL_RTMP_, FILM_URL_AUTH_};
    public String[] arr;
    public Datum datumFilm = new Datum(0);
    public String durationStr = "";
    public long durationL = 0; // Sekunden

    public DatenFilm() {
        makeArr();
    }

    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
        this(ssender, tthema, urlThema, ttitel, uurl, datum, zeit, 0, "", "", "", new String[]{""});
    }

    public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String datum, String zeit,
            long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
        makeArr();
        arr[FILM_SENDER_NR] = ssender;
        arr[FILM_THEMA_NR] = tthema;
        arr[FILM_TITEL_NR] = ttitel;
        arr[FILM_URL_NR] = uurl;
        arr[FILM_DATUM_NR] = checkDatum(datum, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_ZEIT_NR] = checkZeit(arr[FILM_DATUM_NR], zeit, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_WEBSEITE_NR] = filmWebsite;
        arr[FILM_DURATION_NR] = "" + duration;
        arr[FILM_DESCRIPTION_NR] = beschreibung(description, tthema, ttitel);
        if (!imageUrl.equals("")) {
            arr[FILM_IMAGE_URL_NR] = imageUrl;
        } else {
            arr[FILM_IMAGE_URL_NR] = thumbnailUrl;
        }
        arr[FILM_KEYWORDS_NR] = keywordsToString(keywords);
        setWerte(duration);
    }

    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlRtmp,
            String datum, String zeit) {
        this(ssender, tthema, urlThema, ttitel, uurl, uurlRtmp, datum, zeit, 0, "", "", "", new String[]{""});
    }

    public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String uurlRtmp,
            String datum, String zeit,
            long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
        makeArr();
        arr[FILM_SENDER_NR] = ssender;
        arr[FILM_THEMA_NR] = tthema;
        arr[FILM_TITEL_NR] = ttitel;
        arr[FILM_URL_NR] = uurl;
        arr[FILM_URL_RTMP_NR] = uurlRtmp;
        arr[FILM_WEBSEITE_NR] = filmWebsite;
        arr[FILM_DATUM_NR] = checkDatum(datum, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_ZEIT_NR] = checkZeit(arr[FILM_DATUM_NR], zeit, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_DURATION_NR] = "" + duration;
        arr[FILM_DESCRIPTION_NR] = beschreibung(description, tthema, ttitel);
        if (!imageUrl.equals("")) {
            arr[FILM_IMAGE_URL_NR] = imageUrl;
        } else {
            arr[FILM_IMAGE_URL_NR] = thumbnailUrl;
        }
        arr[FILM_KEYWORDS_NR] = keywordsToString(keywords);
        setWerte(duration);
    }

    private String beschreibung(String s, String thema, String titel) {
        // die Beschreibung auf x Zeichen beschränken
        if (s.startsWith(titel)) {
            s = s.substring(titel.length()).trim();
        }
        if (s.startsWith(thema)) {
            s = s.substring(thema.length()).trim();
        }
        if (s.startsWith("|")) {
            s = s.substring(1).trim();
        }
        if (s.startsWith("Video-Clip")) {
            s = s.substring("Video-Clip".length()).trim();
        }
        if (s.startsWith(titel)) {
            s = s.substring(titel.length()).trim();
        }
        if (s.startsWith(":")) {
            s = s.substring(1).trim();
        }
        final int x = 250;
        if (s.length() > x) {
            return s.substring(0, x) + "\n.....";
        } else {
            return s;
        }
    }

    private String keywordsToString(String[] keywords) {
        final int x = 200;
        String k = "";
        for (String kk : keywords) {
            if (k.length() + kk.length() > x) {
                // nicht mehr als x zeichen lang!
                break;
            }
            if (k.length() > 0) {
                k += ",";
            }
            k += kk;
        }
        return k;
    }

    public String getIndex() {
        // liefert einen eindeutigen Index für die Filmliste
        return arr[FILM_SENDER_NR] + arr[FILM_THEMA_NR] + arr[FILM_URL_NR];
    }

    public DatenFilm getClean() {
        // vor dem Speichern nicht benötigte Felder löschen
        arr[FILM_NR_NR] = "";
        arr[FILM_ABO_NAME_NR] = "";
        return this;
    }

    public DatenFilm getCopy() {
        DatenFilm ret = new DatenFilm();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        ret.datumFilm = this.datumFilm;
        return ret;
    }

    @Override
    public int compareTo(DatenFilm arg0) {
        int ret;
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        if ((ret = sorter.compare(arr[FILM_SENDER_NR], arg0.arr[FILM_SENDER_NR])) == 0) {
            ret = sorter.compare(arr[FILM_THEMA_NR], arg0.arr[FILM_THEMA_NR]);
        }
        return ret;
    }

    public String getUrlOrg() {
        return arr[FILM_URL_NR];
    }

    private void makeArr() {
        arr = new String[FILME_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }

    final public void setWerte() {
        if (this.arr[DatenFilm.FILM_DURATION_NR].equals("") || this.arr[DatenFilm.FILM_DURATION_NR].equals("0") || this.arr[DatenFilm.FILM_DURATION_NR].equals("-1")) {
            setWerte(0);
        } else {
            setWerte(Long.parseLong(this.arr[DatenFilm.FILM_DURATION_NR]));
        }
    }

    private void setWerte(long l) {
        if (l <= 0 || l > 3600 * 5 /* Werte über 5 Stungen */) {
            durationStr = "";
            durationL = 0;
        } else {
            durationL = l;
            long hours = l / 3600;
            l = l - (hours * 3600);
            long min = l / 60;
            l = l - (min * 60);
            long seconds = l;
            durationStr = fuellen(String.valueOf(hours)) + ":" + fuellen(String.valueOf(min)) + ":" + fuellen(String.valueOf(seconds));
        }
        arr[FILM_DURATION_NR] = "" + durationL;
        datumFilm = DatumZeit.getDatumForObject(this);
    }

    private String fuellen(String s) {
        while (s.length() < 2) {
            s = "0" + s;
        }
        return s;
    }

    public static String checkDatum(String datum, String fehlermeldung) {
        //Datum max. 100 Tage in der Zukunft
        final long MAX = 1000L * 60L * 60L * 24L * 100L;
        String ret = datum.trim();
        if (ret.equals("")) {
            return "";
        }
        if (!ret.contains(".")) {
            Log.debugMeldung("DatenFilm.CheckDatum-1 [" + datum + "] " + fehlermeldung);
            return "";
        }
        if (ret.length() != 10) {
            Log.debugMeldung("DatenFilm.CheckDatum-2 [" + datum + "] " + fehlermeldung);
            return "";
        }
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("dd.MM.yyyy");
            Date filmDate = sdfIn.parse(ret);
            if (filmDate.getTime() < 0) {
                //Datum vor 1970
                Log.debugMeldung("DatenFilm.CheckDatum-3 - " + "Unsinniger Wert: [" + datum + "] " + fehlermeldung);
                ret = "";
            }
            if ((new Date().getTime() + MAX) < filmDate.getTime()) {
                Log.debugMeldung("DatenFilm.CheckDatum-4 - " + "Unsinniger Wert: [" + datum + "] " + fehlermeldung);
                ret = "";
            }
        } catch (Exception ex) {
            ret = "";
            Log.fehlerMeldung(794630593, Log.FEHLER_ART_PROG, "DatenFilm.checkDatum-5", ex);
            Log.fehlerMeldung(946301596, Log.FEHLER_ART_PROG, "DatenFilm.CheckDatum-6 [", datum + "] " + fehlermeldung);
        }
        if (ret.equals("")) {
        }
        return ret;
    }

    public static String checkZeit(String datum, String zeit, String fehlermeldung) {
        String ret = zeit.trim();
        if (datum.equals("")) {
            //wenn kein Datum, macht die Zeit auch keinen Sinn
            ret = "";
        } else {
            if (!ret.equals("")) {
                if (!ret.contains(":")) {
                    ret = "";
                }
                if (ret.length() != 8) {
                    ret = "";
                }
                if (ret.equals("")) {
                    Log.debugMeldung("DatenFilm.CheckZeit [" + zeit + "] " + fehlermeldung);
                }
            }
        }
        return ret;
    }
}