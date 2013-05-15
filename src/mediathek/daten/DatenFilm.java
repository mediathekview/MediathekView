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
    public static final int FILME_MAX_ELEM = 16;
    //
    public static final String FILM_NR = "Nr"; // wird vor dem Speichern gelöscht!
    public static final int FILM_NR_NR = 0;
    public static final String FILM_SENDER = "Sender";
    public static final int FILM_SENDER_NR = 1;
    public static final String FILM_THEMA = "Thema";
    public static final int FILM_THEMA_NR = 2;
    public static final String FILM_TITEL = "Titel";
    public static final int FILM_TITEL_NR = 3;
    public static final String FILM_DATUM = "Datum";
    public static final int FILM_DATUM_NR = 4;
    public static final String FILM_ZEIT = "Zeit";
    public static final int FILM_ZEIT_NR = 5;
    public static final String FILM_URL = "Url";
    public static final int FILM_URL_NR = 6;
    //public static final String FILM_URL_ORG = "UrlOrg";
    //public static final int FILM_URL_ORG_NR = 7;
    public static final String FILM_URL_RTMP = "UrlRTMP";
    public static final int FILM_URL_RTMP_NR = 7;
    public static final String FILM_URL_AUTH = "UrlAuth";
    public static final int FILM_URL_AUTH_NR = 8;
    public static final String FILM_URL_THEMA = "UrlThema";//url des Themas zum nachladen
    public static final int FILM_URL_THEMA_NR = 9;
    public static final String FILM_ABO_NAME = "Abo-Name";// wird vor dem Speichern gelöscht!
    public static final int FILM_ABO_NAME_NR = 10;
    public static final String FILM_DURATION = "Dauer";
    public static final int FILM_DURATION_NR = 11;
    public static final String FILM_DESCRIPTION = "Beschreibung";
    public static final int FILM_DESCRIPTION_NR = 12;
    public static final String FILM_IMAGE_URL = "Bild";
    public static final int FILM_IMAGE_URL_NR = 13;
    public static final String FILM_THUMBNAIL_URL = "Thumbnail";
    public static final int FILM_THUMBNAIL_URL_NR = 14;
    public static final String FILM_KEYWORDS = "Keywords";
    public static final int FILM_KEYWORDS_NR = 15;
    public static final String[] FILME_COLUMN_NAMES = {FILM_NR, FILM_SENDER, FILM_THEMA, FILM_TITEL, FILM_DATUM, FILM_ZEIT /*f*/,
        FILM_URL /*g*/ /*FILM_URL_ORG h*/, FILM_URL_RTMP, FILM_URL_AUTH, FILM_URL_THEMA, FILM_ABO_NAME /*l*/,
        FILM_DURATION, FILM_DESCRIPTION, FILM_IMAGE_URL, FILM_THUMBNAIL_URL, FILM_KEYWORDS};
    public static final String[] FILME_COLUMN_NAMES_ = {"a", "b", "c", "d", "e", "f", "g" /*"h"*/, "i", "j", "k", "l",
        "m", "n", "o", "p", "q"};
    public String[] arr;
    public Datum datumFilm = new Datum(0);
    public String durationStr = "";
    public long durationL = 0;

    public DatenFilm() {
        makeArr();
    }

    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
        this(ssender, tthema, urlThema, ttitel, uurl, datum, zeit, 0, "", "", "", new String[]{""});
    }

    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit,
            long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
        makeArr();
        arr[FILM_SENDER_NR] = ssender;
        arr[FILM_THEMA_NR] = tthema;
        arr[FILM_TITEL_NR] = ttitel;
        arr[FILM_URL_NR] = uurl;
        arr[FILM_DATUM_NR] = checkDatum(datum, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_ZEIT_NR] = checkZeit(arr[FILM_DATUM_NR], zeit, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_URL_THEMA_NR] = urlThema;
        arr[FILM_DURATION_NR] = "" + duration;
        arr[FILM_DESCRIPTION_NR] = description;
        arr[FILM_THUMBNAIL_URL_NR] = thumbnailUrl;
        arr[FILM_IMAGE_URL_NR] = imageUrl;
        arr[FILM_KEYWORDS_NR] = keywordsToString(keywords);
        setWerte(duration);
    }

    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlRtmp,
            String datum, String zeit) {
        this(ssender, tthema, urlThema, ttitel, uurl, uurlRtmp, datum, zeit, 0, "", "", "", new String[]{""});
    }

    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlRtmp,
            String datum, String zeit,
            long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
        makeArr();
        arr[FILM_SENDER_NR] = ssender;
        arr[FILM_THEMA_NR] = tthema;
        arr[FILM_TITEL_NR] = ttitel;
        arr[FILM_URL_NR] = uurl;
        arr[FILM_URL_RTMP_NR] = uurlRtmp;
        arr[FILM_URL_THEMA_NR] = urlThema;
        arr[FILM_DATUM_NR] = checkDatum(datum, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_ZEIT_NR] = checkZeit(arr[FILM_DATUM_NR], zeit, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_DURATION_NR] = "" + duration;
        arr[FILM_DESCRIPTION_NR] = description;
        arr[FILM_THUMBNAIL_URL_NR] = thumbnailUrl;
        arr[FILM_IMAGE_URL_NR] = imageUrl;
        arr[FILM_KEYWORDS_NR] = keywordsToString(keywords);
        setWerte(duration);
    }

    private String keywordsToString(String[] keywords) {
        String k = "";
        for (String kk : keywords) {
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
            durationStr = "";
            durationL = 0;
        } else {
            long dur = Long.parseLong(this.arr[DatenFilm.FILM_DURATION_NR]);
            durationL = dur;
            long hours = dur / 3600;
            dur = dur - (hours * 3600);
            long min = dur / 60;
            dur = dur - (min * 60);
            long seconds = dur;
            durationStr = fuellen(String.valueOf(hours)) + ":" + fuellen(String.valueOf(min)) + ":" + fuellen(String.valueOf(seconds));
        }
        datumFilm = DatumZeit.getDatumForObject(this);
    }

    final public void setWerte(long l) {
        if (l <= 0) {
            durationStr = "";
            durationL = 0;
        } else {
            long hours = l / 3600;
            l = l - (hours * 3600);
            long min = l / 60;
            l = l - (min * 60);
            long seconds = l;
            durationStr = fuellen(String.valueOf(hours)) + ":" + fuellen(String.valueOf(min)) + ":" + fuellen(String.valueOf(seconds));
            durationL = l;
        }
        datumFilm = DatumZeit.getDatumForObject(this);
    }

    private String fuellen(String s) {
        while (s.length() < 2) {
            s = "0" + s;
        }
        return s;
    }

    private static String checkDatum(String datum, String fehlermeldung) {
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

    private static String checkZeit(String datum, String zeit, String text) {
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
                    Log.debugMeldung("DatenFilm.CheckZeit [" + zeit + "] " + text);
                }
            }
        }
        return ret;
    }
}