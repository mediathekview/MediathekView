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
import mediathek.controller.filmeLaden.suchen.sender.Mediathek3Sat;
import mediathek.controller.filmeLaden.suchen.sender.MediathekNdr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekSf;
import mediathek.controller.filmeLaden.suchen.sender.MediathekSwr;
import mediathek.controller.filmeLaden.suchen.sender.MediathekZdf;
import mediathek.tool.Datum;
import mediathek.tool.DatumZeit;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.GuiKonstanten;
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
    public static final String FILM_DAUER = "Dauer";
    public static final String FILM_DAUER_ = "m";
    public static final int FILM_DAUER_NR = 6;
    public static final String FILM_GROESSE = "Größe [MB]";
    public static final String FILM_GROESSE_ = "t";
    public static final int FILM_GROESSE_NR = 7;
    public static final String FILM_BESCHREIBUNG = "Beschreibung";
    public static final String FILM_BESCHREIBUNG_ = "n";
    public static final int FILM_DESCRIPTION_NR = 8;
    public static final String FILM_KEYWORDS = "Keywords";
    public static final String FILM_KEYWORDS_ = "q";
    public static final int FILM_KEYWORDS_NR = 9;
    public static final String FILM_URL = "Url";
    public static final String FILM_URL_ = "g";
    public static final int FILM_URL_NR = 10;
    public static final String FILM_WEBSEITE = "Website"; //URL der Website des Films beim Sender
    public static final String FILM_WEBSEITE_ = "k";
    public static final int FILM_WEBSEITE_NR = 11;
    public static final String FILM_ABO_NAME = "Aboname";// wird vor dem Speichern gelöscht!
    public static final String FILM_ABO_NAME_ = "l";
    public static final int FILM_ABO_NAME_NR = 12;
    public static final String FILM_IMAGE_URL = "Bild";
    public static final String FILM_IMAGE_URL_ = "o";
    public static final int FILM_IMAGE_URL_NR = 13;
    public static final String FILM_URL_RTMP = "UrlRTMP";
    public static final String FILM_URL_RTMP_ = "i";
    public static final int FILM_URL_RTMP_NR = 14;
    public static final String FILM_URL_AUTH = "UrlAuth";
    public static final String FILM_URL_AUTH_ = "j";
    public static final int FILM_URL_AUTH_NR = 15;
    public static final String FILM_URL_KLEIN = "Url_Klein";
    public static final String FILM_URL_KLEIN_ = "r";
    public static final int FILM_URL_KLEIN_NR = 16;
    public static final String FILM_URL_RTMP_KLEIN = "UrlRTMP_Klein";
    public static final String FILM_URL_RTMP_KLEIN_ = "s";
    public static final int FILM_URL_RTMP_KLEIN_NR = 17;
    public static final int FILME_MAX_ELEM = 18;
    public static final String[] FILME_COLUMN_NAMES = {FILM_NR, FILM_SENDER, FILM_THEMA, FILM_TITEL, FILM_DATUM, FILM_ZEIT, FILM_DAUER, FILM_GROESSE,
        FILM_BESCHREIBUNG, FILM_KEYWORDS, FILM_URL, FILM_WEBSEITE, FILM_ABO_NAME,
        FILM_IMAGE_URL, FILM_URL_RTMP, FILM_URL_AUTH, FILM_URL_KLEIN, FILM_URL_RTMP_KLEIN};
    public static final String[] FILME_COLUMN_NAMES_ = {FILM_NR_, FILM_SENDER_, FILM_THEMA_, FILM_TITEL_, FILM_DATUM_, FILM_ZEIT_, FILM_DAUER_, FILM_GROESSE_,
        FILM_BESCHREIBUNG_, FILM_KEYWORDS_, FILM_URL_, FILM_WEBSEITE_, FILM_ABO_NAME_,
        FILM_IMAGE_URL_, FILM_URL_RTMP_, FILM_URL_AUTH_, FILM_URL_KLEIN_, FILM_URL_RTMP_KLEIN_};
    public String[] arr;
    public Datum datumFilm = new Datum(0);
    public String dauerStr = "";
    public long dauerL = 0; // Sekunden
    public String groesseStr = "";
    public long groesseL = 0; // Dateigröße

    public DatenFilm() {
        makeArr();
    }

//    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
//        this(ssender, tthema, urlThema, ttitel, uurl, ""/*rtmpURL*/, datum, zeit, 0, "", "", "", new String[]{""});
//    }
//    public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String datum, String zeit,
//            long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
//        makeArr();
//        arr[FILM_SENDER_NR] = ssender;
//        arr[FILM_THEMA_NR] = tthema;
//        arr[FILM_TITEL_NR] = ttitel;
//        arr[FILM_URL_NR] = uurl;
//        arr[FILM_DATUM_NR] = checkDatum(datum, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
//        arr[FILM_ZEIT_NR] = checkZeit(arr[FILM_DATUM_NR], zeit, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
//        arr[FILM_WEBSEITE_NR] = filmWebsite;
//        arr[FILM_DURATION_NR] = "" + duration;
//        arr[FILM_DESCRIPTION_NR] = beschreibung(description, tthema, ttitel);
//        if (!imageUrl.equals("")) {
//            arr[FILM_IMAGE_URL_NR] = imageUrl;
//        } else {
//            arr[FILM_IMAGE_URL_NR] = thumbnailUrl;
//        }
//        arr[FILM_KEYWORDS_NR] = keywordsToString(keywords);
//        setWerte(duration);
//    }
//    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlRtmp,
//            String datum, String zeit) {
//        this(ssender, tthema, urlThema, ttitel, uurl, uurlRtmp, datum, zeit, 0, "", "", "", new String[]{""});
//    }
    public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String uurlRtmp,
            String datum, String zeit,
            long dauer, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
        makeArr();
        arr[FILM_SENDER_NR] = ssender;
        arr[FILM_THEMA_NR] = tthema;
        arr[FILM_TITEL_NR] = ttitel;
        arr[FILM_URL_NR] = uurl;
        arr[FILM_URL_RTMP_NR] = uurlRtmp;
        arr[FILM_WEBSEITE_NR] = filmWebsite;
        arr[FILM_DATUM_NR] = checkDatum(datum, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_ZEIT_NR] = checkZeit(arr[FILM_DATUM_NR], zeit, arr[FILM_SENDER_NR] + " " + arr[FILM_THEMA_NR] + " " + arr[FILM_TITEL_NR]);
        arr[FILM_DESCRIPTION_NR] = beschreibung(description, tthema, ttitel);
        // Bild eintragen
        if (!imageUrl.equals("")) {
            arr[FILM_IMAGE_URL_NR] = imageUrl;
        } else {
            arr[FILM_IMAGE_URL_NR] = thumbnailUrl;
        }
        // Schlüsselwörter
        arr[FILM_KEYWORDS_NR] = keywordsToString(keywords);
        // Filmlänge
        if (dauer <= 0 || dauer > 3600 * 5 /* Werte über 5 Stunden */) {
            arr[FILM_DAUER_NR] = "0";
        } else {
            arr[FILM_DAUER_NR] = String.valueOf(dauer);
        }
//        init();
    }

    public boolean addKleineUrl(String url, String urlRtmp) {
        boolean ret = true;
        arr[FILM_URL_KLEIN_NR] = getKlein(arr[FILM_URL_NR], url);
        arr[FILM_URL_RTMP_KLEIN_NR] = getKlein(arr[FILM_URL_RTMP_NR], urlRtmp);
        return ret;
    }

    private String getKlein(String url1, String url2) {
        String ret = "";
        boolean diff = false;
        for (int i = 0; i < url2.length(); ++i) {
            if (url1.length() > i) {
                if (url1.charAt(i) != url2.charAt(i)) {
                    if (!diff) {
                        ret = i + "|";
                    }
                    diff = true;
                }
            } else {
                diff = true;
            }
            if (diff) {
                ret += url2.charAt(i);
            }
        }
        return ret;
    }

    public String getUrlNormal() {
        // liefert die normale URL
        return arr[DatenFilm.FILM_URL_NR];
    }

    public String getUrlKleinNormal() {
        // liefert die kleine normale URL
        String ret = "";
        int i;
        if (arr[DatenFilm.FILM_URL_KLEIN_NR].equals("")) {
            return getUrlLow(arr[DatenFilm.FILM_URL_NR]);
        } else {
            try {
                i = Integer.parseInt(arr[DatenFilm.FILM_URL_KLEIN_NR].substring(0, arr[DatenFilm.FILM_URL_KLEIN_NR].indexOf("|")));
                ret = arr[DatenFilm.FILM_URL_NR].substring(0, i) + arr[DatenFilm.FILM_URL_KLEIN_NR].substring(arr[DatenFilm.FILM_URL_KLEIN_NR].indexOf("|") + 1);
            } catch (Exception ex) {
            }
        }
        return ret;
    }

    public String getUrlFlvstreamer() {
        String ret;
        if (!arr[DatenFilm.FILM_URL_RTMP_NR].equals("")) {
            ret = arr[DatenFilm.FILM_URL_RTMP_NR];
        } else {
            if (arr[DatenFilm.FILM_URL_NR].startsWith(GuiKonstanten.RTMP_PRTOKOLL)) {
                ret = GuiKonstanten.RTMP_FLVSTREAMER + arr[DatenFilm.FILM_URL_NR];
            } else {
                ret = arr[DatenFilm.FILM_URL_NR];
            }
        }
        return ret;
    }

    public String getUrlKleinFlvstreamer() {
        // liefert die kleine flvstreamer URL
        String ret = "";
        if (!arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR].equals("")) {
            // es gibt eine kleine RTMP
            try {
                int i = Integer.parseInt(arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR].substring(0, arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR].indexOf("|")));
                ret = arr[DatenFilm.FILM_URL_RTMP_NR].substring(0, i) + arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR].substring(arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR].indexOf("|") + 1);
            } catch (Exception ex) {
            }
        } else {
            // es gibt keine kleine RTMP
            if (!arr[DatenFilm.FILM_URL_RTMP_NR].equals("")) {
                // dann gibts keine kleine
                ret = getUrlLow(arr[DatenFilm.FILM_URL_RTMP_NR]);
            } else {
                // dann gibts überhaupt nur die normalen URLs
                ret = getUrlKleinNormal();
                // und jetzt noch "-r" davorsetzten wenn nötig
                if (ret.startsWith(GuiKonstanten.RTMP_PRTOKOLL)) {
                    ret = GuiKonstanten.RTMP_FLVSTREAMER + ret;
                }
            }
        }
        return ret;
    }

    private String getUrlLow(String url) {
        String ret = url;
        if (arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(MediathekSwr.SENDER)) {
            //swr
            ret = url.replace(".l.mp4", ".m.mp4");
//        } else if (arr[DOWNLOAD_SENDER_NR].equalsIgnoreCase(MediathekWdr.SENDER) && !arr[DOWNLOAD_THEMA_NR].equals("Rockpalast")) {
//            //WDR
//            ret = url.replace("-l.mp4", "-m.mp4");
//            // funktioniert nur bei einem Teil (Thema: Rockpalast geht nie)
        } else if (arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(Mediathek3Sat.SENDER) || arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(MediathekZdf.SENDER)) {
            // ZDF und 3sat
            // <video dur="00:08:02" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/09/120919_westerwelle_mom_51k_p7v9.mp4" system-bitrate="62000">
            // <video dur="00:08:02" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/09/120919_westerwelle_mom_536k_p9v9.mp4" system-bitrate="700000">
            // <video dur="00:08:02" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/09/120919_westerwelle_mom_1596k_p13v9.mp4" system-bitrate="1700000">
            if (url.endsWith("vh.mp4")) {
                ret = url.replace("vh.mp4", "h.mp4");
            } else if (url.endsWith("1456k_p13v11.mp4")) {
                ret = url.replace("1456k_p13v11.mp4", "436k_p9v11.mp4");
            } else if (url.endsWith("1596k_p13v9.mp4")) {
                ret = url.replace("1596k_p13v9.mp4", "536k_p9v9.mp4");
            }
        } else if (arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(MediathekSf.SENDER)) {
            if (url.endsWith("_hq1.mp4")) {
                ret = url.replace("_hq1.mp4", "_lq1.mp4");
            }
        } else if (arr[DatenFilm.FILM_SENDER_NR].equalsIgnoreCase(MediathekNdr.SENDER)) {
            //NDR
            ret = url.replace(".hq.", ".hi.");
        } else if ((url.startsWith("rtmpt://cp160844.edgefcs.net") || url.startsWith("--host cp160844.edgefcs.net")) && url.endsWith(".hq.mp4")) {
            // für die NDR-Filme beim ARD
            ret = url.replace(".hq.", ".hi.");
        }
        return ret;
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
        return arr[FILM_SENDER_NR].toLowerCase() + arr[FILM_THEMA_NR].toLowerCase() + arr[FILM_URL_NR];
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

    public void init_() {
        // Filmdauer
        long l = Long.parseLong(this.arr[DatenFilm.FILM_DAUER_NR]);
        dauerL = l;
        if (l > 0) {
            long hours = l / 3600;
            l = l - (hours * 3600);
            long min = l / 60;
            l = l - (min * 60);
            long seconds = l;
            dauerStr = fuellen(2, String.valueOf(hours)) + ":" + fuellen(2, String.valueOf(min)) + ":" + fuellen(2, String.valueOf(seconds));
        }
        // Datum
        datumFilm = DatumZeit.getDatumForObject(this);
        // Dateigröße
        if (this.arr[DatenFilm.FILM_GROESSE_NR].equals("")) {
            this.arr[DatenFilm.FILM_GROESSE_NR] = "-1";
        }
        groesseL = Long.parseLong(this.arr[DatenFilm.FILM_GROESSE_NR]);
        if (groesseL > 0) {
            // sonst kann ich mirs sparen
            if (groesseL > 1024 * 1024) {
                groesseStr = String.valueOf(groesseL / (1024 * 1024));
                groesseStr = fuellen(4, groesseStr);
                groesseStr = groesseStr.substring(0, groesseStr.length() - 3) + "." + groesseStr.substring(groesseStr.length() - 3);
            }
        }
    }

    public void init() {
        // Filmdauer
        dauerL = Long.parseLong(this.arr[DatenFilm.FILM_DAUER_NR]);
        if (dauerL > 0) {
            String hours = String.valueOf(dauerL / 3600);
            long l = dauerL % 3600;
            String min = String.valueOf(l / 60);
            String seconds = String.valueOf(l % 60);
            dauerStr = fuellen(2, hours) + ":" + fuellen(2, min) + ":" + fuellen(2, seconds);
        }
        // Datum
        datumFilm = DatumZeit.getDatumForObject(this);
        // Dateigröße
        if (this.arr[DatenFilm.FILM_GROESSE_NR].equals("")) {
            this.arr[DatenFilm.FILM_GROESSE_NR] = "-1";
        }
        groesseL = Long.parseLong(this.arr[DatenFilm.FILM_GROESSE_NR]);
        if (groesseL > 0) {
            // sonst kann ich mirs sparen
            if (groesseL > 1024 * 1024) {
                groesseStr = String.valueOf(groesseL / (1024 * 1024));
                groesseStr = fuellen(4, groesseStr);
                groesseStr = groesseStr.substring(0, groesseStr.length() - 3) + "." + groesseStr.substring(groesseStr.length() - 3);
            }
        }
    }

    private String fuellen(int anz, String s) {
        while (s.length() < anz) {
            s = "0" + s;
        }
        return s;
    }

    private void makeArr() {
        arr = new String[FILME_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }

//    final public void setWerte() {
//        if (this.arr[DatenFilm.FILM_DURATION_NR].equals("") || this.arr[DatenFilm.FILM_DURATION_NR].equals("0") || this.arr[DatenFilm.FILM_DURATION_NR].equals("-1")) {
//            setWerte(0);
//        } else {
//            setWerte(Long.parseLong(this.arr[DatenFilm.FILM_DURATION_NR]));
//        }
//    }
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