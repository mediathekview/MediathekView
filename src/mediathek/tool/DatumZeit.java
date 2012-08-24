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
package mediathek.tool;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;

public class DatumZeit {

    public static String getJetzt_ddMMyyyy_HHmm() {
        Date today;
        String output;
        SimpleDateFormat formatter;
        formatter = new SimpleDateFormat("dd.MM.yyyy, HH:mm");
        today = new Date();
        output = formatter.format(today);
        return output;
    }

    public static String getJetzt_HH_MM_SS() {
        Date today;
        String output;
        SimpleDateFormat formatter;
        formatter = new SimpleDateFormat("HH:mm:ss");
        today = new Date();
        output = formatter.format(today);
        return output;
    }

    public static String getJetzt_HHMMSS() {
        Date today;
        String output;
        SimpleDateFormat formatter;
        formatter = new SimpleDateFormat("HHmmss");
        today = new Date();
        output = formatter.format(today);
        return output;
    }

    public static String getHeute_yyyyMMdd() {
        Date today;
        String output;
        SimpleDateFormat formatter;
        formatter = new SimpleDateFormat("yyyyMMdd");
        today = new Date();
        output = formatter.format(today);
        return output;
    }

    public static String getHeute_dd_MM_yyyy() {
        Date today;
        String output;
        SimpleDateFormat formatter;
        formatter = new SimpleDateFormat("dd.MM.yyyy");
        today = new Date();
        output = formatter.format(today);
        return output;
    }

    public static String convertDatum(String datum) {
        //<pubDate>Mon, 03 Jan 2011 17:06:16 +0100</pubDate>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US);
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("dd.MM.yyyy");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(649600299, "DatumDatum.convertDatum", ex);
        }
        return datum;
    }

    public static String convertTime(String zeit) {
        //<pubDate>Mon, 03 Jan 2011 17:06:16 +0100</pubDate>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z", Locale.US);
            Date filmDate = sdfIn.parse(zeit);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("HH:mm:ss");
            zeit = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(663259004, "DatumZeit.convertTime", ex);
        }
        return zeit;
    }

//    public static String datumDrehen(String datum) {
//        String ret = "";
//        if (!datum.equals("")) {
//            try {
//                if (datum.length() == 10) {
//                    String tmp = datum.substring(6); // Jahr
//                    tmp += "." + datum.substring(3, 5); // Monat
//                    tmp += "." + datum.substring(0, 2); // Tag
//                    ret = tmp;
//                }
//            } catch (Exception ex) {
//                Log.fehlerMeldung(293561086, "DatumZeit.datumDrehen", ex);
//            }
//
//        }
//        return ret;
//    }
//
//    public static String datumDatumZeitReinigen(String datum) {
//        String ret = "";
//        ret = datum;
//        ret = ret.replace(":", "");
//        ret = ret.replace(".", "");
//        return ret;
//    }

    public static Datum getDatumForObject(String datum) {
        Datum tmp = new Datum(0);
        if (!datum.equals("")) {
            try {
                SimpleDateFormat sdf = new SimpleDateFormat("dd.MM.yyyy");
                tmp.setTime(sdf.parse(datum).getTime());
            } catch (ParseException ex) {
            }
        }
        return tmp;
    }

    public static Datum getDatumForObject(DatenFilm film) {
        SimpleDateFormat sdf_datum_zeit = new SimpleDateFormat("dd.MM.yyyyHH:mm:ss");
        SimpleDateFormat sdf_datum = new SimpleDateFormat("dd.MM.yyyy");
        Datum tmp = new Datum(0);
        if (!film.arr[DatenFilm.FILM_DATUM_NR].equals("")) {
            try {
                if (!film.arr[DatenFilm.FILM_ZEIT_NR].equals("")) {
                    tmp.setTime(sdf_datum_zeit.parse(film.arr[DatenFilm.FILM_DATUM_NR] + film.arr[DatenFilm.FILM_ZEIT_NR]).getTime());
                } else {
                    tmp.setTime(sdf_datum.parse(film.arr[DatenFilm.FILM_DATUM_NR]).getTime());
                }
            } catch (ParseException ex) {
            }
        }
        return tmp;
    }

    public static Datum getDatumForObject(DatenDownload datenDownload) {
        SimpleDateFormat sdf_datum_zeit = new SimpleDateFormat("dd.MM.yyyyHH:mm:ss");
        SimpleDateFormat sdf_datum = new SimpleDateFormat("dd.MM.yyyy");
        Datum tmp = new Datum(0);
        if (!datenDownload.arr[DatenDownload.DOWNLOAD_DATUM_NR].equals("")) {
            try {
                if (!datenDownload.arr[DatenDownload.DOWNLOAD_ZEIT_NR].equals("")) {
                    tmp.setTime(sdf_datum_zeit.parse(datenDownload.arr[DatenDownload.DOWNLOAD_DATUM_NR] + datenDownload.arr[DatenDownload.DOWNLOAD_ZEIT_NR]).getTime());
                } else {
                    tmp.setTime(sdf_datum.parse(datenDownload.arr[DatenDownload.DOWNLOAD_DATUM_NR]).getTime());
                }
            } catch (ParseException ex) {
            }
        }
        return tmp;
    }
}
