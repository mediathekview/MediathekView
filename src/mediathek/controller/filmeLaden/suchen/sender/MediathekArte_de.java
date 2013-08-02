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
package mediathek.controller.filmeLaden.suchen.sender;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class MediathekArte_de extends MediathekReader implements Runnable {

    public static final String SENDER_ARTE_DE = "ARTE.DE";
    public static final String SENDER_ARTE_FR = "ARTE.FR";
    public static final String SENDER_ARTE = "ARTE";
    // "Freitag, 02. August um 12:41 Uhr"
    SimpleDateFormat sdfIn = new SimpleDateFormat("EEEE, dd. MMM H:mm yyyy", Locale.GERMANY);
    SimpleDateFormat sdfZeit = new SimpleDateFormat("HH:mm:ss");
    SimpleDateFormat sdfDatum = new SimpleDateFormat("dd.MM.yyyy");
    StringBuffer seite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
    String jahr = "";
    String startUrl = "http://www.arte.tv/guide/de/plus7.json?page=1&per_page=400&regions=default%2CEUR_DE_FR%2CDE_FR%2CSAT%2CALL";

    /**
     *
     * @param ddaten
     * @param dde
     */
    public MediathekArte_de(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER_ARTE_DE, /* threads */ 3, /* urlWarten */ 500, startPrio);
        getUrlIo.setTimeout(15000);
    }

    //===================================
    // public
    //===================================
//    @Override
//    public String[] getNameSenderFilmliste() {
//        return new String[]{SENDER_ARTE_DE, SENDER_ARTE_FR};
//    }
//
//    @Override
//    public boolean checkNameSenderFilmliste(String name) {
//        return SENDER_ARTE_DE.equalsIgnoreCase(name) || SENDER_ARTE_FR.equalsIgnoreCase(name) || SENDER_ARTE.equalsIgnoreCase(name);
//    }
    @Override
    public void addToList() {
        meldungStart();
        getJahr();
        addTheman(startUrl);
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new ArteThemaLaden()).start();
            }
        }
    }

    public void getJahr() {
        SimpleDateFormat formatter = new SimpleDateFormat(" yyyy");
        jahr = formatter.format(new Date());
    }

    private void addTheman(String startUrl) {
        //{"image_url":"http://www.arte.tv/papi/tvguide/images/1055199/W940H530/048531-020-A_1260.jpg",
        // "title":"Alte Schachteln","duration":2,"airdate_long":"Freitag, 02. August um 12:41 Uhr",
        // "desc":"Kleiner Gefallen","url":"/guide/de/048531-020/alte-schachteln","video_channels":"",
        // "video_views":"0 Aufrufe","video_rights_until":"Verfügbar: 167:07","video_rank":null},

        final String MUSTER_START = "{\"image_url\":\"";
        final String MUSTER_TITEL = "\"title\":\"";
        final String MUSTER_DAUER = "\"duration\":";
        final String MUSTER_BESCHREIBUNG = "\"desc\":\"";
        final String MUSTER_URL = "\"url\":\"";
        final String MUSTER_DATUM = "\"airdate_long\":\"";
        String[] arr;
        seite1 = getUrlIo.getUri_Utf(nameSenderMReader, startUrl, seite1, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url;
        String bild;
        String datum;
        String dauer;
        String thema;
        String beschreibung;
        while ((pos = seite1.indexOf(MUSTER_START, pos)) != -1) {
            url = "";
            bild = "";
            datum = "";
            dauer = "";
            thema = "";
            beschreibung = "";
            pos += MUSTER_START.length();
            if ((pos2 = seite1.indexOf("\"", pos)) != -1) {
                bild = seite1.substring(pos, pos2);
            }
            if ((pos1 = seite1.indexOf(MUSTER_TITEL, pos)) != -1) {
                pos1 += MUSTER_TITEL.length();
                if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                    thema = seite1.substring(pos1, pos2);
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_DAUER, pos)) != -1) {
                pos1 += MUSTER_DAUER.length();
                if ((pos2 = seite1.indexOf(",", pos1)) != -1) {
                    dauer = seite1.substring(pos1, pos2);
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_DATUM, pos)) != -1) {
                pos1 += MUSTER_DATUM.length();
                if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                    datum = seite1.substring(pos1, pos2);
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_BESCHREIBUNG, pos)) != -1) {
                pos1 += MUSTER_BESCHREIBUNG.length();
                if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                    beschreibung = seite1.substring(pos1, pos2);
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_URL, pos)) != -1) {
                pos1 += MUSTER_URL.length();
                if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                    url = seite1.substring(pos1, pos2);
                    if (url.isEmpty()) {
                        ////
                        continue;
                    } else {
                        url = "http://www.arte.tv" + url;
                    }
                }
            }
            arr = new String[]{url, bild, thema, dauer, datum, beschreibung};
            listeThemen.add(arr);
        }
    }

    class ArteThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public void run() {
            try {
                meldungAddThread();
                String link[];
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    meldungProgress(link[0] /* url */);
                    filmeLaden1(seite2, link);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-894330854, Log.FEHLER_ART_MREADER, "MediathekHr.ThemaLaden.run", ex, "");
            }
            meldungThreadUndFertig();
        }
    }

    void filmeLaden1(StringBuffer seite, String[] arr) {
        String urlJson = "";
        String titel = "";
        final String MUSTER_URL = "arte_vp_url=\"";
        final String MUSTER_TITEL = "<h2 class=\"span12\">";
        int pos1, pos2;
        if (Daten.filmeLaden.getStop()) {
            return;
        }
        seite = getUrlIo.getUri_Utf(nameSenderMReader, arr[0], seite, "");
        if ((pos1 = seite.indexOf(MUSTER_TITEL)) != -1) {
            pos1 += MUSTER_TITEL.length();
            if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                titel = seite.substring(pos1, pos2);
            }
        }
        if (titel.isEmpty()) {
            titel = arr[2];
        }
        if ((pos1 = seite.indexOf(MUSTER_URL)) != -1) {
            pos1 += MUSTER_URL.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                urlJson = seite.substring(pos1, pos2);
            }
        }
        if (!urlJson.isEmpty()) {
            filmeLaden2(seite, arr, urlJson, titel);
        } else {
            ////
        }
    }

    void filmeLaden2(StringBuffer seite, String[] arr, String urlJson, String titel) {
        String url = "";
        String urlHttp = "";
        String beschreibung = "";
        String stichwoerter = "";
        long dauer = 0;
        final String MUSTER_URL = "arte_vp_url=\"";
        final String MUSTER_BESCHREIBUNG = "\"VDE\":\"";
        final String MUSTER_STICHWOERTER = "\"VTA\":[\"";
        final String MUSTER_URL_HTTP = "\"url\":\"http:";
        int pos1, pos2;
        if (Daten.filmeLaden.getStop()) {
            return;
        }
        meldung(urlJson);
        seite = getUrlIo.getUri_Utf(nameSenderMReader, urlJson, seite, "");
        if ((pos1 = seite.indexOf(MUSTER_BESCHREIBUNG)) != -1) {
            pos1 += MUSTER_BESCHREIBUNG.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                beschreibung = seite.substring(pos1, pos2);
            }
        }
        if ((pos1 = seite.indexOf(MUSTER_STICHWOERTER)) != -1) {
            pos1 += MUSTER_STICHWOERTER.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                stichwoerter = seite.substring(pos1, pos2);
            }
        }
        if ((pos1 = seite.indexOf(MUSTER_URL_HTTP)) != -1) {
            pos1 += MUSTER_URL_HTTP.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                urlHttp = seite.substring(pos1, pos2);
            }
        }
        if (!urlHttp.isEmpty()) {
            try {
                dauer = Long.parseLong(arr[3]) * 60;
            } catch (Exception ex) {
                dauer = 0;
            }
            // Datum ändern
            arr[4] = datumAendern(arr[4]);
            // arr = new String[]{url, bild, thema, dauer, datum, beschreibung};
            // DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String uurlRtmp,
            //     String datum, String zeit,
            //     long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
            DatenFilm film = new DatenFilm(nameSenderMReader, arr[2], arr[0], titel, "http:" + urlHttp, "" /*urlRtmp*/,
                    convertDatum(arr[4]), convertZeit(arr[4]), dauer, beschreibung, arr[1], ""/* imageUrl*/, stichwoerter.split("\",\""));
//            film.addKleineUrl(urlKlein, "");
//            film.arr[DatenFilm.FILM_URL_AUTH_NR] = authurl;
            addFilm(film);
        } else {
            ////
        }
    }

    String datumAendern(String datum) {
        // private static SimpleDateFormat sdfIn = new SimpleDateFormat("EEE, dd, MMM HH:mm", Locale.GERMANY);
        // "Freitag, 02. August um 12:41 Uhr"
        datum = datum.replace(" um", "");
        datum = datum.replace(" Uhr", "");
        return datum + jahr;
    }

    String convertDatum(String datum) {
        // "Freitag, 02. August um 12:41 Uhr"
        try {
            Date filmDate = sdfIn.parse(datum);
            datum = sdfDatum.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(799363221, Log.FEHLER_ART_PROG, "Arte.convertDatum", ex);
        }
        return datum;
    }

    String convertZeit(String datum) {
        // "Freitag, 02. August um 12:41 Uhr"
        try {
            Date filmDate = sdfIn.parse(datum);
            datum = sdfZeit.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(799363221, Log.FEHLER_ART_PROG, "Arte.convertDatum", ex);
        }
        return datum;
    }
}
