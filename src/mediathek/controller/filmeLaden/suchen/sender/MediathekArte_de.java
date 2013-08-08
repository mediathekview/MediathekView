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
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.MVStringBuilder;

public class MediathekArte_de extends MediathekReader implements Runnable {

    public static final String SENDER_ARTE_DE = "ARTE.DE";
    public static final String SENDER_ARTE_FR = "ARTE.FR";
    // "Freitag, 02. August um 12:41 Uhr"
    SimpleDateFormat sdfZeit = new SimpleDateFormat("HH:mm:ss");
    SimpleDateFormat sdfDatum = new SimpleDateFormat("dd.MM.yyyy");
    String URL_ARTE = "http://www.arte.tv/papi/tvguide/epg/schedule/D/L3/";

    public MediathekArte_de(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER_ARTE_DE, /* threads */ 2, /* urlWarten */ 500, startPrio);
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
        addTage();
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                //new Thread(new ThemaLaden()).start();
                Thread th = new Thread(new ThemaLaden());
                th.setName(nameSenderMReader + t);
                th.start();
            }
        }
    }

    private void addTage() {
        // http://www.arte.tv/papi/tvguide/epg/schedule/D/L3/2013-08-04/2013-8-04.json
        Date d = new Date();
        String out1, out2, u;
        SimpleDateFormat formatter1 = new SimpleDateFormat("yyyy-MM-dd");
        SimpleDateFormat formatter2 = new SimpleDateFormat("yyyy-M-dd");
        for (int i = 1; i <= 14; ++i) {
            out1 = formatter1.format(new Date(d.getTime() - i * (1000 * 60 * 60 * 24)));
            out2 = formatter2.format(new Date(d.getTime() - i * (1000 * 60 * 60 * 24)));
            u = URL_ARTE + out1 + "/" + out2 + ".json";
            listeThemen.add(new String[]{u});
        }
    }

    class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private MVStringBuilder seite1 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        private MVStringBuilder seite2 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public void run() {
            try {
                meldungAddThread();
                String link[];
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    meldungProgress(link[0] /* url */);
                    addTheman(seite1, seite2, link[0]);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-894330854, Log.FEHLER_ART_MREADER, "MediathekHr.ThemaLaden.run", ex, "");
            }
            meldungThreadUndFertig();
        }
    }

    private void addTheman(MVStringBuilder seite1, MVStringBuilder seite2, String startUrl) {
        // Datum, Zeit: "BAD":"04/08/2013","BAT":"13:20"
        final String MUSTER_START = "{\"programId\":";
        final String MUSTER_URL_JSON = "\"videoStreamUrl\":\"";
        final String MUSTER_DATUM = "\"BAD\":\"";
        final String MUSTER_ZEIT = "\"BAT\":\"";
        final String MUSTER_TITEL = "\"TIT\":\"";
        final String MUSTER_THEMA = "\"GEN\":\"";
        String[] arr;
        seite1 = getUrlIo.getUri_Utf(nameSenderMReader, startUrl, seite1, "");
        int posStart = 0, posStop = 0;
        int pos1;
        int pos2;
        String urlJson;
        String datum;
        String zeit;
        String titel, thema;
        int count = 1;
        while ((posStart = seite1.indexOf(MUSTER_START, posStart)) != -1) {
            ++count;
            posStart += MUSTER_START.length();
            posStop = seite1.indexOf(MUSTER_START, posStart);
            urlJson = "";
            datum = "";
            zeit = "";
            titel = "";
            thema = "";
            if ((pos1 = seite1.indexOf(MUSTER_URL_JSON, posStart)) != -1) {
                pos1 += MUSTER_URL_JSON.length();
                if (posStop == -1 || pos1 < posStop) {
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        urlJson = seite1.substring(pos1, pos2);
                    }
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_DATUM, posStart)) != -1) {
                pos1 += MUSTER_DATUM.length();
                if (posStop == -1 || pos1 < posStop) {
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        datum = seite1.substring(pos1, pos2);
                    }
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_ZEIT, posStart)) != -1) {
                pos1 += MUSTER_ZEIT.length();
                if (posStop == -1 || pos1 < posStop) {
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        zeit = seite1.substring(pos1, pos2);
                    }
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_TITEL, posStart)) != -1) {
                pos1 += MUSTER_TITEL.length();
                if (posStop == -1 || pos1 < posStop) {
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        titel = seite1.substring(pos1, pos2);
                    }
                }
            }
            if ((pos1 = seite1.indexOf(MUSTER_THEMA, posStart)) != -1) {
                pos1 += MUSTER_THEMA.length();
                if (posStop == -1 || pos1 < posStop) {
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        thema = seite1.substring(pos1, pos2);
                    }
                }
            }
            if (!urlJson.isEmpty()) {
                arr = new String[]{urlJson, datum, zeit, titel, thema};
                filmeLaden(seite2, arr);
            } else {
                Log.fehlerMeldung(-956230147, Log.FEHLER_ART_MREADER, "MediathekArte_de.addThemen", "Keine URL: " + startUrl + "**" + count);
            }
        }
    }

    void filmeLaden(MVStringBuilder seite, String[] arr) {
        // url_hd url, url_klein
        //{"version":"VOF","versionProg":"1","VFO":"HBBTV","VQU":"SQ","VMT":"mp4","VUR":"http://artestras.vo.llnwxd.net/o35/nogeo/HBBTV/042975-013-B_EXT_SQ_2_VOF_00604879_MP4-2200_AMM-HBBTV_EXTRAIT.mp4"},
        //{"version":"VOF","versionProg":"1","VFO":"HBBTV","VQU":"EQ","VMT":"mp4","VUR":"http://artestras.vo.llnwxd.net/o35/nogeo/HBBTV/042975-013-B_EXT_EQ_2_VOF_00604878_MP4-1500_AMM-HBBTV_EXTRAIT.mp4"},
        //{"version":"VOF","versionProg":"1","VFO":"HBBTV","VQU":"HQ","VMT":"mp4","VUR":"http://artestras.vo.llnwxd.net/o35/nogeo/HBBTV/042975-013-B_EXT_HQ_2_VOF_00604876_MP4-800_AMM-HBBTV_EXTRAIT.mp4"},

        String datum = "", zeit = "";
        String urlHd = "", urlKlein = "", url = "";
        String beschreibung = "";
//        String stichwoerter = "";
        String bild = "";
        String filmWebsite = "";
        String dauerStr = "";
        String titel = "", thema = "";
        long dauer = 0;
        final String MUSTER_BILD = "programImage\":\"";
        final String MUSTER_BESCHREIBUNG = "\"VDE\":\"";
        final String MUSTER_STICHWOERTER = "\"VTA\":[\"";
        final String MUSTER_FILM_WEBSITE = "\"VUP\":\"";
        final String MUSTER_URL_HD = "\"HBBTV\",\"VQU\":\"SQ\",\"VMT\":\"mp4\",\"VUR\":\"";
        final String MUSTER_URL = "HBBTV\",\"VQU\":\"EQ\",\"VMT\":\"mp4\",\"VUR\":\"";
        final String MUSTER_URL_KLEIN = "HBBTV\",\"VQU\":\"HQ\",\"VMT\":\"mp4\",\"VUR\":\"";
        final String MUSTER_DAUER = "\"videoDurationSeconds\":";
        int pos1, pos2;
        if (Daten.filmeLaden.getStop()) {
            return;
        }
        meldung(arr[0]);
        seite = getUrlIo.getUri_Utf(nameSenderMReader, arr[0], seite, "");
        if ((pos1 = seite.indexOf(MUSTER_BILD)) != -1) {
            pos1 += MUSTER_BILD.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                bild = seite.substring(pos1, pos2);
            }
        }
        if ((pos1 = seite.indexOf(MUSTER_BESCHREIBUNG)) != -1) {
            pos1 += MUSTER_BESCHREIBUNG.length();
            if ((pos2 = seite.indexOf(",", pos1)) != -1) {
                beschreibung = seite.substring(pos1, pos2);
                if (!beschreibung.isEmpty() && beschreibung.endsWith("\"")) {
                    beschreibung = beschreibung.substring(0, beschreibung.length() - 2);
                }
            }
        }
//        if ((pos1 = seite.indexOf(MUSTER_STICHWOERTER)) != -1) {
//            pos1 += MUSTER_STICHWOERTER.length();
//            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
//                ///////
//                stichwoerter = seite.substring(pos1, pos2);
//            }
//        }
        if ((pos1 = seite.indexOf(MUSTER_FILM_WEBSITE)) != -1) {
            pos1 += MUSTER_FILM_WEBSITE.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                filmWebsite = seite.substring(pos1, pos2);
            }
        }
        if ((pos1 = seite.indexOf(MUSTER_URL_HD)) != -1) {
            pos1 += MUSTER_URL_HD.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                urlHd = seite.substring(pos1, pos2);
            }
        }
        if ((pos1 = seite.indexOf(MUSTER_URL_KLEIN)) != -1) {
            pos1 += MUSTER_URL_KLEIN.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                urlKlein = seite.substring(pos1, pos2);
            }
        }
        if ((pos1 = seite.indexOf(MUSTER_URL)) != -1) {
            pos1 += MUSTER_URL.length();
            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                url = seite.substring(pos1, pos2);
            }
        }
        if ((pos1 = seite.indexOf(MUSTER_DAUER)) != -1) {
            pos1 += MUSTER_DAUER.length();
            if ((pos2 = seite.indexOf(",", pos1)) != -1) {
                dauerStr = seite.substring(pos1, pos2);
                if (!dauerStr.isEmpty()) {
                    try {
                        dauer = Long.parseLong(dauerStr);
                    } catch (Exception ex) {
                        dauer = 0;
                    }

                }
            }
        }
        // Datum ändern
        // arr = new String[]{urlJson, datum, zeit, titel, thema};

        datum = convertDatum(arr[1]);
        zeit = convertZeit(arr[2]);
        titel = arr[3];
        thema = arr[4];
        if (!url.isEmpty()) {
            //    public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String uurlRtmp,
            //         String datum, String zeit, long dauerSekunden, String description, String thumbnailUrl, String imageUrl, String[] keywords) {

            DatenFilm film = new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, url, "" /*urlRtmp*/,
                    datum, zeit, dauer, beschreibung, bild, ""/* imageUrl*/, new String[]{});
            if (!urlKlein.isEmpty()) {
                film.addUrlKlein(urlKlein, "");
            }
            if (!urlHd.isEmpty()) {
                film.addUrlHd(urlHd, "");
            }
            addFilm(film);
        } else if (!urlKlein.isEmpty()) {
            DatenFilm film = new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, urlKlein, "" /*urlRtmp*/,
                    datum, zeit, dauer, beschreibung, bild, ""/* imageUrl*/, new String[]{});
            if (!urlHd.isEmpty()) {
                film.addUrlHd(urlHd, "");
            }
            addFilm(film);
        } else if (!urlHd.isEmpty()) {
            DatenFilm film = new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, urlHd, "" /*urlRtmp*/,
                    datum, zeit, dauer, beschreibung, bild, ""/* imageUrl*/, new String[]{});
            addFilm(film);
        } else {
            Log.fehlerMeldung(-963025874, Log.FEHLER_ART_MREADER, "MediathekArte_de.filmeLaden", "Keine URL: " + arr[0]);
        }
    }

    String convertDatum(String datum) {
        // "BAD":"04/08/2013","BAT":"13:20"
        return datum.replace("/", ".");
    }

    String convertZeit(String zeit) {
        // "BAD":"04/08/2013","BAT":"13:20"
        return zeit + ":00";
    }
}
