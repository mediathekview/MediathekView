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
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.MVStringBuilder;

public class MediathekBr extends MediathekReader implements Runnable {

    public static final String SENDER = "BR";
    private SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.GERMANY);         // "date": "2013-07-06 13:00:00", 

    public MediathekBr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        //new Thread(new ThemaLaden()).start();
        Thread th = new Thread(new ThemaLaden());
        th.setName(nameSenderMReader);
        th.start();
    }

    private class ThemaLaden implements Runnable {

        @Override
        public synchronized void run() {
            meldungStart();
            meldungAddMax(1);
            meldungAddThread();
            try {
                jsonSuchen();
            } catch (Exception ex) {
                Log.fehlerMeldung(-203069877, Log.FEHLER_ART_MREADER, "MediathekBr.JsonLaden.run", ex, "");
            }
            meldungThreadUndFertig();
        }
    }

    private void jsonSuchen() {
        //"date": "2013-07-13 13:00:00", 
        //"description": "Pflanzentricks - Wie erreicht eine Pflanze, was sie will?", 
        //"duration": "25", 
        //"image": "http://mediathek-video.br.de/listra/sendungsbilder/xenius_xl.jpg", 
        //"title": "X:enius", 
        //"videos": {
        //"l": {
        //  "bitrate": 700000, 
        //  "url": "http://hbbtv.b7.br.gl-systemhaus.de/b7/konks/1373715065-b7konks_nc_203101728_107154.mp4"
        //}, 
        //"m": {
        //  "bitrate": 200000, 
        //  "url": "http://hbbtv.b7.br.gl-systemhaus.de/b7/konks/1373715063-b7konks_nc_203101728_107153.mp4"
        //}, 
        //"xl": {
        //  "bitrate": 2000000, 
        //  "url": "http://hbbtv.b7.br.gl-systemhaus.de/b7/konks/1373715066-b7konks_nc_203101728_107155.mp4"
        //}
        final String URL_JSON = "http://rd.gl-systemhaus.de/br/b7/nc/jsonp/latestarchive.json";
        final String DATE = "\"date\": \"";
        final String DESCRIPTION = "\"description\": \"";
        final String DURATION = "\"duration\": \"";
        final String IMAGE = "\"image\": \"";
        final String THEMA = "\"title\": \"";
        final String URL__ = "\"xl\":";
        final String URL = "\"url\": \"";
        final String URL_KLEIN__ = "\"l\":";
        final String URL_KLEIN = "\"url\": \"";
        String date, datum, zeit, thema, titel, description, duration, image, url, url_klein;
        long dauer;
        MVStringBuilder seite1 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        seite1 = getUrl.getUri(nameSenderMReader, URL_JSON, Konstanten.KODIERUNG_UTF, 3/*max Versuche*/, seite1, URL_JSON);
        if (seite1.length() == 0) {
            Log.fehlerMeldung(-302590789, Log.FEHLER_ART_MREADER, "MediathekBr.jsonSuchen", "Leere Seite: " + URL_JSON);
            return;
        }
        int pos;
        int pos1;
        int pos2;
        pos = 0;
        while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(DATE, pos)) != -1) {
            date = "";
            datum = "";
            zeit = "";
            thema = "";
            titel = "";
            description = "";
            duration = "";
            image = "";
            url = "";
            url_klein = "";
            dauer = 0;
            try {
                pos += DATE.length();
                pos1 = pos;
                if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                    date = seite1.substring(pos1, pos2);
                    datum = convertDatumJson(date);
                    zeit = convertZeitJson(date);
                }
                if ((pos1 = seite1.indexOf(DESCRIPTION, pos)) != -1) {
                    pos1 += DESCRIPTION.length();
                    final String TRENNER = " - ";
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        description = seite1.substring(pos1, pos2);
                        if (description.contains(TRENNER)) {
                            titel = description.substring(0, description.indexOf(TRENNER)).trim();
                            description = description.substring(description.indexOf(TRENNER) + TRENNER.length()).trim();
                        } else if (description.length() > 25) {
                            titel = description.substring(0, 25).trim() + "...";
                        } else {
                            titel = description;
                            description = "";
                        }
                    }
                }
                if ((pos1 = seite1.indexOf(DURATION, pos)) != -1) {
                    pos1 += DURATION.length();
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        duration = seite1.substring(pos1, pos2);
                        if (!duration.equals("")) {
                            try {
                                dauer = Long.parseLong(duration) * 60;
                            } catch (Exception ex) {
                                Log.fehlerMeldung(-304973047, Log.FEHLER_ART_MREADER, "MediathekBR.jsonSuchen", ex, "duration: " + duration);
                            }
                        }
                    }
                }
                if ((pos1 = seite1.indexOf(THEMA, pos)) != -1) {
                    pos1 += THEMA.length();
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        thema = seite1.substring(pos1, pos2);
                    }
                }
                if ((pos1 = seite1.indexOf(IMAGE, pos)) != -1) {
                    pos1 += IMAGE.length();
                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                        image = seite1.substring(pos1, pos2);
                    }
                }
                if ((pos1 = seite1.indexOf(URL__, pos)) != -1) {
                    if ((pos1 = seite1.indexOf(URL, pos1)) != -1) {
                        pos1 += URL.length();
                        if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                            url = seite1.substring(pos1, pos2);
                        }
                    }
                }
                if ((pos1 = seite1.indexOf(URL_KLEIN__, pos)) != -1) {
                    if ((pos1 = seite1.indexOf(URL_KLEIN, pos1)) != -1) {
                        pos1 += URL_KLEIN.length();
                        if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                            url_klein = seite1.substring(pos1, pos2);
                        }
                    }
                }
                if (url.equals("")) {
                    continue;
                }
                thema = GuiFunktionen.utf8(thema);
                titel = GuiFunktionen.utf8(titel);
                description = GuiFunktionen.utf8(description);
                // public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String datum, String zeit,
                //   long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
                DatenFilm film = new DatenFilm(nameSenderMReader, thema, "http://www.br.de/mediathek/index.html", titel, url, ""/*rtmpURL*/, datum, zeit,
                        dauer, description, "", image, new String[]{});
                film.addUrlKlein(url_klein, "");
                addFilm(film);
                meldung(film.arr[DatenFilm.FILM_URL_NR]);
            } catch (Exception ex) {
                Log.fehlerMeldung(-902483073, Log.FEHLER_ART_MREADER, "MediathekBR.jsonSuchen", ex, "");
            }
        }
    }

    private String convertDatumJson(String datum) {
        try {
            return new SimpleDateFormat("dd.MM.yyyy").format(sdfIn.parse(datum));
        } catch (Exception ex) {
            Log.fehlerMeldung(-963297249, Log.FEHLER_ART_MREADER, "MediathekBR.convertDatum", ex);
        }
        return "";
    }

    private String convertZeitJson(String datum) {
        try {
            return new SimpleDateFormat("HH:mm:ss").format(sdfIn.parse(datum));
        } catch (Exception ex) {
            Log.fehlerMeldung(-963297249, Log.FEHLER_ART_MREADER, "MediathekBR.convertDatum", ex);
        }
        return "";
    }

    public String convertDatum(String datum) {
        //      <beginnPlan>2010-12-09T10:55:00</beginnPlan>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("dd.MM.yyyy");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(-210365944, Log.FEHLER_ART_MREADER, "MediathekBr.convertDatum", ex, "");
        }
        return datum;
    }

    public String convertTime(String datum) {
        //      <beginnPlan>2010-12-09T10:55:00</beginnPlan>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("HH:mm:ss");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(-573690176, Log.FEHLER_ART_MREADER, "MediatheBr.convertTime", ex, "");
        }
        return datum;
    }
}
