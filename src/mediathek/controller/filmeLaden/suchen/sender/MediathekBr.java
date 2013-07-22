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

public class MediathekBr extends MediathekReader implements Runnable {

    public static final String SENDER = "BR";
    private SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.GERMANY);         // "date": "2013-07-06 13:00:00", 

    public MediathekBr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        new Thread(new JsonLaden()).start();
    }

    private class JsonLaden implements Runnable {

        @Override
        public synchronized void run() {
            meldungStart();
            meldungAddMax(1);
            meldungAddThread();
            try {
                jsonSuchen();
//                laden();
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
        StringBuffer seite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
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
                film.addKleineUrl(url_klein, "");
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

//    private void laden() {
//        getUrlIo.getDummy(nameSenderMReader);
//        //        <video application="a792/o16" host="gffstream.fcod.llnwd.net" groesse="xlarge" stream="br/b7/b7konks25277.mp4" typ="aufzeichnung"/>
//        StringBuilder seite = new StringBuilder(200 * 1024 * 8);
//        int pos = 0;
//        int posEnde;
//        int pos1;
//        int pos2;
//        String url, url_klein;
//        String thema;
//        String datum;
//        String zeit;
//        String titel;
//        String tmp;
//        String beschreibung;
//        final String ITEM_1 = "<ausstrahlung";
//        final String ITEM_2 = "</ausstrahlung>";
//        final String MUSTER_URL = "<video ";
//        final String MUSTER_THEMA = "<titel>";
//        final String MUSTER_TITEL = "<nebentitel>";
//        final String MUSTER_DATUM = "<beginnPlan>";
//        final String MUSTER_BESCHREIBUNG = "<beschreibung>";
//        final String MUSTER_KURZBESCHREIBUNG = "<kurzbeschreibung>";
//        //final String ADRESSE = "http://rd.gl-systemhaus.de/br/b7/archive/archive.xml.zip.adler32";
//        //final String ADRESSE = "http://rd.gl-systemhaus.de/br/b7/listra/archive/archive.xml.zip.adler32";
//        //final String ADRESSE="http://mediathek-video.br.de/nc/archive/archive-1328897218.xml.zip.adler32";
//        final String ADRESSE = "http://rd.gl-systemhaus.de/br/b7/nc/archive/archive.xml.zip.adler32";
//        meldungProgress(ADRESSE);
//        try {
//            InputStreamReader inReader;
//            int timeout = 30000;
//            char[] zeichen = new char[1];
//            URLConnection conn = new URL(ADRESSE).openConnection();
//            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
//            conn.setReadTimeout(timeout);
//            conn.setConnectTimeout(timeout);
//            ZipInputStream zipInputStream = new ZipInputStream(conn.getInputStream());
//            zipInputStream.getNextEntry();
//            inReader = new InputStreamReader(zipInputStream, Konstanten.KODIERUNG_UTF);
//            seite.setLength(0);
//            while (!Daten.filmeLaden.getStop() && inReader.read(zeichen) != -1) {
//                seite.append(zeichen);
//            }
//            while ((pos = seite.indexOf(ITEM_1, pos)) != -1) {
//                pos += ITEM_1.length();
//                if ((posEnde = seite.indexOf(ITEM_2, pos)) == -1) {
//                    break;
//                }
//                url = "";
//                url_klein = "";
//                thema = "";
//                datum = "";
//                zeit = "";
//                titel = "";
//                beschreibung = "";
//                pos1 = pos;
//                while (true) {
//                    pos1 = seite.indexOf(MUSTER_URL, pos1);
//                    if (pos1 == -1) {
//                        break;
//                    } else {
//                        pos1 += MUSTER_URL.length();
//                        if ((pos2 = seite.indexOf("/>", pos1)) != -1) {
//                            if (pos2 > posEnde) {
//                                break;
//                            }
//                            String tmpUrl = seite.substring(pos1, pos2);
//                            if (tmpUrl.contains("\"xlarge\"")) {
//                                url = tmpUrl;
//                                continue;
//                            }
//                            if (tmpUrl.contains("\"large\"")) {
//                                url_klein = tmpUrl;
//                                if (url.isEmpty()) {
//                                    url = tmpUrl;
//                                }
//                            }
//                            if (url.contains("\"xlarge\"") && url_klein.contains("\"large\"")) {
//                                break;
//                            }
//                        }
//                    }
//                }
//                if (url.equals(url_klein)) {
//                    // dann gibts nur "large"
//                    url_klein = "";
//                }
//                if (url.equals("")) {
//                    Log.fehlerMeldung(-978451398, Log.FEHLER_ART_MREADER, "MediathekBr.laden", "");
//                } else {
//                    if ((pos1 = seite.indexOf(MUSTER_BESCHREIBUNG, pos)) != -1) {
//                        pos1 += MUSTER_BESCHREIBUNG.length();
//                        if ((pos2 = seite.indexOf("</", pos1)) != -1) {
//                            if (pos2 < posEnde) {
//                                // <beschreibung><![CDATA[In dieser Woche sind zu Gast:auspieler]]></beschreibung>
//                                beschreibung = seite.substring(pos1, pos2);
//                                beschreibung = beschreibung.replace("<!", "");
//                                beschreibung = beschreibung.replace("[", "");
//                                beschreibung = beschreibung.replace("CDATA", "");
//                                beschreibung = beschreibung.replace("]", "");
//                                beschreibung = beschreibung.replace(">", "");
//                            }
//                        }
//                    }
//                    if (beschreibung.isEmpty()) {
//                        if ((pos1 = seite.indexOf(MUSTER_KURZBESCHREIBUNG, pos)) != -1) {
//                            pos1 += MUSTER_KURZBESCHREIBUNG.length();
//                            if ((pos2 = seite.indexOf("</", pos1)) != -1) {
//                                if (pos2 < posEnde) {
//                                    // <kurzbeschreibung><![CDATA[Pflanzen in unserer Umgebung]]></kurzbeschreibung>
//                                    beschreibung = seite.substring(pos1, pos2);
//                                    beschreibung = beschreibung.replace("<!", "");
//                                    beschreibung = beschreibung.replace("[", "");
//                                    beschreibung = beschreibung.replace("CDATA", "");
//                                    beschreibung = beschreibung.replace("]", "");
//                                    beschreibung = beschreibung.replace(">", "");
//                                }
//                            }
//                        }
//                    }
//                    if ((pos1 = seite.indexOf(MUSTER_THEMA, pos)) != -1) {
//                        pos1 += MUSTER_THEMA.length();
//                        if ((pos2 = seite.indexOf("</", pos1)) != -1) {
//                            if (pos1 < posEnde && pos2 < posEnde) {
//                                //      <titel><![CDATA[Vom Ahorn bis zur Zwiebel]]></titel>
//                                thema = seite.substring(pos1, pos2);
//                                thema = thema.replace("<!", "");
//                                thema = thema.replace("[", "");
//                                thema = thema.replace("CDATA", "");
//                                thema = thema.replace("]", "");
//                                thema = thema.replace(">", "");
//                            }
//                        }
//                    }
//                    if ((pos1 = seite.indexOf(MUSTER_TITEL, pos)) != -1) {
//                        pos1 += MUSTER_TITEL.length();
//                        if ((pos2 = seite.indexOf("</", pos1)) != -1) {
//                            if (pos1 < posEnde && pos2 < posEnde) {
//                                //      <nebentitel><![CDATA[Der Spargel]]></nebentitel>
//                                titel = seite.substring(pos1, pos2);
//                                titel = titel.replace("<!", "");
//                                titel = titel.replace("[", "");
//                                titel = titel.replace("CDATA", "");
//                                titel = titel.replace("]", "");
//                                titel = titel.replace(">", "");
//                            }
//                        }
//                    }
//                    if (titel.equals("")) {
//                        titel = thema;
//                    }
//                    if ((pos1 = seite.indexOf(MUSTER_DATUM, pos)) != -1) {
//                        pos1 += MUSTER_DATUM.length();
//                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
//                            if (pos1 < posEnde && pos2 < posEnde) {
//                                //      <beginnPlan>2010-12-09T10:55:00</beginnPlan>
//                                tmp = seite.substring(pos1, pos2);
//                                datum = convertDatum(tmp);
//                                zeit = convertTime(tmp);
//                            }
//                        }
//                    }
//                    // <video application="a792/o16" host="gffstream.fcod.llnwd.net" groesse="xlarge" stream="br/b7/b7konks25277.mp4" typ="aufzeichnung"/>
//                    // mnt/daten/software/bin/flvstreamer/flvstreamer --host gffstream.fcod.llnwd.net --app a792/o16/ --playpath mp4:br/b7/b7konks25241.mp4 -o film-3
//                    int p;
//                    String host = "";
//                    String app = "";
//                    String play = "", play_klein = "";
//                    if ((p = url.indexOf("host=\"")) != -1) {
//                        p += "host=\"".length();
//                        host = url.substring(p, url.indexOf("\"", p));
//                    }
//                    if ((p = url.indexOf("application=\"")) != -1) {
//                        p += "application=\"".length();
//                        app = url.substring(p, url.indexOf("\"", p));
//                    }
//                    if ((p = url.indexOf("stream=\"")) != -1) {
//                        p += "stream=\"".length();
//                        play = url.substring(p, url.indexOf("\"", p));
//                    }
//                    if ((p = url_klein.indexOf("stream=\"")) != -1) {
//                        p += "stream=\"".length();
//                        play_klein = url_klein.substring(p, url_klein.indexOf("\"", p));
//                    }
//                    //rtmp://gffstream.fcod.llnwd.net/a792/o16/br/b7/b7konks25460.mp4
//                    //mp4:konks/b7/listra/konks/1316073962-b7konks_listra_144931476_7292.mp4
//                    String urlRtmp = "--host " + host + " --app " + app + " --playpath " + play;
//                    String urlRtmp_klein = "--host " + host + " --app " + app + " --playpath " + play_klein;
//                    //String urlRtmp = "--host " + host + "/" + app + " --playpath " + play;
//                    String urlOrg = "rtmp://" + host + "/" + app + "/" + play;
//                    String urlOrg_klein = "rtmp://" + host + "/" + app + "/" + play_klein;
//                    if (urlOrg.startsWith("rtmp://cp121360")) {
//                        // die gehen nicht mehr:
//                        // rtmp://cp121360.edgefcs.net/ondemand/mp4:konks/b7/konks/1372853103-b7konks_nc_201799986_106447.mp4
//                        continue;
//                    }
//                    DatenFilm film = new DatenFilm(nameSenderMReader, thema, "http://www.br.de/mediathek/index.html", titel, urlOrg, urlRtmp, datum, zeit,
//                            0 /*duration*/, beschreibung, ""/*bild*/, ""/* imageUrl*/, new String[]{""});
//                    //DatenFilm film = new DatenFilm(nameSenderMReader, thema, "http://www.br.de/mediathek/index.html", titel, urlOrg, urlRtmp, datum, zeit);
//                    film.addKleineUrl(urlOrg_klein, urlRtmp_klein);
//                    addFilm(film);
//                }
//            } //while, die ganz große Schleife
//            // ====================================================
//            // und jetzt noch für Sendungen
//            final String FEED_1 = "<feed name=\"";
//            final String FEED_2 = "</feed>";
//            final String PODCAST = "<podcast";
//            final String TITEL = "<title>";
//            final String BESCHREIBUNG = "<description>";
//            final String BILD = "<image>";
//            final String DATUM = "<pubdate>";
//            final String DAUER = "<duration>";
//            final String URL = ">http://cdn-storage";
//            String bild;
//            String dauer;
//            String urlThema;
//            url_klein = "";
//            long duration = 0;
//            while ((pos = seite.indexOf(FEED_1, pos)) != -1) {
//                pos += FEED_1.length();
//                if ((posEnde = seite.indexOf(FEED_2, pos)) == -1) {
//                    break;
//                }
//                thema = "";
//                bild = "";
//                pos1 = pos;
//                if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
//                    thema = seite.substring(pos1, pos2);
//                    if (thema.isEmpty()) {
//                        thema = SENDER;
//                    }
//                }
//                if ((pos1 = seite.indexOf(BILD, pos)) != -1) {
//                    pos1 += BILD.length();
//                    if ((pos2 = seite.indexOf("<", pos1)) != -1) {
//                        if (pos2 > posEnde) {
//                            break;
//                        }
//                        bild = seite.substring(pos1, pos2);
//                    }
//                }
//                while ((pos1 = seite.indexOf(PODCAST, pos)) != -1) {
//                    if (pos1 > posEnde) {
//                        break;
//                    }
//                    pos = pos1 + PODCAST.length();
//                    url = "";
//                    datum = "";
//                    zeit = "";
//                    titel = "";
//                    beschreibung = "";
//                    dauer = "";
//                    if ((pos1 = seite.indexOf(TITEL, pos)) != -1) {
//                        pos1 += TITEL.length();
//                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
//                            if (pos2 > posEnde) {
//                                break;
//                            }
//                            titel = seite.substring(pos1, pos2);
//                        }
//                    }
//                    if ((pos1 = seite.indexOf(BESCHREIBUNG, pos)) != -1) {
//                        pos1 += BESCHREIBUNG.length();
//                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
//                            if (pos2 > posEnde) {
//                                break;
//                            }
//                            beschreibung = seite.substring(pos1, pos2);
//                        }
//                    }
//                    if ((pos1 = seite.indexOf(DAUER, pos)) != -1) {
//                        pos1 += DAUER.length();
//                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
//                            if (pos2 > posEnde) {
//                                break;
//                            }
//                            dauer = seite.substring(pos1, pos2);
//                            try {
//                                if (!dauer.equals("")) {
//                                    duration = 0;
//                                    String[] parts = dauer.split(":");
//                                    long power = 1;
//                                    for (int i = parts.length - 1; i >= 0; i--) {
//                                        duration += Long.parseLong(parts[i]) * power;
//                                        power *= 60;
//                                    }
//                                }
//                            } catch (Exception ex) {
//                                Log.fehlerMeldung(-963297054, Log.FEHLER_ART_MREADER, "MediathekBr.addFilm", "d: " + (dauer == null ? " " : dauer));
//                            }
//                        }
//                    }
//                    if ((pos1 = seite.indexOf(DATUM, pos)) != -1) {
//                        pos1 += DATUM.length();
//                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
//                            if (pos2 > posEnde) {
//                                break;
//                            }
//                            tmp = seite.substring(pos1, pos2);
//                            datum = convertDatum(tmp);
//                            zeit = convertTime(tmp);
//                        }
//                    }
//                    if ((pos1 = seite.indexOf(URL, pos)) != -1) {
//                        pos1 += URL.length();
//                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
//                            if (pos2 > posEnde) {
//                                break;
//                            }
//                            url = seite.substring(pos1, pos2);
//                        }
//                    }
//                    if (url.equals("")) {
//                        Log.fehlerMeldung(-397512398, Log.FEHLER_ART_MREADER, "MediathekBr.laden", "");
//                    } else {
//                        // DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String uurlRtmp,
//                        //     String datum, String zeit,
//                        //     long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
//                        url = "http://cdn-storage" + url;
//                        DatenFilm film = new DatenFilm(nameSenderMReader, thema, "http://www.br-online.de/podcast/", titel, url, "" /*urlRtmp*/, datum, zeit,
//                                duration, beschreibung, bild, ""/* imageUrl*/, new String[]{""});
//                        addFilm(film);
//                    }
//                }
//            } //while, die ganz große Schleife
//
//        } catch (Exception ex) {
//            Log.fehlerMeldung(-963486054, Log.FEHLER_ART_MREADER, "MediathekBr.laden", ex, "");
//        }
//    }

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
