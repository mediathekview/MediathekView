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

import java.util.LinkedList;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.MVStringBuilder;
import org.apache.commons.lang3.StringEscapeUtils;

public class MediathekSrf extends MediathekReader implements Runnable {

    public static final String SENDER = "SRF";
    private final int MAX_FILME_THEMA = 5;

    public MediathekSrf(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 1000, startPrio);
    }

    @Override
    public void addToList() {
        //Liste von http://www.videoportal.sf.tv/sendungen holen
        //<a class="sendung_name" href="/player/tv/sendung/1-gegen-100?id=6fd27ab0-d10f-450f-aaa9-836f1cac97bd">1 gegen 100</a>
        final String MUSTER = "sendung_name\" href=\"/player/tv";
        final String MUSTER_ID = "?id=";
        MVStringBuilder seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        listeThemen.clear();
        meldungStart();
        seite = getUrlIo.getUri_Utf(nameSenderMReader, "http://www.srf.ch/player/sendungen", seite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url;
        String thema = "";
        while ((pos = seite.indexOf(MUSTER, pos)) != -1) {
            pos += MUSTER.length();
            pos1 = pos;
            pos2 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
                if (url.contains(MUSTER_ID)) {
                    url = url.substring(url.indexOf(MUSTER_ID) + MUSTER_ID.length());
                } else {
                    url = "";
                }
                if (!url.equals("")) {
                    pos1 = seite.indexOf(">", pos);
                    pos2 = seite.indexOf("</a>", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        thema = seite.substring(pos1 + 1, pos2);
                    }
                    String[] add = new String[]{"http://www.videoportal.sf.tv/rss/sendung?id=" + url, thema};
                    listeThemen.addUrl(add);
                } else {
                    Log.fehlerMeldung(-198620778, Log.FEHLER_ART_MREADER, "MediathekSf.addToList", "keine URL");
                }
            }
        }
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

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private MVStringBuilder seite1 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        private MVStringBuilder seite2 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        private MVStringBuilder seite3 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public void run() {
            try {
                meldungAddThread();
                String link[];
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    meldungProgress(link[0] /* url */);
                    addFilme(link[1], link[0] /* url */);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-832002877, Log.FEHLER_ART_MREADER, "MediathekSf.SfThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }

        private void addFilme(String thema, String strUrlFeed) {
            // href="/player/tv/die-groessten-schweizer-talente/video/andrea-sutter-der-weg-ins-finale?id=06411758-1bd6-42ea-bc0e-cb8ebde3dfaa"
            // <title>Die grössten Schweizer Talente vom 17.03.2012, 20:11</title>
            // href="/player/tv/die-groessten-schweizer-talente/video/andrea-sutter-der-weg-ins-finale?id=06411758-1bd6-42ea-bc0e-cb8ebde3dfaa"&gt;Andrea Sutter - der Weg ins Finale&lt;/a&gt;&lt;/li&gt;
            // oder <link>http://www.srf.ch/player/tv/dok-panamericana/video/panamericana-vom-machu-piccu-in-peru-nach-bolivien-67?id=09f2cb4d-c5be-4809-9c9c-2d4cc703ad00</link>
            final String MUSTER_TITEL = "&gt;"; //bis zum &
            final String MUSTER_URL = "href=\"/player/tv"; //bis zum ;
            final String MUSTER_URL_NEU = "<link>http://www.srf.ch/player/tv"; //bis zum <
            final String MUSTER_ID = "?id=";
            final String MUSTER_ITEM_1 = "<item>";
            final String MUSTER_ITEM_2 = "</item>";
            final String MUSTER_DATUM = "<title>";
            meldung(strUrlFeed);
            seite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite1, "");
//            String s = seite.toString();
//            s = s.replace("&lt;", "<");
//            s = s.replace("&gt;", ">");
//            seite.setLength(0);
//            seite.append(s);
            try {
                int counter = 0;
                int posItem1 = 0;
                int posItem2 = 0;
                int pos = 0;
                int pos1;
                int pos2;
                String url;
                String urlWebsite;
                String datum = "";
                String zeit = "";
                String titel;
                String tmp;
                while (!Daten.filmeLaden.getStop() && (filmeSuchenSender.senderAllesLaden || counter < MAX_FILME_THEMA) && (posItem1 = seite1.indexOf(MUSTER_ITEM_1, posItem1)) != -1) {
                    posItem1 += MUSTER_ITEM_1.length();
//                    posItem2 = seite1.indexOf(MUSTER_ITEM_2, posItem1);
                    ++counter;
                    titel = "";
                    if ((pos1 = seite1.indexOf(MUSTER_DATUM, posItem1)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            tmp = seite1.substring(pos1, pos2);
                            if (tmp.contains("vom")) {
                                titel = tmp.substring(0, tmp.indexOf("vom")).trim();
                                tmp = tmp.substring(tmp.indexOf("vom") + 3);
                                if (tmp.contains(",")) {
                                    datum = tmp.substring(0, tmp.indexOf(",")).trim();
                                    zeit = tmp.substring(tmp.indexOf(",") + 1).trim() + ":00";
                                    titel = titel + " vom: " + datum;
                                }
                            }
                        }
                    }
                    if ((pos1 = seite1.indexOf(MUSTER_URL_NEU, posItem1)) != -1) {
                        // neu
                        pos1 += MUSTER_URL_NEU.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            url = seite1.substring(pos1, pos2);
                            if (url.contains(MUSTER_ID)) {
                                urlWebsite = "http://www.srf.ch/player/tv" + url;
                                url = url.substring(url.indexOf(MUSTER_ID) + MUSTER_ID.length());
                                if (!url.equals("")) {
                                    if (titel.equals("")) {
                                        titel = thema;
                                    }
                                    addFilme2(thema, urlWebsite, "http://www.videoportal.sf.tv/cvis/segment/" + url + "/.json", titel, datum, zeit);
                                } else {
                                    Log.fehlerMeldung(-499556023, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme", "keine URL: " + strUrlFeed);
                                }
                            }
                        }
                    } else {
                        if ((pos1 = seite1.indexOf(MUSTER_URL, posItem1)) != -1) {
                            pos1 += MUSTER_URL.length();
                            if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                                url = seite1.substring(pos1, pos2);
                                if (url.contains(MUSTER_ID)) {
                                    urlWebsite = "http://www.srf.ch/player/tv" + url;
                                    url = url.substring(url.indexOf(MUSTER_ID) + MUSTER_ID.length());
                                    if (!url.equals("")) {
                                        if ((pos1 = seite1.indexOf(MUSTER_TITEL, pos1)) != -1) {
                                            pos1 += MUSTER_TITEL.length();
                                            if ((pos2 = seite1.indexOf("&", pos1)) != -1) {
                                                titel = seite1.substring(pos1, pos2);
                                                addFilme2(thema, urlWebsite, "http://www.videoportal.sf.tv/cvis/segment/" + url + "/.json", titel, datum, zeit);
                                            } else {
                                                Log.fehlerMeldung(-499556023, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme", "keine URL: " + strUrlFeed);
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-795638103, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme", ex);
            }
        }

        private void addFilme2(String thema, String urlWebsite, String urlFilm, String titel, String datum, String zeit) {
            // "description_title":"Panamericana: Vom Machu Piccu in Peru nach Bolivien (6\/7)"
            // "description_title":"\u00abmyStory\u00bb \u2013 Mein Team (2\/4)",
            final String MUSTER_URL = "\"url\":\""; //bis zum "
            final String MUSTER_TITEL = "\"description_title\":\"";
            final String MUSTER_DURATION = "\"mark_out\":";
            //final String MUSTER_DESCRIPTION = "\"description_lead\":\"";
            final String MUSTER_DESCRIPTION = "\"description\":\"";
            final String MUSTER_ID = "\"segments\":[{\"id\":\"";
            String t = "";
            meldung(urlFilm);
            seite2 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite2, "");
            try {
                int pos1;
                int pos2;

                long duration = 0;
                String description = "";
                String thumbnail = "";
                String image = "";

                if ((pos1 = seite2.indexOf(MUSTER_DURATION)) != -1) {
                    pos1 += MUSTER_DURATION.length();
                    if ((pos2 = seite2.indexOf(",", pos1)) != -1) {
                        int pos3 = seite2.indexOf(".", pos1);
                        if (pos3 != -1 && pos3 < pos2) {
                            // we need to strip the . decimal divider
                            pos2 = pos3;
                        }
                        try {
                            String d = seite2.substring(pos1, pos2);
                            if (!d.equals("")) {
                                duration = Long.parseLong(d);
                            }
                        } catch (Exception ex) {
                            Log.fehlerMeldung(-646490237, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme2", ex);
                        }
                    }
                }

                if ((pos1 = seite2.indexOf(MUSTER_DESCRIPTION)) != -1) {
                    pos1 += MUSTER_DESCRIPTION.length();
                    if ((pos2 = seite2.indexOf("\",", pos1)) != -1) {
                        description = seite2.substring(pos1, pos2);
                        description = StringEscapeUtils.unescapeJava(description).trim();
                    }
                }

                if ((pos1 = seite2.indexOf(MUSTER_ID)) != -1) {
                    pos1 += MUSTER_ID.length();
                    if ((pos2 = seite2.indexOf("\",", pos1)) != -1) {
                        String id = seite2.substring(pos1, pos2);
                        thumbnail = "http://www.srf.ch/webservice/cvis/segment/thumbnail/" + id + "?width=150";
                        image = "http://www.srf.ch/webservice/cvis/segment/thumbnail/" + id;
                    }
                }

                String[] keywords = extractKeywords(seite2);
                if ((pos1 = seite2.indexOf(MUSTER_TITEL)) != -1) {
                    pos1 += MUSTER_TITEL.length();
                    if ((pos2 = seite2.indexOf("\"", pos1)) != -1) {
                        t = seite2.substring(pos1, pos2);
                        t = StringEscapeUtils.unescapeJava(t).trim();
                        if (!t.equals("")) {
                            titel = t;
                        }
                    }
                }
                if ((pos1 = seite2.indexOf(MUSTER_URL)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = seite2.indexOf("\"", pos1)) != -1) {
                        String url = seite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            url = url.replace("\\", "");
                            if (url.endsWith("m3u8")) {
                                String tmp = getUrlFrom_m3u8(url);
                                if (!tmp.equals("")) {
                                    url = tmp;
                                    // thema = "--- test ----";
                                } else {
                                    Log.fehlerMeldung(-312136970, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme2", "keine URL" + url);
                                }
                            }
                            // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                            // DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit);
                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, urlWebsite, titel, url, ""/*rtmpURL*/, datum, zeit, duration, description,
                                    image.isEmpty() ? thumbnail : image, keywords);
                            if (url.endsWith("_hq1.mp4")) {
                                String urlKlein = url.replace("_hq1.mp4", "_mq1.mp4");
                                film.addUrlKlein(urlKlein, "");
                            }
                            addFilm(film);
                        } else {
                            Log.fehlerMeldung(-698325618, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme2", "keine URL" + url);
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-556320087, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme2", ex);
            }
        }

        private String getUrlFrom_m3u8(String m3u8Url) {
            // http://srfvod-vh.akamaihd.net/i/vod/chfilmszene/2012/12/chfilmszene_20121213_001157_web_h264_16zu9_,lq1,mq1,hq1,.mp4.csmil/index_0_av.m3u8?null=&e=ace6e3bb3f9f8597
            // rtmp://cp50792.edgefcs.net/ondemand/mp4:aka/vod/chfilmszene/2012/12/chfilmszene_20121213_001157_web_h264_16zu9_hq1.mp4
            String url = "";
            final String MUSTER_URL = "http://";
            meldung(m3u8Url);
            seite3 = getUrl.getUri_Utf(nameSenderMReader, m3u8Url, seite3, "");
            if (seite3.length() == 0) {
                return "";
            }
            try {
                // Möglichkeit 1
                int pos1;
                int pos2;
                if ((pos1 = seite3.indexOf(MUSTER_URL)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = seite3.indexOf("?", pos1)) != -1) {
                        url = seite3.substring(pos1, pos2);
                        url = url.substring(url.indexOf("/vod/"));
                        url = "rtmp://cp50792.edgefcs.net/ondemand/mp4:aka" + url;
                        url = url.substring(0, url.indexOf("h264"));
                        url = url + "h264_16zu9_hq1.mp4";
                        if (url.equals("")) {
                            Log.fehlerMeldung(-362514789, Log.FEHLER_ART_MREADER, "MediathekSf.getUrlFrom_m3u8", "keine URL" + url);
                        }
                    }
                }
                if (url.isEmpty()) {
                    // Möglichkeit 2
                    // http://hdvodsrforigin-f.akamaihd.net/i/vod/coverme/2013/07/coverme_20130708_205743_web_h264_16zu9_,lq1,mq1,hq1,.mp4.csmil/master.m3u8
                    // index_2_av.m3u8?e=b471643725c47acd
                    // m3u-URL:
                    // http://cdn-vod-ios.br.de/i/mir-live/bw1XsLz.......A,B,.mp4.csmil/master.m3u8?__b__=200
                    final String URL = "index_2_av";
                    final String CSMIL = "csmil/";
                    if (m3u8Url.contains(CSMIL)) {
                        url = m3u8Url.substring(0, m3u8Url.indexOf(CSMIL)) + CSMIL;
                        url = url + URL + seite3.extract(URL, "\n");
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-827485890, Log.FEHLER_ART_MREADER, "MediathekSf.getUrlFrom_m3u8", ex);
            }
            return url;
        }

        private String[] extractKeywords(MVStringBuilder string) {
            LinkedList<String> l = new LinkedList<String>();

            /*	"tags": {
             "user": [],
             "editor": [{
             "name": "Show",
             "count": 1
             }, {
             "name": "Susanne Kunz",
             "count": 1
             }, {
             "name": "Quiz",
             "count": 1
             }, {
             "name": "1 gegen 100",
             "count": 1
             }]
             },*/

            final String PATTERN_TAGS_START = "\"tags\":{";
            final String PATTERN_TAGS_END = "]},";
            final String PATTERN_TAG_START = "\"name\":\"";

            int pos0 = string.indexOf(PATTERN_TAGS_START);
            if (pos0 != -1) {
                pos0 += PATTERN_TAGS_START.length();
                int pos1 = string.indexOf(PATTERN_TAGS_END, pos0);
                String tags = string.substring(pos0, pos1);
                pos0 = 0;
                while ((pos0 = tags.indexOf(PATTERN_TAG_START, pos0)) != -1) {
                    pos0 += PATTERN_TAG_START.length();
                    pos1 = tags.indexOf("\",", pos0);
                    if (pos1 != -1) {
                        String tag = tags.substring(pos0, pos1);
                        l.add(tag);
                    }
                }
            }
            return l.toArray(new String[l.size()]);
        }
    }
}
