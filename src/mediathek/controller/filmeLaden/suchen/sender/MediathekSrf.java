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
        private MVStringBuilder film_website = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

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
            final String BASE_URL_JSON = "http://srf.ch/webservice/cvis/segment/";
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
                                    addFilme2(thema, urlWebsite, BASE_URL_JSON + url + "/.json", titel, datum, zeit);
                                } else {
                                    Log.fehlerMeldung(-499556023, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme", "keine URL: " + strUrlFeed);
                                }
                            }
                        }
                    } else {
                        System.out.println("RelativeUrls");
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-795638103, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme", ex);
            }
        }

        private void addFilme2(String thema, String urlWebsite, String urlFilm, String titel, String datum, String zeit) {


            meldung(urlFilm);
            seite2 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite2, "");
            try {


                String[] keywords = extractKeywords(seite2);
                String thumbOrImage = extractThumbnail(seite2);
                long duration = extractDuration(seite2);
                String description = extractDescription(seite2);
                String title = extractTitle(seite2);
                String urlHd = extractHdUrl(seite2, urlWebsite);
                String url_normal = extractUrl(seite2);
                String url_small = extractSmallUrl(seite2);






                urlHd = urlHd.isEmpty() ? getHdUrlFromM3u8(seite2) : urlHd;
//                url_normal = url_normal.isEmpty() ? getNormalUrlFromM3u8(seite2) : url_normal;
//                url_small = url_small.isEmpty() ? getSmallUrlFromM3u8(seite2) : url_small;

                //For HD films, normal or small RTMP urls don't work
                url_normal = (isHdAvailable(seite2) || url_normal.isEmpty()) ? getNormalUrlFromM3u8(seite2) : url_normal;
                url_small = (isHdAvailable(seite2) || url_small.isEmpty()) ? getSmallUrlFromM3u8(seite2) : url_small;

                if (url_normal.isEmpty()) {
                    Log.fehlerMeldung(-159873540, Log.FEHLER_ART_MREADER, "MediathekSRf.filmLaden", "keine NORMALE Url für: " + urlWebsite + " : " + url_normal);
                }

                DatenFilm film = new DatenFilm(nameSenderMReader, thema, urlWebsite, title, url_normal, ""/*rtmpURL*/, datum, zeit, duration, description,
                        thumbOrImage, keywords);

                if (!url_small.isEmpty()) {
                    film.addUrlKlein(url_small, "");
                } else {
                    Log.fehlerMeldung(-159873540, Log.FEHLER_ART_MREADER, "MediathekArd.SRF", "keine kleine Url für: " + urlWebsite + " : " + url_normal);
                }
                if (!urlHd.isEmpty()) {
                    film.addUrlHd(urlHd, "");
                }
                addFilm(film);
            } catch (Exception ex) {
                Log.fehlerMeldung(-556320087, Log.FEHLER_ART_MREADER, "MediathekSf.addFilme2", ex);
            }
        }

        private String getSmallUrlFromM3u8(MVStringBuilder page) {
            final String PATTERN_QUALITY_100 = "\"quality\":\"100\",";
            final String PATTERN_RESOLUTION = "RESOLUTION=320x180";
            final String INDEX_2 = "index_0_av.m3u8";

            final String PATTERN_URL = "\"url\":\"";
            final String PATTERN_URL_END = "\"";


            String m3u8Url = normalizeJsonUrl(subString(PATTERN_QUALITY_100, PATTERN_URL, PATTERN_URL_END, page));
            if (m3u8Url.isEmpty()) {
                return m3u8Url;
            }
            return getUrlFromM3u8(m3u8Url, PATTERN_RESOLUTION, INDEX_2);
        }

        private String getNormalUrlFromM3u8(MVStringBuilder page) {
            final String PATTERN_QUALITY_100 = "\"quality\":\"100\",";
            final String PATTERN_RESOLUTION = "RESOLUTION=640x360";
            final String INDEX_2 = "index_2_av.m3u8";

            final String PATTERN_URL = "\"url\":\"";
            final String PATTERN_URL_END = "\"";


            String m3u8Url = normalizeJsonUrl(subString(PATTERN_QUALITY_100, PATTERN_URL, PATTERN_URL_END, page));
            if (m3u8Url.isEmpty()) {
                return m3u8Url;
            }
            return getUrlFromM3u8(m3u8Url, PATTERN_RESOLUTION, INDEX_2);
        }

        private String getHdUrlFromM3u8(MVStringBuilder page) {
            final String PATTERN_QUALITY_200 = "\"quality\":\"200\",";
            final String PATTERN_RESOLUTION = "RESOLUTION=1280x720";
            final String PATTERN_URL = "\"url\":\"";
            final String PATTERN_URL_END = "\"";

            final String INDEX_5 = "index_5_av.m3u8";


            String m3u8Url = normalizeJsonUrl(subString(PATTERN_QUALITY_200, PATTERN_URL, PATTERN_URL_END, page));
            if (m3u8Url.isEmpty()) {
                return m3u8Url;
            }

            return getUrlFromM3u8(m3u8Url, PATTERN_RESOLUTION, INDEX_5);

        }

        private String getUrlFromM3u8(String m3u8Url, String resolutionPattern, String qualityIndex) {
            final String PATTERN_URL = "http://";
            final String PATTERN_URL_END = "?null";

            final String CSMIL = "csmil/";
            String playlist = "";
            String url = "";
            seite3 = getUrl.getUri_Utf(nameSenderMReader, m3u8Url, seite3, "");
            if (seite3.length() != 0) {
                playlist = subString(resolutionPattern, PATTERN_URL, PATTERN_URL_END, seite3);
                if (playlist.isEmpty()) {
                    if (seite3.indexOf(qualityIndex) != -1) {
                        if (m3u8Url.contains(CSMIL)) {
                            url = m3u8Url.substring(0, m3u8Url.indexOf(CSMIL)) + CSMIL + qualityIndex;
                            if (url.contains(PATTERN_URL)) {
                                return url;
                            }
                        } else {
                            url = PATTERN_URL + url;
                        }
                        return url;
                    }

                } else {
                    playlist = PATTERN_URL + playlist;
                }

            } else {
                Log.fehlerMeldung(-362514789, Log.FEHLER_ART_MREADER, "MediathekSf.getUrlFromm3u8", "keine Seite gefunden" + m3u8Url);
            }

            return playlist;
        }

        private String extractHdUrl(MVStringBuilder page, String urlWebsite) {

//            final String PATTERN_HD_WIDTH = "\"frame_width\":1280";
//            final String PATTERN_QUALITY_200 = "\"quality\":\"200\",";

            final String PATTERN_DL_URL_START = "button_download_img offset\" href=\"";
            final String PATTERN_DL_URL_END = "\"";

            if (isHdAvailable(page)) {
                film_website = getUrl.getUri_Utf(nameSenderMReader, urlWebsite, film_website, "");

                String dlUrl = subString(PATTERN_DL_URL_START, PATTERN_DL_URL_END, film_website);
                return dlUrl;
            }
            return "";
        }

        private boolean isHdAvailable(MVStringBuilder page) {
            final String PATTERN_HD_WIDTH = "\"frame_width\":1280";
            final String PATTERN_QUALITY_200 = "\"quality\":\"200\",";

            if ((page.indexOf(PATTERN_HD_WIDTH) != -1) || page.indexOf(PATTERN_QUALITY_200) != -1) {
                return true;
            }

            return false;
        }

        private String extractUrl(MVStringBuilder page) {
            final String PATTERN_WIDTH_640 = "\"frame_width\":640";

            final String PATTERN_URL = "\"url\":\"";
            final String PATTERN_URL_END = "\"";

            return normalizeJsonUrl(subString(PATTERN_WIDTH_640, PATTERN_URL, PATTERN_URL_END, page));
        }

        private String extractSmallUrl(MVStringBuilder page) {
            final String PATTERN_WIDTH_320 = "\"frame_width\":320";
            final String PATTERN_WIDTH_384 = "\"frame_width\":384";
            final String PATTERN_URL = "\"url\":\"";
            final String PATTERN_URL_END = "\"";

//            String url = subString(PATTERN_WIDTH_320, PATTERN_URL, PATTERN_URL_END, page);
//            if (url.isEmpty()) {
//                url = subString(PATTERN_WIDTH_384, PATTERN_URL, PATTERN_URL_END, page);
//            }
//            return normalizeJsonUrl(url);

            String url = subString(PATTERN_WIDTH_320, PATTERN_URL, PATTERN_URL_END, page);
            if (url.isEmpty()) {
                url = subString(PATTERN_WIDTH_384, PATTERN_URL, PATTERN_URL_END, page);
            }
            return normalizeJsonUrl(url);

        }

        private long extractDuration(MVStringBuilder page) {
            int pos1, pos2;
            long duration = 0;
            final String PATTERN_DURATION = "\"mark_out\":";

            if ((pos1 = page.indexOf(PATTERN_DURATION)) != -1) {
                pos1 += PATTERN_DURATION.length();
                if ((pos2 = page.indexOf(",", pos1)) != -1) {
                    int pos3 = page.indexOf(".", pos1);
                    if (pos3 != -1 && pos3 < pos2) {
                        // we need to strip the . decimal divider
                        pos2 = pos3;
                    }
                    try {
                        String d = page.substring(pos1, pos2);
                        if (!d.isEmpty()) {
                            duration = Long.parseLong(d);
                        }
                    } catch (Exception ex) {
                        Log.fehlerMeldung(-646490237, Log.FEHLER_ART_MREADER, "MediathekSf.extractDuration", ex);
                    }
                }
            }
            return duration;
        }

        private String extractThumbnail(MVStringBuilder page) {


            final String PATTERN_ID = "\"id\":\"";
            final String PATTERN_ID_END = "\",";


            String id = subString(PATTERN_ID, PATTERN_ID_END, page);
            String thumbnail = "http://www.srf.ch/webservice/cvis/segment/thumbnail/" + id + "?width=150";


            return thumbnail;
        }

        private String extractDescription(MVStringBuilder page) {
            final String PATTERN_DESCRIPTION = "\"description_lead\":\"";
            final String PATTERN_DESC_END = "\",";
            final String PATTERN_DESC_ALTERNATIVE = "\"description\":\"";



            String description = subString(PATTERN_DESCRIPTION, PATTERN_DESC_END, page);
            if (description.isEmpty()) {
                description = subString(PATTERN_DESC_ALTERNATIVE, PATTERN_DESC_END, page);
            }



            return StringEscapeUtils.unescapeJava(description).trim();
        }

        private String extractTitle(MVStringBuilder page) {


            final String PATTERN_TITLE = "\"description_title\":\"";
            final String PATTERN_TITLE_END = "\",";

            String title = subString(PATTERN_TITLE, PATTERN_TITLE_END, page);
            return StringEscapeUtils.unescapeJava(title).trim();
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

        private String subString(String searchPattern, String patternStart, String patternEnd, MVStringBuilder page) {
            int posSearch, pos1, pos2;
            String extracted = "";
            if ((posSearch = page.indexOf(searchPattern)) != -1) {
                if ((pos1 = page.indexOf(patternStart, posSearch)) != -1) {
                    pos1 += patternStart.length();

                    if ((pos2 = page.indexOf(patternEnd, pos1)) != -1) {
                        extracted = page.substring(pos1, pos2);

                    }
                }
            }
            return extracted;
        }

        private String subString(String patternStart, String patternEnd, MVStringBuilder page) {
            int pos1, pos2;
            String extracted = "";
            if ((pos1 = page.indexOf(patternStart)) != -1) {
                pos1 += patternStart.length();
                if ((pos2 = page.indexOf(patternEnd, pos1)) != -1) {
                    extracted = page.substring(pos1, pos2);
                }
            }
            return extracted;
        }

        private String normalizeJsonUrl(String jsonurl) {
            final String SEARCH_PATTERN = "\\/";
            final String REPLACE_PATTERN = "/";

            return jsonurl.replace(SEARCH_PATTERN, REPLACE_PATTERN);
        }
    }
}