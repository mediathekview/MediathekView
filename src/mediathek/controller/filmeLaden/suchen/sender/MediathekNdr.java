/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
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

public class MediathekNdr extends MediathekReader implements Runnable {

    public static final String SENDER = "NDR";
    private MVStringBuilder seiteAlle = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

    public MediathekNdr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 4, /* urlWarten */ 500, startPrio);
    }

    //-> erste Seite:
    // <h5><a href="/mediathek/mediatheksuche103_broadcast-30.html">Nordmagazin</a></h5>
    @Override
    void addToList() {
        //<broadcast id="1391" site="ndrfernsehen">45 Min</broadcast>
        final String ADRESSE = "http://www.ndr.de/mediathek/dropdown101-extapponly.html";
        final String MUSTER_URL1 = "<h5><a href=\"/mediathek/";
        listeThemen.clear();
        meldungStart();
        MVStringBuilder seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        seite = getUrlIo.getUri(nameSenderMReader, ADRESSE, Konstanten.KODIERUNG_UTF, 5 /* versuche */, seite, ""/* meldung */);
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        String thema = "";
        while ((pos = seite.indexOf(MUSTER_URL1, pos)) != -1) {
            try {
                pos += MUSTER_URL1.length();
                pos1 = pos;
                if ((pos2 = seite.indexOf("\"", pos)) != -1) {
                    url = seite.substring(pos1, pos2);
                }
                pos1 = seite.indexOf(">", pos);
                pos2 = seite.indexOf("<", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 < pos2) {
                    thema = seite.substring(pos1 + 1, pos2);
                }
                if (url.equals("")) {
                    Log.fehlerMeldung(-210367600, Log.FEHLER_ART_MREADER, "MediathekNdr.addToList", "keine Url");
                    continue;
                }
                String url_ = "http://www.ndr.de/mediathek/" + url;
                String[] add = new String[]{url_, thema};
                if (filmeSuchenSender.senderAllesLaden) {
                    if (!alleSeiteSuchen(url_, thema)) {
                        // dann halt so versuchen
                        listeThemen.addUrl(add);
                    }
                } else {
                    listeThemen.addUrl(add);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-332945670, Log.FEHLER_ART_MREADER, "MediathekNdr.finden", ex);
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

    private boolean alleSeiteSuchen(String strUrlFeed, String tthema) {
        boolean ret = false;
        seiteAlle = getUrlIo.getUri(nameSenderMReader, strUrlFeed, Konstanten.KODIERUNG_UTF, 3 /* versuche */, seiteAlle, "Thema: " + tthema/* meldung */);
        int pos1;
        int pos2;
        try {
            // http://www.ndr.de/mediathek/mediatheksuche103_broadcast-35.html
            // http://www.ndr.de/mediathek/mediatheksuche105_broadcast-35_format-video_page-1.html
            final String WEITER = "Alle zeigen (";
            if ((pos1 = seiteAlle.indexOf(WEITER)) != -1) {
                pos1 += WEITER.length();
                if ((pos2 = seiteAlle.indexOf(")", pos1)) != -1) {
                    String anz = seiteAlle.substring(pos1, pos2);
                    try {
                        int z = Integer.parseInt(anz);
                        for (int i = 1; i <= z / 10; ++i) {
                            // geht bei 2 los da das ja schon die erste Seite ist!
                            String url_ = strUrlFeed.replace(".html", "_format-video_page-" + String.valueOf(i) + ".html");
                            url_ = url_.replace("mediatheksuche103", "mediatheksuche105");
                            listeThemen.addUrl(new String[]{url_, tthema});
                            ret = true;
                        }
                    } catch (Exception ex) {
                        Log.fehlerMeldung(-913047821, Log.FEHLER_ART_MREADER, "MediathekNdr.feddEinerSeiteSuchen", strUrlFeed);
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(-643208979, Log.FEHLER_ART_MREADER, "MediathekNdr.feddEinerSeiteSuchen", strUrlFeed);
        }
        return ret;
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private MVStringBuilder seite1 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        private MVStringBuilder seite2 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    try {
                        meldungProgress(link[1]);
                        feedEinerSeiteSuchen(link[0], link[1] /* thema */);
                    } catch (Exception ex) {
                        Log.fehlerMeldung(-336901211, Log.FEHLER_ART_MREADER, "MediathekNdr.ThemaLaden.run.1", ex);
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-554632590, Log.FEHLER_ART_MREADER, "MediathekNdr.ThemaLaden.run.2", ex);
            }
            meldungThreadUndFertig();
        }

        void feedEinerSeiteSuchen(String strUrlFeed, String tthema) {
            // <h4><a href="/ratgeber/gesundheit/ernaehrung/minuten779.html" title="Zum Video: Wie funktioniert cholesterinsenkende Margarine?">Wie funktioniert cholesterinsenkende Margarine?</a></h4>
            // <div class="subline">45&nbsp;Min -&nbsp;22.04.2013 22:00</div>

            final String MUSTER_URL = "<h4><a href=\"/";
            final String MUSTER_ZEIT = "<div class=\"subline\">";
            final String MUSTER_DURATION = "<span class=\"runtime\" title=\"Spieldauer\">";
            seite1 = getUrlIo.getUri(nameSenderMReader, strUrlFeed, Konstanten.KODIERUNG_UTF, 3 /* versuche */, seite1, "Thema: " + tthema/* meldung */);
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            String titel = "";
            String thema = tthema;
            String datum = "";
            String zeit = "";
            long durationInSeconds = 0;
            String tmp;
            int lastPos = 0;
            try {
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_URL, pos)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    if ((pos2 = seite1.indexOf("\"", pos1)) == -1) {
                        continue;
                    }
                    url = seite1.substring(pos1, pos2);
                    if (url.equals("")) {
                        Log.fehlerMeldung(-659210274, Log.FEHLER_ART_MREADER, "MediathekNdr.feddEinerSeiteSuchen", "keine Url feedEinerSeiteSuchen" + strUrlFeed);
                        continue;
                    }
                    if ((pos1 = seite1.indexOf(">", pos)) != -1) {
                        pos1 += 1;
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            titel = seite1.substring(pos1, pos2);
                        }
                    }
                    if ((pos1 = seite1.indexOf(MUSTER_ZEIT, pos)) != -1) {
                        pos1 += MUSTER_ZEIT.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            tmp = seite1.substring(pos1, pos2);
                            if (tmp.contains("-")) {
                                tmp = tmp.substring(tmp.lastIndexOf("-") + 1);
                                tmp = tmp.replaceAll("&nbsp;", "");
                                try {
                                    SimpleDateFormat sdfIn = new SimpleDateFormat("dd.MM.yyyy HH:mm");
                                    Date filmDate = sdfIn.parse(tmp);
                                    datum = new SimpleDateFormat("dd.MM.yyyy").format(filmDate);
                                    zeit = new SimpleDateFormat("HH:mm:ss").format(filmDate);
                                } catch (Exception ex) {
                                    Log.fehlerMeldung(-623657941, Log.FEHLER_ART_MREADER, "MediathekNdr.FilmSuchen", "convertDatum: " + strUrlFeed);
                                }
                            }
                        }
                    }
                    // Die dauer des filmes kommt vor dem titel über welchen wir hier am
                    // iterieren sind. Wir müssen also rückwärts suchen. Die url kommt jeweils doppelt
                    // vor im dokument. Wir werdenden diese als marker zum zurückzu springen:
                    /*<a href="/fernsehen/sendungen/extra_3/videos/extra5349.html" title="Zum Video: Extra 3 vom 08.05.2013" >
                     <img width="376" height="212" src="/fernsehen/ehring255_v-zweispaltig.jpg" alt="Moderator Christian Ehring mit einem Schild mit der Aufschrift, wer das umdreht ist doof, in der Hand.  " title="Moderator Christian Ehring mit einem Schild mit der Aufschrift, wer das umdreht ist doof, in der Hand."  /><div class="overlay">
                     <div class="shiny_line">&nbsp;</div><div class="marker"></div>
                     <div class="inner">
                     <span class="icon icon_video" title="Video">Video</span>
                     <span class="runtime" title="Spieldauer">28:55</span></div>
                     </div>
                     </a>
                     <div class="content">
                     <h4><a href="/fernsehen/sendungen/extra_3/videos/extra5349.html" title="Zum Video: Extra 3 vom 08.05.2013">Extra 3 vom 08.05.2013</a></h4>
                     <div class="subline">extra&nbsp;3 -&nbsp;08.05.2013 22:50</div>*/
                    if ((pos1 = seite1.indexOf(url, lastPos)) != -1) {
                        if ((pos1 = seite1.indexOf(MUSTER_DURATION, pos1)) != -1) {
                            pos1 += MUSTER_DURATION.length();
                            if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                                String duration = seite1.substring(pos1, pos2).trim();
                                try {
                                    if (!duration.equals("")) {
                                        String[] parts = duration.split(":");
                                        long power = 1;
                                        durationInSeconds = 0;
                                        for (int i = parts.length - 1; i >= 0; i--) {
                                            durationInSeconds += Long.parseLong(parts[i]) * power;
                                            power *= 60;
                                        }
                                    }
                                } catch (Exception ex) {
                                    Log.fehlerMeldung(-369015497, Log.FEHLER_ART_MREADER, "MediathekNdr.feddEinerSeiteSuchen", ex, strUrlFeed);
                                }
                            }
                        }
                    }
                    if (thema.equals("")) {
                        thema = "NDR";
                    }
                    filmSuchen(strUrlFeed, thema, titel, "http://www.ndr.de/" + url, datum, zeit, durationInSeconds);
                    lastPos = pos;
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-693219870, Log.FEHLER_ART_MREADER, "MediathekNdr.feddEinerSeiteSuchen", strUrlFeed);
            }
        }

        void filmSuchen(String strUrlThema, String thema, String titel, String filmWebsite, String datum, String zeit, long durationInSeconds) {
            //playlist: [
            //{
            //1: {src:'http://hds.ndr.de/z/2013/0419/TV-20130419-1010-0801.,hi,hq,.mp4.csmil/manifest.f4m', type:"application/f4m+xml"},
            //2: {src:'http://hls.ndr.de/i/2013/0419/TV-20130419-1010-0801.,lo,hi,hq,.mp4.csmil/master.m3u8', type:"application/x-mpegURL"},
            //3: {src:'http://media.ndr.de/progressive/2013/0419/TV-20130419-1010-0801.hi.mp4', type:"video/mp4"},

            // http://media.ndr.de/progressive/2012/0820/TV-20120820-2300-0701.hi.mp4
            // rtmpt://cp160844.edgefcs.net/ondemand/mp4:flashmedia/streams/ndr/2012/0820/TV-20120820-2300-0701.hq.mp4

            final String MUSTER_URL = "3: {src:'http://";
            seite2 = getUrl.getUri_Utf(nameSenderMReader, filmWebsite, seite2, "strUrlThema: " + strUrlThema);
            //long durationInSeconds = extractDuration(seite2);
            String description = extractDescription(seite2);
            String[] keywords = extractKeywords(seite2);
            String imageUrl = extractImageURL(seite2);
            meldung(filmWebsite);
            int pos1;
            int pos2;
            String url, tmp;
            try {
                if ((pos1 = seite2.indexOf(MUSTER_URL)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = seite2.indexOf("'", pos1)) != -1) {
                        url = seite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            url = "http://" + url;
                            if (url.contains("http://media.ndr.de/progressive")) {
                                if (url.contains("hi.mp4")) {
                                    url = url.replace("hi.mp4", "hq.mp4");
                                }
                            }
                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, url, ""/*rtmpURL*/, datum, zeit, durationInSeconds, description, "", imageUrl, keywords);
                            if (url.contains(".hq.")) {
                                String urlKlein = url.replace(".hq.", ".hi.");
                                film.addUrlKlein(urlKlein, "");
                            }
                            addFilm(film);
                        } else {
                            Log.fehlerMeldung(-623657941, Log.FEHLER_ART_MREADER, "MediathekNdr.FilmSuchen", "keine URL: " + filmWebsite);
                        }
                    }
                } else {
                    Log.fehlerMeldung(-698970145, Log.FEHLER_ART_MREADER, "MediathekNdr.FilmSuchen", "keine Url: " + filmWebsite);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-699830157, Log.FEHLER_ART_MREADER, "MediathekNdr.FilmSuchen", ex);
            }
        }

        private String extractDescription(MVStringBuilder page) {
            String desc = extractString(page, "<meta property=\"og:description\" content=\"", "\"");
            if (desc == null) {
                return "";
            }
            return desc;
        }

        private String[] extractKeywords(MVStringBuilder page) {
            String keywords = extractString(page, "<meta name=\"keywords\"  lang=\"de\" content=\"", "\"");
            if (keywords == null) {
                return new String[]{""};
            }
            String[] k = keywords.split(",");
            for (int i = 0; i < k.length; i++) {
                k[i] = k[i].trim();
            }
            return k;
        }

        private String extractImageURL(MVStringBuilder page) {
            String image = extractString(page, "<meta property=\"og:image\" content=\"", "\"");
            if (image == null) {
                return "";
            }
            return image;
        }

        private String extractString(MVStringBuilder source, String startMarker, String endMarker) {
            int start = source.indexOf(startMarker);
            if (start == -1) {
                return null;
            }
            start = start + startMarker.length();
            int end = source.indexOf(endMarker, start);
            if (end == -1) {
                return null;
            }
            return source.substring(start, end);
        }
    }
}
