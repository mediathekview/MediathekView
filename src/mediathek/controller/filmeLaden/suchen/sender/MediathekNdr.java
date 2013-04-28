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

public class MediathekNdr extends MediathekReader implements Runnable {

    public static final String SENDER = "NDR";

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
        StringBuffer seite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
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
                String[] add;
                add = new String[]{"http://www.ndr.de/mediathek/" + url, thema};
                listeThemen.addUrl(add);
            } catch (Exception ex) {
                Log.fehlerMeldung(-332945670, Log.FEHLER_ART_MREADER, "MediathekNdr.finden", ex);
            }
        }
//        addTage();
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new ThemaLaden()).start();
            }
        }
    }

    private void addTage() {
        // Seiten der Ansicht: "letzten Tage"
        // http://www.ndr.de/mediathek/mediathek100-mediathek_medium-tv_pageSize-24.xml
        // http://www.ndr.de/mediathek/mediathek100-mediathek_page-1_medium-tv_pageSize-24.xml
        String[] add = new String[]{"http://www.ndr.de/mediathek/mediathek100-mediathek_medium-tv_pageSize-24.xml", ""};
        listeThemen.addUrl(add);
        int m = (suchen.senderAllesLaden ? 20 : 10);
        for (int i = 0; i <= m; ++i) {
            add = new String[]{"http://www.ndr.de/mediathek/mediathek100-mediathek_page-" + Integer.toString(i) + "_medium-tv_pageSize-24.xml", ""};
            listeThemen.addUrl(add);
        }
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer seite2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

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
            seite1 = getUrlIo.getUri(nameSenderMReader, strUrlFeed, Konstanten.KODIERUNG_UTF, 3 /* versuche */, seite1, "Thema: " + tthema/* meldung */);
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            String titel = "";
            String thema = tthema;
            String datum = "";
            String zeit = "";
            String tmp = "";
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
                    if (thema.equals("")) {
                        thema = "NDR";
                    }
                    filmSuchen(strUrlFeed, thema, titel, "http://www.ndr.de/" + url, datum, zeit);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-693219870, Log.FEHLER_ART_MREADER, "MediathekNdr.feddEinerSeiteSuchen", strUrlFeed);
            }
        }

        void filmSuchen(String strUrlThema, String thema, String titel, String urlFilm, String datum, String zeit) {
            //playlist: [
            //{
            //1: {src:'http://hds.ndr.de/z/2013/0419/TV-20130419-1010-0801.,hi,hq,.mp4.csmil/manifest.f4m', type:"application/f4m+xml"},
            //2: {src:'http://hls.ndr.de/i/2013/0419/TV-20130419-1010-0801.,lo,hi,hq,.mp4.csmil/master.m3u8', type:"application/x-mpegURL"},
            //3: {src:'http://media.ndr.de/progressive/2013/0419/TV-20130419-1010-0801.hi.mp4', type:"video/mp4"},

            // http://media.ndr.de/progressive/2012/0820/TV-20120820-2300-0701.hi.mp4
            // rtmpt://cp160844.edgefcs.net/ondemand/mp4:flashmedia/streams/ndr/2012/0820/TV-20120820-2300-0701.hq.mp4

            final String MUSTER_URL = "3: {src:'http://";
            seite2 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite2, "strUrlThema: " + strUrlThema);
            meldung(urlFilm);
            int pos;
            int pos1;
            int pos2;
            String url, tmp;
            try {
                if ((pos = seite2.indexOf(MUSTER_URL)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    if ((pos2 = seite2.indexOf("'", pos)) != -1) {
                        url = seite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            url = "http://" + url;
                            // rtmp bauen
                            if (url.contains("http://media.ndr.de/progressive")) {
                                tmp = url.replace("http://media.ndr.de/progressive", "rtmpt://cp160844.edgefcs.net/ondemand/mp4:flashmedia/streams/ndr");
                                if (tmp.contains("hi.mp4")) {
                                    tmp = tmp.replace("hi.mp4", "hq.mp4");
                                    url = tmp;
                                }
                            }
                            //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel)
                            addFilm(new DatenFilm(nameSenderMReader, thema, strUrlThema, titel, url, datum, zeit));
                        } else {
                            Log.fehlerMeldung(-623657941, Log.FEHLER_ART_MREADER, "MediathekNdr.FilmSuchen", "keine URL: " + urlFilm);
                        }
                    }
                } else {
                    // Mist!
                    Log.fehlerMeldung(-698970145, Log.FEHLER_ART_MREADER, "MediathekNdr.FilmSuchen", "keine Url: " + urlFilm);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-699830157, Log.FEHLER_ART_MREADER, "MediathekNdr.FilmSuchen", ex);
            }
        }
    }
}
