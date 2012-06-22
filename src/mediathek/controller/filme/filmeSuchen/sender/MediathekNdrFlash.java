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
package mediathek.controller.filme.filmeSuchen.sender;

import java.text.SimpleDateFormat;
import java.util.Date;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.filmeSuchenSender.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;

public class MediathekNdrFlash extends MediathekReader implements Runnable {

    public static final String SENDER = "NDR-Flash";
    private final int MAX_PER_FEED = 5;

    public MediathekNdrFlash(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 4, /* urlWarten */ 1000, startPrio);
    }

//-> erste Seite:
//http://www.ndr.de/mediathek/mediathek100-mediathek_medium-tv_searchtype-broadcasts.xml
//-> Thema:
//http://www.ndr.de/mediathek/mediathek100-mediathek_medium-tv_broadcast-46_pageSize-24.xml
//-> Film:
//http://www.ndr.de/mediathek/visite4392-mediathek_details-true.xml
    @Override
    void addToList() {
        //<broadcast id="1391" site="ndrfernsehen">45 Min</broadcast>
        final String ADRESSE = "http://www.ndr.de/mediathek/mediathek100-mediathek_medium-tv_searchtype-broadcasts.xml";
        final String MUSTER_URL1 = "<broadcast id=\"";
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        seite = getUrlIo.getUri(nameSenderMReader, ADRESSE, Konstanten.KODIERUNG_UTF, 5 /* versuche */, seite, ""/* meldung */);
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        String thema = "";
        //Podcasts auslesen
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
                    Log.fehlerMeldungMReader(-210367600, "MediathekNdr.addToList", "keine Url");
                    continue;
                }
                String[] add = new String[]{"http://www.ndr.de/mediathek/mediathek100-mediathek_medium-tv_broadcast-" + url + "_pageSize-24.xml", thema};
                if (!istInListe(listeThemen, url, 0)) {
                    listeThemen.add(add);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-332945670, "MediathekNdr.finden", ex);
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                for (int t = 0; t < maxThreadLaufen; ++t) {
                    new Thread(new ThemaLaden()).start();
                }
            }
        }

    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();
        private StringBuffer seite2 = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    try {
                        meldungProgress(link[1]);
                        feedEinerSeiteSuchen(link[0], link[1] /* thema */);
                    } catch (Exception ex) {
                        Log.fehlerMeldung(-336901211, "MediathekNdr.ThemaLaden.run.1", ex);
                    }
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung(-554632590, "MediathekNdr.ThemaLaden.run.2", ex);
            }
        }

        void feedEinerSeiteSuchen(String strUrlFeed, String thema) {
            //<mediaItem id="diezockerbank110" type="video">
            //<title><![CDATA[Die Zockerbank]]></title>
            final String MUSTER_URL = "<mediaItem id=\"";
            final String MUSTER_TITEL = "<title><![CDATA[";
            int counter = 0;
            seite1 = getUrlIo.getUri(nameSenderMReader, strUrlFeed, Konstanten.KODIERUNG_UTF, 3 /* versuche */, seite1, "Thema: " + thema/* meldung */);
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            String titel = "";
            try {
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_URL, pos)) != -1) {
                    ++counter;
//////////                    if (!suchen.allesLaden && counter > MAX_PER_FEED) {
//////////                        break;
//////////                    }
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    if ((pos2 = seite1.indexOf("\"", pos)) != -1) {
                        url = seite1.substring(pos1, pos2);
                    }
                    if (!url.equals("")) {
                        pos = pos2;
                        if ((pos = seite1.indexOf(MUSTER_TITEL, pos)) != -1) {
                            pos += MUSTER_TITEL.length();
                            pos1 = pos;
                            if ((pos2 = seite1.indexOf("]", pos + 1)) != -1) {
                                titel = seite1.substring(pos1, pos2);
                            }
                            filmSuchen(strUrlFeed, thema, titel, "http://www.ndr.de/mediathek/" + url + "-mediathek_details-true.xml");
                        }
                    } else {
                        Log.fehlerMeldungMReader(-659210274, "MediathekNdr.feddEinerSeiteSuchen", "keine Url feedEinerSeiteSuchen" + strUrlFeed);
                    }

                }
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-693219870, "MediathekNdr.feddEinerSeiteSuchen", strUrlFeed);
            }
        }

        void filmSuchen(String strUrlThema, String thema, String titel, String urlFilm) {
            //<source format="mp4hi" mimetype="video/mp4" protocol="rtmpt">rtmpt://ndr.fcod.llnwd.net/a3715/d1/flashmedia/streams/ndr/2010/0208/TV-20100208-1833-0101.hi.mp4</source>
            //<source format="mp4hq" mimetype="video/mp4" protocol="rtmpt">rtmpt://ndr.fcod.llnwd.net/a3715/d1/flashmedia/streams/ndr/2010/0208/TV-20100208-1833-0101.hq.mp4</source>
            //http://www.ndr.de/mediathek/visite4392-mediathek_details-true.xml
            // <date weekday="Dienstag" weekdayShort="Di">2010-11-30T23:45:00</date>
            final String MUSTER_DATUM = "<date ";
            final String MUSTER_URL = "<source format=\"mp4hq\" mimetype=\"video/mp4\" protocol=\"rtmpt\">";
            seite2 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite2, "strUrlThema: " + strUrlThema);
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            String datum = "";
            String zeit = "";
            String tmp = "";
            try {
                if ((pos = seite2.indexOf(MUSTER_DATUM)) != -1) {
                    pos += MUSTER_DATUM.length();
                    if ((pos1 = seite2.indexOf(">", pos)) != -1) {
                        pos1 += 1;
                        if ((pos2 = seite2.indexOf("<", pos1)) != -1) {
                            tmp = seite2.substring(pos1, pos2);
                            try {
                                SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
                                Date filmDate = sdfIn.parse(tmp);
                                SimpleDateFormat sdfOut;
                                sdfOut = new SimpleDateFormat("dd.MM.yyyy");
                                datum = sdfOut.format(filmDate);
                                sdfOut = new SimpleDateFormat("HH:mm:ss");
                                zeit = sdfOut.format(filmDate);
                            } catch (Exception ex) {
                                Log.fehlerMeldungMReader(-623657941, "MediathekNdr.FilmSuchen", "convertDatum: " + strUrlThema);
                            }
                        }
                    }
                }
                if ((pos = seite2.indexOf(MUSTER_URL)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    if ((pos2 = seite2.indexOf("<", pos)) != -1) {
                        url = seite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            // URL für den flvstreamer aufbereiten:
                            // aus:  rtmpt://ndr.fcod.llnwd.net/a3715/d1/flashmedia/streams/ndr/2012/0621/TV-20120621-1319-2801.hq.mp4
                            // wird: rtmpt://cp160844.edgefcs.net/ondemand/mp4:flashmedia/streams/ndr/2012/0525/TV-20120525-0207-5901.hq.mp4
                            String s1 = "rtmpt://cp160844.edgefcs.net/ondemand/mp4:";
                            String s2 = url.substring(url.indexOf("flashmedia"));
                            String sUrl = s1 + s2;
                            if (!sUrl.equals("")) {
                                //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel)
                                addFilm(new DatenFilm(nameSenderMReader, thema, strUrlThema, titel, sUrl, datum, zeit));
                            } else {
                                Log.fehlerMeldungMReader(-878542100, "MediathekNdr.FilmSuchen", "Zusammenbau URL: " + url);
                            }
                        } else {
                            Log.fehlerMeldungMReader(-623657941, "MediathekNdr.FilmSuchen", "keine URL: " + urlFilm);
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-699830157, "MediathekNdr.FilmSuchen", ex);
            }
        }
    }
}
