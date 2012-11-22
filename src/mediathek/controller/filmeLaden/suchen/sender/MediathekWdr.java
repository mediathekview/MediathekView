/*
 *    MediathekView
 *    Copyright (C) 2008 - 2012     W. Xaver
 *                              &   thausherr
 * 
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
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

import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Log;

/**
 *
 * @author
 */
public class MediathekWdr extends MediathekReader implements Runnable {

    public static final String SENDER = "WDR";
    private final int MAX_COUNT = 5;
    final String ROCKPALAST_URL = "http://www.wdr.de/tv/rockpalast/videos/uebersicht.jsp"; //TH
    private LinkedListUrl listeFilme = new LinkedListUrl();

    /**
     *
     * @param ddaten
     * @param dde
     */
    public MediathekWdr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 4, /* urlWarten */ 500, startPrio);
    }

    //===================================
    // public
    //===================================
    @Override
    public synchronized void addToList() {
        //Theman suchen
        listeThemen.clear();
        listeFilme.clear();
        meldungStart();
        addToList__("http://www.wdr.de/mediathek/html/regional/index.xml");
        if (suchen.allesLaden) {
            //TH Rockpalast hinzu
            String[] add = new String[]{ROCKPALAST_URL, "Rockpalast"};
            listeThemen.addUrl(add);
        }
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new SenderThemaLaden()).start();
            }
        }
    }

    //===================================
    // private
    //===================================
    private void addToList__(String ADRESSE) {
        //Theman suchen
        final String MUSTER_URL = "<a href=\"";
        final String START = "<h2>Sendungen A-Z</h2>";
        final String ENDE = "<h2>Themen</h2>";
        StringBuffer strSeite = new StringBuffer();
        strSeite = getUrlIo.getUri_Iso(nameSenderMReader, ADRESSE, strSeite, "");
        int pos;
        int pos1;
        int pos2;
        String url;
        String thema;
        // nach Sendungen suchen
        int ende = strSeite.indexOf(ENDE);
        int start = strSeite.indexOf(START);
        if (start != -1 && ende != -1) {
            pos = start;
            while (!Daten.filmeLaden.getStop() && (pos = strSeite.indexOf(MUSTER_URL, pos)) != -1) {
                if (pos > ende) {
                    //Themenbereich zu Ende
                    break;
                }
                thema = "";
                pos += MUSTER_URL.length();
                pos1 = pos;
                pos2 = strSeite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                    url = strSeite.substring(pos1, pos2);
                    pos1 = strSeite.indexOf("\">", pos);
                    pos2 = strSeite.indexOf("<", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        thema = strSeite.substring(pos1 + 2, pos2).trim();
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldungMReader(-995122047, "MediathekWdr.addToList__", "keine URL");
                    } else {
                        url = url.replace("&amp;", "&");
                        String[] add;
                        if (suchen.allesLaden) {
                            add = new String[]{"http://www.wdr.de" + url + "&rankingcount=20", thema};
                        } else {
                            add = new String[]{"http://www.wdr.de" + url + "&rankingcount=10", thema};
                        }
                        listeThemen.addUrl(add);
                    }
                }
            }
        } else {
            Log.fehlerMeldungMReader(-778521300, "MediathekWdr", "nix gefunden!!");
        }
        // nach Datum suchen
        final String MUSTER_URL_DATUM = "href=\"/mediathek/html/regional/ergebnisse/datum.xml?";
        pos = 0;
        while (!Daten.filmeLaden.getStop() && (pos = strSeite.indexOf(MUSTER_URL_DATUM, pos)) != -1) {
            thema = "";
            pos += MUSTER_URL_DATUM.length();
            if ((pos2 = strSeite.indexOf("\"", pos)) != -1) {
                url = strSeite.substring(pos, pos2);
                if (url.equals("")) {
                    Log.fehlerMeldungMReader(-656360477, "MediathekWdr.addToList__", "keine URL");
                } else {
                    url = url.replace("&amp;", "&");
                    String[] add;
                    final String a = "http://www.wdr.de/mediathek/html/regional/ergebnisse/datum.xml?";
                    if (suchen.allesLaden) {
                        add = new String[]{a + url + "&rankingcount=20", thema};
                    } else {
                        add = new String[]{a + url + "&rankingcount=10", thema};
                    }
                    listeThemen.addUrl(add);
                }
            }
        }
    }

    private class SenderThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer strSeite1 = new StringBuffer();
        private StringBuffer strSeite2 = new StringBuffer();
        private StringBuffer strVideoSeite = new StringBuffer();

        @Override
        public void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    //TH Weiche für Rockpalast
                    if (ROCKPALAST_URL.equals(link[0])) {
                        themenSeiteRockpalast();
                    } else {
                        themenSeitenSuchen(link[0] /* url */, link[1] /* Thema */);
                    }
                    meldungProgress(link[0]);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-633250489, "MediathekWdr.SenderThemaLaden.run", ex.getMessage());
            }
        }

        //TH
        private void themenSeiteRockpalast() {
            final String ROOTADR = "http://www.wdr.de";
            final String ITEM_1 = "<a href=\"/tv/rockpalast/extra/videos";
            // <li><a href="/tv/rockpalast/extra/videos/2009/0514/trail_of_dead.jsp">...And you will know us by the Trail Of Dead (2009)</a></li>
            int pos = 0;
            strVideoSeite = getUrl.getUri_Iso(nameSenderMReader, ROCKPALAST_URL, strVideoSeite, "");
            try {
                while ((pos = strVideoSeite.indexOf(ITEM_1, pos)) != -1) {
                    int pos1 = pos + 9;
                    int pos2 = strVideoSeite.indexOf("\">", pos1);
                    if (pos2 < 0) {
                        break;
                    }
                    int pos3 = strVideoSeite.indexOf("</a>", pos2);
                    if (pos3 < 0) {
                        break;
                    }
                    String titel = strVideoSeite.substring(pos2 + 2, pos3);
                    String url = ROOTADR + strVideoSeite.substring(pos1, pos2);
                    //System.out.println ("TH rock url = " + url + ", " + titel);
                    addFilme2(ROCKPALAST_URL, "Rockpalast", titel, url);
                    pos = pos3;
                }
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-696963025, "MediathekWdr.themenSeiteRockpalast", ex.getMessage());
            }
        }

        private void themenSeitenSuchen(String strUrlFeed, String thema) {
            //<div class="wsDisplayP"><strong>Video:</strong> http://www.wdr.de/mediathek/html/regional/
            //              2009/02/14/a40-komplett.xml;jsessionid=F203466678603049365CEB7FD61F087C.mediathek4</div>
            // diathek4" title="Zum Video 'Von Wundern und Bunkern'">

            //oder Audio:
            //    <div class="wsDisplayP"><strong>Audio:</strong> http://www.wdr.de/mediathek/html/regional/2009/01/
            //        30/cosmo-konsum.xml;jsessionid=A5A9CDE4FCA9E3339AFC9137656767CA.mediathek4</div>
            // title="Zum Audio 'Kauflaune ungetrübt'">Kauflaune u


            //<li class="wsVor">
            // <a href="?rankingtype=sendung&amp;rankingvisible=newest&amp;rankingvalue=A40&amp;rankingcount=5&amp;rankingpage=1">

            final String NEUESEITE_1 = "<li class=\"wsVor\">";
            final String NEUESEITE_2 = "<a href=\"?";
            final String MUSTER_URL = "<div class=\"wsDisplayP\"><strong>Video:</strong>";
            final String TITEL = "title=\"Zum Video '";
            int seitenCount = 0;
            int pos;
            int pos1;
            int pos2;
            boolean neueSeite;
            String url;
            String titel;
            do {
                neueSeite = false;
                pos = 0;
                strSeite1 = getUrl.getUri_Iso(nameSenderMReader, strUrlFeed, strSeite1, "");
                ++seitenCount;
                meldung(strUrlFeed);
                while (!Daten.filmeLaden.getStop() && (pos = strSeite1.indexOf(MUSTER_URL, pos)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    pos2 = strSeite1.indexOf("<", pos);
                    if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                        url = strSeite1.substring(pos1, pos2).trim();
                        if (!url.equals("")) {
                            // erst mal schauen obs die schon gab
                            if (listeFilme.addUrl(new String[]{url})) {
                                pos1 = strSeite1.indexOf(TITEL, pos);
                                if (pos1 != -1) {
                                    pos1 += TITEL.length();
                                    pos2 = strSeite1.indexOf("'", pos1);
                                    if (pos2 != -1 && pos1 < pos2) {
                                        titel = strSeite1.substring(pos1, pos2);
                                        //weiter gehts
                                        addFilme2(strUrlFeed, thema, titel, url);
                                    }
                                }
                            } else {
                                //Log.debugMeldung("WDR-doppelt");
                            }
                        } else {
                            Log.fehlerMeldungMReader(-375862100, "MediathekWdr.themenSeiteSuchen-1", "keine Url" + thema);
                        }
                    } else {
                        Log.fehlerMeldungMReader(-752589666, "MediathekWdr.themenSeiteSuchen-2", "keine Url" + thema);
                    }
                }
                if (suchen.allesLaden) {
                    if ((pos = strSeite1.indexOf(NEUESEITE_1)) != -1) {
                        if ((pos = strSeite1.indexOf(NEUESEITE_2, pos)) != -1) {
                            pos += NEUESEITE_2.length();
                            pos1 = pos;
                            pos2 = strSeite1.indexOf("\"", pos);
                            if (pos2 != -1 && pos1 < pos2) {
                                strUrlFeed = "http://www.wdr.de/mediathek/html/regional/ergebnisse/sendung.xml?" + strSeite1.substring(pos1, pos2);
                                strUrlFeed = strUrlFeed.replace("&amp;", "&");
                                if (!strUrlFeed.equals("")) {
                                    if (seitenCount < MAX_COUNT) {
                                        neueSeite = true;
                                    }
                                }
                            }
                        }
                    }
                }
            } while (!Daten.filmeLaden.getStop() && neueSeite);
        }

        private void addFilme2(String strUrlFeed, String thema, String titel, String urlFilm) {
            // ;dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e1/media/video/2009/02/14/20090214_a40_komplett_big.flv&amp;isdnSrc=rtm
            // <p class="wsArticleAutor">Ein Beitrag von Heinke Schröder, 24.11.2010	</p>
            final String MUSTER_URL = "dslSrc=";
            final String MUSTER_DATUM = "<p class=\"wsArticleAutor\">";
            final String MUSTER_THEMA = "Homepage der Sendung ["; //Homepage der Sendung [west.art]</a>
            meldung(urlFilm);
            strSeite2 = getUrl.getUri_Iso(nameSenderMReader, urlFilm, strSeite2, "");
            int pos;
            int pos1;
            int pos2;
            String url;
            String datum = "";
            // Datum suchen
            if ((pos = strSeite2.indexOf(MUSTER_DATUM)) != -1) {
                pos += MUSTER_DATUM.length();
                pos1 = pos;
                if ((pos2 = strSeite2.indexOf("<", pos)) != -1) {
                    if (pos1 < pos2) {
                        datum = strSeite2.substring(pos1, pos2).trim();
                        if (datum.length() > 10) {
                            datum = datum.substring(datum.length() - 10);
                        }
                    }
                }
            }
            // Thema suchen
            if (thema.equals("")) {
                if ((pos = strSeite2.indexOf(MUSTER_THEMA)) != -1) {
                    pos += MUSTER_THEMA.length();
                    pos1 = pos;
                    if ((pos2 = strSeite2.indexOf("]", pos)) != -1) {
                        if (pos1 < pos2) {
                            thema = strSeite2.substring(pos1, pos2).trim();
                        }
                    }
                }
                if (thema.equals("")) {
                    final String MUSTER = "]\" ";
                    if ((pos = strSeite2.indexOf(MUSTER)) != -1) {
                        pos += MUSTER.length();
                        if (pos > 100) {
                            String sub = strSeite2.substring(pos - 100, pos);
                            if ((pos = sub.indexOf("[")) != -1) {
                                pos += 1;
                                if ((pos2 = sub.indexOf("]", pos)) != -1) {
                                    thema = sub.substring(pos, pos2).trim();
                                }
                            }
                        }
                    }
                }
                if (thema.equals("")) {
                    // dann gehts halt nicht
                    thema = nameSenderFilmliste;
                }
            }
            // URL suchen
            if ((pos = strSeite2.indexOf(MUSTER_URL)) != -1) {
                pos += MUSTER_URL.length();
                pos1 = pos;
                if ((pos2 = strSeite2.indexOf("&", pos)) != -1) {
                    if (pos1 < pos2) {
                        url = strSeite2.substring(pos1, pos2);
                        //um Fehler bereinigen, wenn 2x rtmp im Link ist
                        if (url.substring(1).contains("rtmp://")) {
                            url = url.substring(1);
                            url = url.substring(url.indexOf("rtmp://"));
                        }
                        if (url.substring(1).contains("http://")) {
                            url = url.substring(1);
                            url = url.substring(url.indexOf("http://"));
                        }
                        if (!url.equals("")) {
                            // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, ""/* zeit */);
                            addFilm(film);
                        } else {
                            Log.fehlerMeldungMReader(-763299001, "MediathekWdr.addFilme2-1", "keine Url" + thema);
                        }
                    }
                }
            } else {
                Log.fehlerMeldungMReader(-596631004, "MediathekWdr.addFilme2-2", "keine Url" + thema);
            }
        }
    }
}
