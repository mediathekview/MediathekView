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
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class MediathekWdr extends MediathekReader implements Runnable {
    
    public static final String SENDER = "WDR";
    final String ROCKPALAST_URL = "http://www.wdr.de/tv/rockpalast/videos/uebersicht.jsp"; //TH
    private StringBuffer strSeite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

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
        meldungStart();
        addToList__("http://www1.wdr.de/mediathek/video/sendungen/abisz-a102.html");
        if (suchen.senderAllesLaden) {
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
        // http://www1.wdr.de/mediathek/video/sendungen/abisz-b100.html
        //Theman suchen
        final String MUSTER_URL = "<a href=\"/mediathek/video/sendungen/abisz-";
        StringBuffer strSeite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        strSeite = getUrlIo.getUri_Iso(nameSenderMReader, ADRESSE, strSeite, "");
        int pos1 = 0;
        int pos2;
        String url;
        themenSeitenSuchen(ADRESSE); // ist die erste Seite: "a"
        while (!Daten.filmeLaden.getStop() && (pos1 = strSeite.indexOf(MUSTER_URL, pos1)) != -1) {
            pos1 += MUSTER_URL.length();
            if ((pos2 = strSeite.indexOf("\"", pos1)) != -1) {
                url = strSeite.substring(pos1, pos2);
                if (url.equals("")) {
                    Log.fehlerMeldung(-995122047, Log.FEHLER_ART_MREADER, "MediathekWdr.addToList__", "keine URL");
                } else {
                    url = "http://www1.wdr.de/mediathek/video/sendungen/abisz-" + url;
                    themenSeitenSuchen(url);
                }
            }
        }
    }
    
    private void themenSeitenSuchen(String strUrlFeed) {
        //<ul class="linkList pictured">
        //<li class="neutral" >
        //<img src="/mediathek/video/sendungen/abenteuer_erde/logo-abenteuer-erde100_v-ARDGrosserTeaser.jpg"    title="Bildrechte: wdr" alt="Logo Abenteuer Erde"   />
        //<a href="/mediathek/video/sendungen/abenteuer_erde/filterseite-abenteuer-erde100.html" >
        //<strong>
        //<strong>Abenteuer Erde</strong>: Die Sendungen im Überblick
        //</strong>
        //</a>

        // url:
        // http://www1.wdr.de/mediathek/video/sendungen/dittsche/videonichtimpapamobil100.html

        final String MUSTER_START = "<ul class=\"linkList pictured\">";
        final String MUSTER_URL = "<a href=\"/mediathek/video/sendungen/";
        int pos1;
        int pos2;
        String url;
        strSeite = getUrlIo.getUri_Iso(nameSenderMReader, strUrlFeed, strSeite, "");
        meldung(strUrlFeed);
        if ((pos1 = strSeite.indexOf(MUSTER_START)) == -1) {
            Log.fehlerMeldung(-460857479, Log.FEHLER_ART_MREADER, "MediathekWdr.themenSeiteSuchen", "keine Url" + strUrlFeed);
            return;
        }
        while (!Daten.filmeLaden.getStop() && (pos1 = strSeite.indexOf(MUSTER_URL, pos1)) != -1) {
            pos1 += MUSTER_URL.length();
            if ((pos2 = strSeite.indexOf("\"", pos1)) != -1) {
                url = strSeite.substring(pos1, pos2).trim();
                if (!url.equals("")) {
                    url = "http://www1.wdr.de/mediathek/video/sendungen/" + url;
                    //weiter gehts
                    String[] add;
                    add = new String[]{url, ""};
                    listeThemen.addUrl(add);
                }
            } else {
                Log.fehlerMeldung(-375862100, Log.FEHLER_ART_MREADER, "MediathekWdr.themenSeiteSuchen", "keine Url" + strUrlFeed);
            }
        }
    }
    
    private class SenderThemaLaden implements Runnable {
        
        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer strSeite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer strSeite2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer strSeite3 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer strSeite4 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer strVideoSeite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        
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
                        sendungsSeitenSuchen1(link[0] /* url */);
                    }
                    meldungProgress(link[0]);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-633250489, Log.FEHLER_ART_MREADER, "MediathekWdr.SenderThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }
        
        private void sendungsSeitenSuchen1(String strUrl) {
            // http://www1.wdr.de/mediathek/video/sendungen/ein_fall_fuer_die_anrheiner/filterseite-ein-fall-fuer-die-anrheiner100_compage-2_paginationId-picturedList0.html#picturedList0
            int pos1;
            int pos2;
            int ende;
            strSeite1 = getUrl.getUri_Utf(nameSenderMReader, strUrl, strSeite1, "");
            meldung(strUrl);
            // Sendungen auf der Seite
            sendungsSeitenSuchen2(strUrl);
            if (!suchen.senderAllesLaden) {
                // dann wars das
                return;
            }
            // weitere Seiten suchen
            if ((pos1 = strSeite1.indexOf("<ul class=\"pageCounterNavi\">")) == -1) {
                return;
            }
            if ((ende = strSeite1.indexOf("</ul>", pos1)) == -1) {
                return;
            }
            while ((pos1 = strSeite1.indexOf("<a href=\"/mediathek/video/sendungen/", pos1)) != -1) {
                if (pos1 > ende) {
                    // dann wars das
                    return;
                }
                pos1 += "<a href=\"/mediathek/video/sendungen/".length();
                if ((pos2 = strSeite1.indexOf("\"", pos1)) != -1) {
                    String urlWeiter = strSeite1.substring(pos1, pos2);
                    if (!urlWeiter.equals("")) {
                        // Sendungen auf der Seite
                        sendungsSeitenSuchen2("http://www1.wdr.de/mediathek/video/sendungen/" + urlWeiter);
                    }
                }
            }
        }
        
        private void sendungsSeitenSuchen2(String strUrl) {
            //<div class="pictureCont" id="picturedList0">
            //<ul class="linkList pictured">
            //<li class="mediathekvideo" >
            //<img src="/mediathek/video/sendungen/der_vorkoster/dervorkosterspeisesalzimqualitaetsvergleich100_v-ARDGrosserTeaser.jpg"    title="Bildrechte: wdr" alt="Zwei verschiedene Salzsorten"   />
            //<a href="/mediathek/video/sendungen/der_vorkoster/videodervorkosterspeisesalzimqualitaetsvergleich100.html" >
            //<strong>
            //Der Vorkoster - Speisesalz im Qualitätsvergleich: Sendung vom 03.05.2013
            //</strong>
            //<span class="supplementary">
            //<span class="mediaLength">
            //<span class="hidden">L&auml;nge: </span>00:44:27 <abbr title="Minuten">Min.</abbr>
            //</span>

            // url:
            // http://www1.wdr.de/mediathek/video/sendungen/der_vorkoster/videodervorkosterspeisesalzimqualitaetsvergleich100.html

            final String MUSTER_START_1 = "<ul class=\"linkList pictured\">";
            final String MUSTER_START_2 = "<div id=\"pageLeadIn\">";
            
            final String MUSTER_URL = "<a href=\"/mediathek/video/sendungen/";
            final String MUSTER_TITEL = "<strong>";
            final String MUSTER_DAUER = "<span class=\"hidden\">L&auml;nge: </span>";
            final String MUSTER_THEMA = "<title>";
            int pos;
            int pos1;
            int pos2;
            int ende;
            String url;
            String titel = "";
            String dauer = "";
            String datum = "";
            String thema = "";
            long duration = 0;
            pos = 0;
            strSeite2 = getUrl.getUri_Utf(nameSenderMReader, strUrl, strSeite2, "");
            meldung(strUrl);
            // Thema suchen
            // <title>Lokalzeit aus Bonn - WDR MEDIATHEK</title>
            if ((pos1 = strSeite2.indexOf(MUSTER_THEMA, pos)) != -1) {
                pos1 += MUSTER_THEMA.length();
                if ((pos2 = strSeite2.indexOf("<", pos1)) != -1) {
                    thema = strSeite2.substring(pos1, pos2);
                    // putzen
                    thema = thema.replace("- WDR MEDIATHEK", "").trim();
                }
            }
            // und jetzt die Beiträge
            if ((pos = strSeite2.indexOf(MUSTER_START_1)) == -1) {
                if ((pos = strSeite2.indexOf(MUSTER_START_2)) == -1) {
                    Log.fehlerMeldung(-765323079, Log.FEHLER_ART_MREADER, "MediathekWdr.sendungsSeiteSuchen", "keine Url" + strUrl);
                    return;
                }
            }
            if ((ende = strSeite2.indexOf("<ul class=\"pageCounterNavi\">", pos)) == -1) {
                if ((ende = strSeite2.indexOf("<div id=\"socialBookmarks\">", pos)) == -1) {
                    Log.fehlerMeldung(-646897321, Log.FEHLER_ART_MREADER, "MediathekWdr.sendungsSeiteSuchen", "keine Url" + strUrl);
                    return;
                }
            }
            while (!Daten.filmeLaden.getStop() && (pos = strSeite2.indexOf(MUSTER_URL, pos)) != -1) {
                if (pos > ende) {
                    break;
                }
                pos += MUSTER_URL.length();
                pos1 = pos;
                if ((pos2 = strSeite2.indexOf("\"", pos)) != -1) {
                    url = strSeite2.substring(pos1, pos2).trim();
                    if (!url.equals("")) {
                        url = "http://www1.wdr.de/mediathek/video/sendungen/" + url;
                        if ((pos1 = strSeite2.indexOf(MUSTER_TITEL, pos)) != -1) {
                            pos1 += MUSTER_TITEL.length();
                            if ((pos2 = strSeite2.indexOf("<", pos1)) != -1) {
                                titel = strSeite2.substring(pos1, pos2).trim();
                                // putzen
                                titel = titel.replace("\n", "");
                                if (titel.contains("-")) {
                                    titel = titel.substring(titel.indexOf("-") + 1, titel.length());
                                }
                                if (titel.contains(":")) {
                                    datum = titel.substring(titel.lastIndexOf(":") + 1, titel.length()).trim();
                                    if (datum.contains(" vom")) {
                                        datum = datum.substring(datum.indexOf(" vom") + " vom".length()).trim();
                                    }
                                    titel = titel.substring(0, titel.lastIndexOf(":")).trim();
                                }
                            }
                        }
                        if ((pos1 = strSeite2.indexOf(MUSTER_DAUER, pos)) != -1) {
                            pos1 += MUSTER_DAUER.length();
                            if ((pos2 = strSeite2.indexOf("<", pos1)) != -1) {
                                dauer = strSeite2.substring(pos1, pos2).trim();
                                try {
                                    if (!dauer.equals("")) {
                                        String[] parts = dauer.split(":");
                                        duration = 0;
                                        long power = 1;
                                        for (int i = parts.length - 1; i >= 0; i--) {
                                            duration += Long.parseLong(parts[i]) * power;
                                            power *= 60;
                                        }
                                    }
                                } catch (Exception ex) {
                                    Log.fehlerMeldung(-306597519, Log.FEHLER_ART_MREADER, "MediathekWdr.sendungsSeiteSuchen", ex, strUrl);
                                }
                            }
                        }
                        if (thema.equals("") || datum.equals("") || titel.equals("") || duration == 0) {
                            Log.fehlerMeldung(-323569701, Log.FEHLER_ART_MREADER, "MediathekWdr.sendungsSeiteSuchen", strUrl);
                        }
                        //weiter gehts
                        addFilm1(thema, titel, url, duration, datum);
                    } else {
                        Log.fehlerMeldung(-646432970, Log.FEHLER_ART_MREADER, "MediathekWdr.sendungsSeiteSuchen-1", "keine Url" + strUrl);
                    }
                }
            }
        }
        
        private void addFilm1(String thema, String titel, String filmWebsite, long dauer, String datum) {
            // http://www1.wdr.de/mediathek/video/sendungen/die_story/videopharmasklaven100-videoplayer_size-L.html

            final String MUSTER_URL_START = "<span class=\"videoLink\">";
            final String MUSTER_URL = "<a href=\"/mediathek/video/sendungen/";
            final String MUSTER_DESCRIPTION = "<meta name=\"Description\" content=\"";
            final String MUSTER_IMAGE_START = "<div class=\"linkCont\">";
            final String MUSTER_IMAGE = "<img src=\"/mediathek/video/sendungen/";
            
            final String MUSTER_KEYWORDS = "<meta name=\"Keywords\" content=\"";
            meldung(filmWebsite);
            strSeite3 = getUrl.getUri_Utf(nameSenderMReader, filmWebsite, strSeite3, "");
            int pos1;
            int pos2;
            String url = "";
            String description = "";
            String image = "";
            String[] keywords = new String[]{};
            
            if ((pos1 = strSeite3.indexOf(MUSTER_DESCRIPTION)) != -1) {
                pos1 += MUSTER_DESCRIPTION.length();
                if ((pos2 = strSeite3.indexOf("\"", pos1)) != -1) {
                    description = strSeite3.substring(pos1, pos2);
                }
            }
            
            if ((pos1 = strSeite3.indexOf(MUSTER_IMAGE_START)) != -1) {
                pos1 += MUSTER_IMAGE_START.length();
                if ((pos1 = strSeite3.indexOf(MUSTER_IMAGE, pos1)) != -1) {
                    pos1 += MUSTER_IMAGE.length();
                    if ((pos2 = strSeite3.indexOf("\"", pos1)) != -1) {
                        image = "http://www1.wdr.de/mediathek/video/sendungen/" + strSeite3.substring(pos1, pos2);
                    }
                }
            } else if ((pos1 = strSeite3.indexOf("<li class=\"mediathekvideo\" >")) != -1) {
                //pos1 += "<li class=\"mediathekvideo\" >".length();
                if ((pos1 = strSeite3.indexOf(MUSTER_IMAGE, pos1)) != -1) {
                    pos1 += MUSTER_IMAGE.length();
                    if ((pos2 = strSeite3.indexOf("\"", pos1)) != -1) {
                        image = "http://www1.wdr.de/mediathek/video/sendungen/" + strSeite3.substring(pos1, pos2);
                    }
                }
            }
            
            if ((pos1 = strSeite3.indexOf(MUSTER_KEYWORDS)) != -1) {
                pos1 += MUSTER_KEYWORDS.length();
                if ((pos2 = strSeite3.indexOf("\"", pos1)) != -1) {
                    String k = strSeite3.substring(pos1, pos2);
                    keywords = k.split(", ");
                }
            }
            // URL suchen
            if ((pos1 = strSeite3.indexOf(MUSTER_URL_START)) != -1) {
                pos1 += MUSTER_URL_START.length();
                if ((pos1 = strSeite3.indexOf(MUSTER_URL, pos1)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = strSeite3.indexOf("\"", pos1)) != -1) {
                        url = strSeite3.substring(pos1, pos2);
                    }
                }
            } else if ((pos1 = strSeite3.indexOf("<li class=\"mediathekvideo\" >")) != -1) {
                //pos1 += "<li class=\"mediathekvideo\" >".length();
                if ((pos1 = strSeite3.indexOf(MUSTER_URL, pos1)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = strSeite3.indexOf("\"", pos1)) != -1) {
                        url = strSeite3.substring(pos1, pos2);
                    }
                }
            }
            
            
            if (description.equals("") || image.equals("") || keywords.equals("")) {
                Log.fehlerMeldung(-649830789, Log.FEHLER_ART_MREADER, "MediathekWdr.addFilm1", new String[]{filmWebsite});
            }
            if (!url.equals("")) {
                addFilm2(filmWebsite, thema, titel, "http://www1.wdr.de/mediathek/video/sendungen/" + url, dauer, datum, description, keywords, image);
            } else {
                Log.fehlerMeldung(-763299001, Log.FEHLER_ART_MREADER, "MediathekWdr.addFilme1", new String[]{"keine Url: " + filmWebsite});
            }
            
        }
        
        private void addFilm2(String filmWebsite, String thema, String titel, String urlFilmSuchen, long dauer, String datum, String beschreibung, String[] keyword, String image) {
            // ;dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e1/media/video/2009/02/14/20090214_a40_komplett_big.flv&amp;isdnSrc=rtm
            // <p class="wsArticleAutor">Ein Beitrag von Heinke Schröder, 24.11.2010	</p>
            final String MUSTER_URL_L = "<a rel=\"webL\"  href=\"";
            final String MUSTER_URL_M = "<a rel=\"webM\"  href=\"";
            final String MUSTER_URL_S = "<a rel=\"webS\"  href=\"";
            meldung(urlFilmSuchen);
            strSeite4 = getUrl.getUri_Utf(nameSenderMReader, urlFilmSuchen, strSeite4, "");
            int pos1;
            int pos2;
            String url = "";
            String urlKlein = "";

            // URL suchen
            if ((pos1 = strSeite4.indexOf(MUSTER_URL_L)) != -1) {
                pos1 += MUSTER_URL_L.length();
                if ((pos2 = strSeite4.indexOf("\"", pos1)) != -1) {
                    url = strSeite4.substring(pos1, pos2);
                }
            }
            if ((pos1 = strSeite4.indexOf(MUSTER_URL_M)) != -1) {
                pos1 += MUSTER_URL_M.length();
                if ((pos2 = strSeite4.indexOf("\"", pos1)) != -1) {
                    if (url.equals("")) {
                        url = strSeite4.substring(pos1, pos2);
                    } else {
                        urlKlein = strSeite4.substring(pos1, pos2);
                    }
                }
            }
            if ((pos1 = strSeite4.indexOf(MUSTER_URL_S)) != -1) {
                pos1 += MUSTER_URL_S.length();
                if ((pos2 = strSeite4.indexOf("\"", pos1)) != -1) {
                    if (url.equals("")) {
                        url = strSeite4.substring(pos1, pos2);
                    } else {
                        if (urlKlein.equals("")) {
                            urlKlein = strSeite4.substring(pos1, pos2);
                        }
                    }
                }
            }
            if (!url.equals("")) {
                // URL bauen von
                // http://mobile-ondemand.wdr.de/CMS2010/mdb/14/149491/lokalzeitausaachen_1474195.mp4
                // nach
                // rtmp://gffstream.fcod.llnwd.net/a792/e2/mp4:CMS2010/mdb/14/149491/lokalzeitausaachen_1474195.mp4
                url = url.replace("http://mobile-ondemand.wdr.de/", "rtmp://gffstream.fcod.llnwd.net/a792/e2/mp4:");
                urlKlein = urlKlein.replace("http://mobile-ondemand.wdr.de/", "rtmp://gffstream.fcod.llnwd.net/a792/e2/mp4:");
                //public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String datum, String zeit,
                //long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
                DatenFilm film = new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, url, datum, ""/* zeit */,
                        dauer, beschreibung, "", image, keyword);
                film.addKleineUrl(urlKlein, "");
                addFilm(film);
            } else {
                Log.fehlerMeldung(-978451239, Log.FEHLER_ART_MREADER, "MediathekWdr.addFilme2", new String[]{"keine Url: " + urlFilmSuchen, "UrlThema: " + filmWebsite});
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
                while (!Daten.filmeLaden.getStop() && (pos = strVideoSeite.indexOf(ITEM_1, pos)) != -1) {
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
                    addFilmeRockpalast("Rockpalast", titel, url);
                    pos = pos3;
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-696963025, Log.FEHLER_ART_MREADER, "MediathekWdr.themenSeiteRockpalast", ex);
            }
        }
        
        private void addFilmeRockpalast(String thema, String titel, String filmWebsite) {
            // ;dslSrc=rtmp://gffstream.fcod.llnwd.net/a792/e1/media/video/2009/02/14/20090214_a40_komplett_big.flv&amp;isdnSrc=rtm
            // <p class="wsArticleAutor">Ein Beitrag von Heinke Schröder, 24.11.2010	</p>
            final String MUSTER_URL = "dslSrc=";
            final String MUSTER_DATUM = "<p class=\"wsArticleAutor\">";
            final String MUSTER_THEMA = "Homepage der Sendung ["; //Homepage der Sendung [west.art]</a>
            final String MUSTER_DURATION_START_0 = "<span class=\"moVideoIcon\">Video:";
            final String MUSTER_DURATION_START_1 = "(";
            final String MUSTER_DURATION_END = ")";
            final String MUSTER_DESCRIPTION_1 = "<meta name=\"description\" content=\"";
            final String MUSTER_DESCRIPTION_2 = "<meta name=\"Description\" content=\"";
            final String MUSTER_DESCRIPTION_END = "/>";
            final String MUSTER_THUMBNAIL = "<link rel=\"image_src\" href=\"";
            final String MUSTER_THUMBNAIL_END = "\"";
            final String MUSTER_IMAGE = "<img class=\"teaserPic\" src=\"";
            final String MUSTER_IMAGE_END = "\"";
            final String MUSTER_KEYWORDS = "<meta name=\"Keywords\" content=\"";
            final String MUSTER_KEYWORDS_END = "\"";
            meldung(filmWebsite);
            strSeite2 = getUrl.getUri_Iso(nameSenderMReader, filmWebsite, strSeite2, "");
            int pos;
            int pos1;
            int pos2;
            String url;
            String datum = "";
            long duration = 0;
            String description = "";
            String thumnail = "";
            String image = "";
            String[] keywords = new String[]{};
            
            if ((pos1 = strSeite2.indexOf(MUSTER_DURATION_START_0)) != -1) {
                pos1 += MUSTER_DURATION_START_0.length();
                if ((pos1 = strSeite2.indexOf(MUSTER_DURATION_START_1, pos1)) != -1) {
                    pos1 += MUSTER_DURATION_START_1.length();
                    if ((pos2 = strSeite2.indexOf(MUSTER_DURATION_END, pos1)) != -1) {
                        String d = strSeite2.substring(pos1, pos2);
                        try {
                            if (!d.equals("") && d.length() < 8 && d.contains(":")) {
                                String[] parts = d.split(":");
                                long power = 1;
                                for (int i = parts.length - 1; i >= 0; i--) {
                                    duration += Long.parseLong(parts[i]) * power;
                                    power *= 60;
                                }
                            }
                        } catch (Exception ex) {
                            Log.fehlerMeldung(-302058974, Log.FEHLER_ART_MREADER, "MediathekWdr.addFilme2-1", ex, "duration: " + d);
                        }
                    }
                }
            }
            if ((pos1 = strSeite2.indexOf(MUSTER_DESCRIPTION_1, 0)) != -1) {
                pos1 += MUSTER_DESCRIPTION_1.length();
                if ((pos2 = strSeite2.indexOf(MUSTER_DESCRIPTION_END, pos1)) != -1) {
                    description = strSeite2.substring(pos1, pos2);
                    if (description.startsWith("<p>")) {
                        description = description.replaceFirst("<p>", "");
                    }
                    if (description.endsWith("\"")) {
                        description = description.replace("\"", "");
                    }
                    if (description.endsWith("</p>")) {
                        description = description.replace("</p>", "");
                    }
                }
            } else if ((pos1 = strSeite2.indexOf(MUSTER_DESCRIPTION_2, 0)) != -1) {
                pos1 += MUSTER_DESCRIPTION_2.length();
                if ((pos2 = strSeite2.indexOf(MUSTER_DESCRIPTION_END, pos1)) != -1) {
                    description = strSeite2.substring(pos1, pos2);
                    if (description.startsWith("<p>")) {
                        description = description.replaceFirst("<p>", "");
                    }
                    if (description.endsWith("\"")) {
                        description = description.replace("\"", "");
                    }
                    if (description.endsWith("</p>")) {
                        description = description.replace("</p>", "");
                    }
                }
            }
            
            if ((pos1 = strSeite2.indexOf(MUSTER_THUMBNAIL)) != -1) {
                pos1 += MUSTER_THUMBNAIL.length();
                if ((pos2 = strSeite2.indexOf(MUSTER_THUMBNAIL_END, pos1)) != -1) {
                    thumnail = strSeite2.substring(pos1, pos2);
                }
            }
            
            if ((pos1 = strSeite2.indexOf(MUSTER_IMAGE)) != -1) {
                pos1 += MUSTER_IMAGE.length();
                if ((pos2 = strSeite2.indexOf(MUSTER_IMAGE_END, pos1)) != -1) {
                    image = "http://www.wdr.de" + strSeite2.substring(pos1, pos2);
                }
            }
            
            if ((pos1 = strSeite2.indexOf(MUSTER_KEYWORDS)) != -1) {
                pos1 += MUSTER_KEYWORDS.length();
                if ((pos2 = strSeite2.indexOf(MUSTER_KEYWORDS_END, pos1)) != -1) {
                    String k = strSeite2.substring(pos1, pos2);
                    keywords = k.split(", ");
                }
            }
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
                            //DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, ""/* zeit */);
                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, url, datum, ""/* zeit */, duration, description, thumnail, image, keywords);
                            addFilm(film);
                        } else {
                            Log.fehlerMeldung(-632917318, Log.FEHLER_ART_MREADER, "MediathekWdr.addFilmeRockpalast", "keine Url" + thema);
                        }
                    }
                }
            } else {
                Log.fehlerMeldung(-596631004, Log.FEHLER_ART_MREADER, "MediathekWdr.addFilmeRockpalast", "keine Url" + thema);
            }
        }
    }
}
