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
package mediathek.controller.filme.filmeSuchen.sender;

import mediathek.Daten;
import mediathek.Log;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;
import mediathek.tool.DatumZeit;

/**
 *
 * @author
 */
public class MediathekArdPodcast extends MediathekReader implements Runnable {

    public static final String SENDER = "ARD.Podcast";

    /**
     *
     * @param ddaten
     */
    public MediathekArdPodcast(FilmeSuchen ssearch) {
        super(ssearch, /* name */ SENDER, /* text */ "ARD-Podcasts  (bis ca. 12 MB, bis 3600 Filme)", /* threads */ 4, /* urlWarten */ 500);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://www.ardmediathek.de/ard/servlet/ajax-cache/3551682/view=module/index.html";
        final String MUSTER_URL = "link\": \"";
        final String MUSTER_THEMA = "{ \"titel\": \"";
        final String MUSTER_SET = "http://www.ardmediathek.de";
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        seite = getUrlIo.getUri_Utf(senderName, ADRESSE, seite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        String thema = "";
        //Podcasts auslesen
        while ((pos = seite.indexOf(MUSTER_THEMA, pos)) != -1) {
            try {
                pos += MUSTER_THEMA.length();
                pos1 = pos;
                pos2 = seite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1) {
                    thema = seite.substring(pos1, pos2);
                    if (!themaLaden(senderName, thema)) {
                        //nur Abos laden
                        continue;
                    }
                }
                pos1 = seite.indexOf(MUSTER_URL, pos);
                pos1 = pos1 + MUSTER_URL.length();
                pos2 = seite.indexOf("\"", pos1);
                if (pos1 != -1 && pos2 != -1 && pos1 < pos2) {
                    url = seite.substring(pos1, pos2);
                }
                if (url.equals("")) {
                    continue;
                }
                String[] add = new String[]{MUSTER_SET + url, thema};
                if (!istInListe(listeThemen, url, 0)) {
                    listeThemen.add(add);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekArdPodcast.addToList", ex, "kein Thema");
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                listeSort(listeThemen, 1);
                for (int t = 0; t < senderMaxThread; ++t) {
                    new Thread(new ArdThemaLaden()).start();
                }
            }
        }
    }

    private class ArdThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl();
        private StringBuffer seite = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    feedEinerSeiteSuchen(link[0] /* url */, link[1] /* Thema */);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekArdPodcast.ArdThemaLaden.run", ex);
            }
        }

        private void feedEinerSeiteSuchen(String strUrlFeed, String thema) {
            //Feed eines Themas laden
            //<a class="mt-box_preload mt-box-overflow" href="/ard/servlet/ajax-cache/3516938/view=switch/documentId=427262/index.html">
            final String MUSTER = "<a class=\"mt-box_preload mt-box-overflow\" href=\"";
            final String MUSTER_SET = "http://www.ardmediathek.de";
            seite = getUrl.getUri_Utf(senderName, strUrlFeed, seite, "Thema: " + thema);
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            //++++++++++++++++++++++++++++++++++ 1te Seite
            if ((pos = seite.indexOf(MUSTER, pos)) != -1) {
                pos += MUSTER.length();
                pos1 = pos;
                pos2 = seite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1) {
                    url = seite.substring(pos1, pos2);
                }
                if (url.equals("")) {
                    //-------------
                    Log.fehlerMeldung("MediathekArdPodcast.filmeEinerSeiteSuchen-1", "keine URL für: " + strUrlFeed);
                } else {
                    url = MUSTER_SET + url;
                    //++++++++++++++++++++++++++++++++++ 2te Seite
                    //<h3 class="mt-title"><a href="/ard/servlet/content/3516968?documentId=1441144"
                    //final String MUSTER_2a = "<h3 class=\"mt-title\"><a href=\"";
                    final String MUSTER_2a = "<a href=\"";
                    String tmpUrl = url;
                    seite.setLength(0);
                    seite = getUrl.getUri_Utf(senderName, url, seite, "Thema: " + thema);
                    pos = 0;
                    pos1 = 0;
                    pos2 = 0;
                    url = "";
                    if ((pos = seite.indexOf(MUSTER_2a, pos)) != -1) {
                        pos += MUSTER_2a.length();
                        pos1 = pos;
                        pos2 = seite.indexOf("\"", pos);
                        if (pos1 != -1 && pos2 != -1) {
                            url = seite.substring(pos1, pos2);
                        }
                        if (url.equals("")) {
                            //<a href="/ard/servlet/content/3517244?documentId=590570" class="mt-btt_rss" onclick="
                            final String MUSTER_2b = "\" class=\"mt-btt_rss\" onclick=\"";
                            pos = 0;
                            pos1 = 0;
                            pos2 = 0;
                            url = "";
                            String temp = "";
                            if ((pos = seite.indexOf(MUSTER_2b, pos)) != -1) {
                                temp = seite.substring(0, pos);
                                pos1 = seite.lastIndexOf("\"");
                                if (pos1 != -1) {
                                    url = seite.substring(pos1);
                                }
                            }
                        }
                        if (url.equals("#")) {
                            Log.fehlerMeldung("MediathekArdPodcast.filmeEinerSeiteSuchen-3", "keine URL für: " + tmpUrl);
                        } else if (url.equals("")) {
                            //-------------
                            Log.fehlerMeldung("MediathekArdPodcast.filmeEinerSeiteSuchen-2", "keine URL für: " + strUrlFeed);
                        } else {
                            url = MUSTER_SET + url;
                            final String MUSTER_ = "http://www.ardmediathek.de/ard/servlet/content/3517244";
                            if (url.contains("?")) {
                                //3517136 ersetzen mit 3517244
                                //http://www.ardmediathek.de/ard/servlet/content/3516968?documentId=2584998
                                //
                                url = MUSTER_ + url.substring(url.indexOf("?"));
                                //++++++++++++++++++++++++++++++++++ 3te Seite
                                //<input name="" type="text" value="http://www1.swr.de/podcast/xml/swr-fernsehen/60-jahre-rlp.xml" />
                                //final String MUSTER_3 = "<input name=\"\" type=\"text\" value=\"";
                                final String MUSTER_3 = "addMediaStream(0, 1, \"\", \"";
                                tmpUrl = url;
                                seite.setLength(0);
                                seite = getUrl.getUri_Utf(senderName, url, seite, "Thema: " + thema);
                                pos = 0;
                                pos1 = 0;
                                pos2 = 0;
                                url = "";
                                if ((pos = seite.indexOf(MUSTER_3, pos)) != -1) {
                                    pos += MUSTER_3.length();
                                    pos1 = pos;
                                    pos2 = seite.indexOf("\"", pos);
                                    if (pos1 != -1 && pos2 != -1) {
                                        url = seite.substring(pos1, pos2);
                                    }
                                    if (url.equals("")) {
                                        //-------------
                                        Log.fehlerMeldung("MediathekArdPodcast.filmeEinerSeiteSuchen-3", "keine URL für: " + tmpUrl);
                                        Log.fehlerMeldung("MediathekArdPodcast.filmeEinerSeiteSuchen-3", "keine URL für: " + strUrlFeed);
                                    } else {
//////                                        filmLaden(strUrlFeed, url, thema);
                                        //<title>ARD Mediathek: DiD-Folge 925: Die Dünnbrett-Bohrer - 16.05.2012 | Bayerisches Fernsehen</title>
                                        //<title>ARD Mediathek: 28 Stunden Ausnahmezustand in Freiburg | SWR Fernsehen BW</title>
                                        //<title>ARD Mediathek: Die Wahrheit über Deutschland: Leidenschaft | DW-TV</title>
                                        //<title>ARD Mediathek: Gipfeltreffen mit Ilse Neubauer - 17.05.2012 | Bayerisches Fernsehen</title>
                                        final String MUSTER_TITEL = "<title>";
                                        if ((pos1 = seite.indexOf(MUSTER_TITEL, 0)) != -1) {
                                            pos1 += MUSTER_TITEL.length();
                                            if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                                                String titel = seite.substring(pos1, pos2);
                                                if (titel.startsWith("ARD Mediathek:")) {
                                                    titel = titel.replaceFirst("ARD Mediathek:", "").trim();
                                                    if (titel.contains("|")) {
                                                        titel = titel.substring(0, titel.lastIndexOf("|") );
                                                        titel = titel.trim();
                                                        //    public DatenFilm( ddaten,  ssender,  tthema,  urlThema,  ttitel,  uurl,  uurlorg,  uurlRtmp, uurlHd) {
                                                        addFilm(new DatenFilm(senderName, thema, strUrlFeed, titel, url, "", ""));
                                                    }

                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
////        boolean filmLaden(String urlThema, String urlFeed, String thema) {
////            //<item>
////            //<title>angeklickt: 26.11.2010, Zeitung lesen auf dem Tablet-PC</title>
////            //<link>http://medien.wdr.de/m/1290794400/angeklickt/wdr_fernsehen_angeklickt_20101126.mp4</link>
////            //<pubDate>Fri, 26 Nov 2010 19:00:00 +0100</pubDate>
////            //
////            //<description>Immer mehr Menschen lesen Zeitungen, Zeitschriften und Bücher auf elektronischen Lesegeräten. Diese Woche hat ein großer Medienkonzern sogar eine tägliche Zeitung angekündigt, die ausschließlich auf dem iPad erscheinen wird. Ein Trend? ; © WDR VideoPodcast</description>
////            //<guid isPermaLink="false">/wdr_fernsehen_angeklickt_20101126.mp4</guid>
////            //<enclosure url="http://medien.wdr.de/m/1290794400/angeklickt/wdr_fernsehen_angeklickt_20101126.mp4" length="15522707" type="video/mp4" />
////            //</item>
////
////            final String MUSTER_ITEM = "<item>";
////            final String MUSTER_ITEM_ENDE = "</item>";
////            final String MUSTER_TITEL = "<title>";
////            final String MUSTER_URL = "<enclosure url=\"";
////            final String MUSTER_DATUM = "<pubDate>";
////            boolean ret = false;
////            seite = getUrl.getUri_Utf(senderName, urlFeed, seite, "Thema: " + thema);
////            int posItem = 0;
////            int posItemEnde = 0;
////            int pos = 0;
////            int pos1 = 0;
////            int pos2 = 0;
////            String url = "";
////            String titel = "";
////            String datum = "";
////            String zeit = "";
////            String tmp = "";
////            while (!Daten.filmeLaden.getStop() && (posItem = seite.indexOf(MUSTER_ITEM, posItem)) != -1) {
////                posItem += MUSTER_ITEM.length();
////                pos = posItem;
////                posItemEnde = seite.indexOf(MUSTER_ITEM_ENDE, posItem);
////                url = "";
////                titel = "";
////                datum = "";
////                zeit = "";
////                tmp = "";
////                if ((pos1 = seite.indexOf(MUSTER_TITEL, pos)) != -1) {
////                    pos1 += MUSTER_TITEL.length();
////                    if ((pos2 = seite.indexOf("</title>", pos1)) != -1) {
////                        titel = seite.substring(pos1, pos2);
////                        titel = titel.replace("<![CDATA", "");
////                        titel = titel.replace("[", "");
////                        titel = titel.replace("]", "");
////                        titel = titel.replace("<", "");
////                        titel = titel.replace(">", "");
////                    }
////                    if (titel.equals("")) {
////                        Log.fehlerMeldung("MediathekArdPodcast.filmeLaden", "kein Titel: " + urlFeed);
////                    }
////                }
////                if ((pos1 = seite.indexOf(MUSTER_DATUM, pos)) != -1) {
////                    pos1 += MUSTER_DATUM.length();
////                    if ((pos2 = seite.indexOf("<", pos1)) != -1) {
////                        //<pubDate>Mon, 03 Jan 2011 17:06:16 +0100</pubDate>
////                        tmp = seite.substring(pos1, pos2);
////                        if (tmp.equals("")) {
////                            Log.fehlerMeldung("MediathekArdPodcast.filmeLaden", "keine Datum" + urlFeed);
////                        } else {
////                            datum = DatumZeit.convertDatum(tmp);
////                            zeit = DatumZeit.convertTime(tmp);
////                        }
////                    }
////                }
////                if ((pos1 = seite.indexOf(MUSTER_URL, pos)) != -1) {
////                    pos1 += MUSTER_URL.length();
////                    if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
////                        url = seite.substring(pos1, pos2);
////                        if (url.equals("")) {
////                            Log.fehlerMeldung("MediathekArdPodcast.filmeLaden", "keine URL: " + urlFeed);
////                        } else {
////                            //    public DatenFilm( ddaten,  ssender,  tthema,  urlThema,  ttitel,  uurl,  uurlorg,  uurlRtmp, uurlHd) {
////                            addFilm(new DatenFilm(senderName, thema, urlThema, titel, url, datum, zeit));
////                            ret = true;
////                        }
////                    }
////                }
////            }
////            return ret;
////        }
////    }
}
