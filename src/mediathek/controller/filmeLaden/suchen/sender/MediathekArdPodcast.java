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
import mediathek.daten.Daten;
import mediathek.tool.Log;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;

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
    public MediathekArdPodcast(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 4, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://www.ardmediathek.de/ard/servlet/ajax-cache/3551682/view=module/index.html";
        final String MUSTER_URL = "link\": \"";
        final String MUSTER_THEMA = "{ \"titel\": \"";
        final String MUSTER_SET = "http://www.ardmediathek.de";
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        seite = getUrlIo.getUri_Utf(nameSenderMReader, ADRESSE, seite, "");
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
                if (!url.startsWith("/podcast/")) {
                    // nur dann ARD.Podcast
                    continue;
                }
                String[] add = new String[]{MUSTER_SET + url, thema};
                listeThemen.addUrl(add);
            } catch (Exception ex) {
                Log.fehlerMeldung(-764238903, "MediathekArdPodcast.addToList", ex, "kein Thema");
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                listeSort(listeThemen, 1);
                for (int t = 0; t < maxThreadLaufen; ++t) {
                    new Thread(new ArdThemaLaden()).start();
                }
            }
        }
    }

    private class ArdThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite = new StringBuffer();
        private StringBuffer seite2 = new StringBuffer();

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
                Log.fehlerMeldung(-460287629, "MediathekArdPodcast.ArdThemaLaden.run", ex);
            }
        }

        private void feedEinerSeiteSuchen(String strUrlFeed, String thema) {
            //Feed eines Themas laden
            //<a class="mt-box_preload mt-box-overflow" href="/ard/servlet/ajax-cache/3516938/view=switch/documentId=427262/index.html">
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            final String MUSTER = "<a class=\"mt-box_preload mt-box-overflow\" href=\"";
            final String MUSTER_SET = "http://www.ardmediathek.de";
            LinkedList<String> listeWeiter = new LinkedList<String>();
            boolean weiter = false;
            final String MUSTER_WEITER = "<option value=\"";
            seite = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite, "Thema: " + thema);
            pos = 0;
            pos1 = 0;
            pos2 = 0;
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
                    Log.fehlerMeldungMReader(-643188097, "MediathekArdPodcast.filmeEinerSeiteSuchen-1", "keine URL für: " + strUrlFeed);
                } else {
                    url = MUSTER_SET + url;
                    //++++++++++++++++++++++++++++++++++ 2te Seite
                    //<h3 class="mt-title"><a href="/ard/servlet/content/3516968?documentId=1441144"
                    //final String MUSTER_2a = "<h3 class=\"mt-title\"><a href=\"";
                    final String MUSTER_2a = "<a href=\"";
                    final String MUSTER_2b = "\" class=\"mt-btt_rss\" onclick=\"";
                    String tmpUrl = url;
                    seite.setLength(0);
                    seite = getUrl.getUri_Utf(nameSenderMReader, url, seite, "Thema: " + thema);
                    pos = 0;
                    pos1 = 0;
                    pos2 = 0;
                    while ((pos = seite.indexOf(MUSTER_WEITER, pos)) != -1) {
                        pos += MUSTER_WEITER.length();
                        pos1 = pos;
                        pos2 = seite.indexOf("\"", pos);
                        if (pos1 != -1 && pos2 != -1) {
                            String tmpWeiter = MUSTER_SET + seite.substring(pos1, pos2);
                            listeWeiter.add(tmpWeiter);
                        }
                    }
                    do {
                        pos = 0;
                        pos1 = 0;
                        pos2 = 0;
                        url = "";
                        while (!Daten.filmeLaden.getStop() && (pos = seite.indexOf(MUSTER_2a, pos)) != -1) {
                            pos += MUSTER_2a.length();
                            pos1 = pos;
                            pos2 = seite.indexOf("\"", pos);
                            if (pos1 != -1 && pos2 != -1) {
                                url = seite.substring(pos1, pos2);
                            }
                            if (url.equals("")) {
                                //<a href="/ard/servlet/content/3517244?documentId=590570" class="mt-btt_rss" onclick="
                                pos = 0;
                                pos1 = 0;
                                pos2 = 0;
                                url = "";
                                if ((pos = seite.indexOf(MUSTER_2b, pos)) != -1) {
                                    pos1 = seite.lastIndexOf("\"");
                                    if (pos1 != -1) {
                                        url = seite.substring(pos1);
                                    }
                                }
                            }
                            if (url.equals("#")) {
                                Log.fehlerMeldungMReader(-698025468, "MediathekArdPodcast.filmeEinerSeiteSuchen-3", "keine URL für: " + tmpUrl);
                            } else if (url.equals("")) {
                                //-------------
                                Log.fehlerMeldungMReader(-456903578, "MediathekArdPodcast.filmeEinerSeiteSuchen-2", "keine URL für: " + strUrlFeed);
                            } else {
                                url = MUSTER_SET + url;
                                filmLaden(strUrlFeed, url, thema);
                            }
                        }
                        if (suchen.allesLaden && listeWeiter.size() > 0) {
                            url = listeWeiter.pollFirst();
                            seite = getUrl.getUri_Utf(nameSenderMReader, url, seite, "Thema: " + thema);
                            weiter = true;
                        } else {
                            weiter = false;
                        }
                    } while (weiter);
                }
            }

        }

        private void filmLaden(String strUrlFeed, String url, String thema) {
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
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
                String tmpUrl = url;
                seite2.setLength(0);
                seite2 = getUrl.getUri_Utf(nameSenderMReader, url, seite2, "Thema: " + thema);
                pos = 0;
                pos1 = 0;
                pos2 = 0;
                url = "";
                if ((pos = seite2.indexOf(MUSTER_3, pos)) != -1) {
                    pos += MUSTER_3.length();
                    pos1 = pos;
                    pos2 = seite2.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite2.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        //-------------
                        Log.fehlerMeldungMReader(-789628694, "MediathekArdPodcast.filmeEinerSeiteSuchen-3", "keine URL für: " + tmpUrl);
                        Log.fehlerMeldungMReader(-495623876, "MediathekArdPodcast.filmeEinerSeiteSuchen-3", "keine URL für: " + strUrlFeed);
                    } else {
                        //<title>ARD Mediathek: DiD-Folge 925: Die Dünnbrett-Bohrer - 16.05.2012 | Bayerisches Fernsehen</title>
                        //<title>ARD Mediathek: 28 Stunden Ausnahmezustand in Freiburg | SWR Fernsehen BW</title>
                        //<title>ARD Mediathek: Die Alpenüberquerung - Hoffentlich schwindelfrei | SWR Fernsehen</title>
                        //<title>ARD Mediathek: Die Wahrheit über Deutschland: Leidenschaft | DW-TV</title>
                        //<title>ARD Mediathek: Gipfeltreffen mit Ilse Neubauer - 17.05.2012 | Bayerisches Fernsehen</title>
                        //<title>ARD Mediathek: Angeklickt: 18.05.2012, Es muss nicht immer Facebook sein | WDR Fernsehen</title>
                        final String MUSTER_TITEL = "<title>";
                        if ((pos1 = seite2.indexOf(MUSTER_TITEL, 0)) != -1) {
                            pos1 += MUSTER_TITEL.length();
                            if ((pos2 = seite2.indexOf("<", pos1)) != -1) {
                                String titel = seite2.substring(pos1, pos2);
                                if (titel.startsWith("ARD Mediathek:")) {
                                    titel = titel.replaceFirst("ARD Mediathek:", "").trim();
                                    if (titel.contains("|")) {
                                        titel = titel.substring(0, titel.lastIndexOf("|"));
                                        titel = titel.trim();
                                        String datum = "";
                                        if (titel.contains(" - ") && titel.contains("20")) {
                                            datum = titel.substring(titel.lastIndexOf(" - ") + 3).trim();
                                            if (datum.length() != 10) {
                                                //noch ein Versuch
                                                if (titel.contains(".20")) {
                                                    int p = titel.indexOf(".20");
                                                    if (p > 6 && (p + 6) < titel.length()) {
                                                        datum = titel.substring(titel.indexOf(".20") - 5, titel.indexOf(".20") + 5);
                                                        titel = titel.replace(datum, "").trim();
                                                    }
                                                }
                                            } else {
                                                titel = titel.substring(0, titel.lastIndexOf(" - ")).trim();
                                            }
                                        }
                                        //  DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
                                        meldung(url);
                                        addFilm(new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, ""));
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
