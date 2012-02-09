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
import mediathek.daten.DatenFilm;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.controller.io.GetUrl;
import mediathek.Log;

/**
 *
 * @author
 */
public class MediathekSfPod extends MediathekReader implements Runnable {

    public static final String SENDER = "SF.Podcast";
    private StringBuffer seite = new StringBuffer();

    /**
     * 
     * @param ddaten
     */
    public MediathekSfPod(FilmeSuchen ssearch) {
        super(ssearch, /* name */ SENDER, /* text */ "SF.Podcast (ca. 3 MB, 100 Filme)", /* threads */ 2, /* urlWarten */ 1000);
    }

    /**
     * 
     */
    @Override
    public void addToList() {
        //Liste von http://www.sf.tv/podcasts/index.php holen
        //http://www.podcast.sf.tv/Podcasts/al-dente
        // class="" href="/Podcasts/al-dente" rel="2" >
        final String MUSTER = "class=\"\" href=\"/Podcasts/";
        String addr1 = "http://www.podcast.sf.tv/";
        listeThemen.clear();
        seite = getUrlIo.getUri_Utf(senderName, addr1, seite, "");
        int pos = 0;
        int pos1 = 0;
        int pos2 = 0;
        String url = "";
        while (!Daten.filmeLaden.getStop() && (pos = seite.indexOf(MUSTER, pos)) != -1) {
            pos += MUSTER.length();
            pos1 = pos;
            pos2 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
                if (!url.startsWith("http://www.sf.tv")) {
                    url = "http://www.podcast.sf.tv/Podcasts/" + url;
                }
            }
            if (url.equals("")) {
                Log.fehlerMeldung("MediathekSfPod.addToList", "keine URL");
            } else {
                String[] add = new String[]{url, ""};
                if (!istInListe(listeThemen, url, 0)) {
                    listeThemen.add(add);
                }
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                for (int t = 0; t < senderMaxThread; ++t) {
                    new Thread(new SfThemaLaden()).start();
                }
            }
        }
    }

    private class SfThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl( senderWartenSeiteLaden);
        private StringBuffer seite = new StringBuffer();

        @Override
        public void run() {
            try {
                meldungAddThread();
                String link[];
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    meldungProgress(link[0] /* url */);
                    addFilme(link[1], link[0] /* url */);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekSfPod.SfThemaLaden.run", ex);
            }
        }

        void addFilme(String thema, String strUrlFeed) {
            //<title>al dente - Podcasts - Schweizer Fernsehen</title>
            // <h2 class="sf-hl1">al dente vom 28.06.2010</h2>
            // <li><a href="
            final String MUSTER_THEMA_1 = "<title>";
            final String MUSTER_THEMA_2 = "</title>";
            final String MUSTER_TITEL_1 = "<h2 class=\"sf-hl1\">";
            final String MUSTER_TITEL_2 = "</h2>";
            final String MUSTER_URL_1 = "<li><a href=\"";
            final String MUSTER_URL_2 = "\"";
            int pos = 0;
            int posEnd = 0;
            String titel = "";
            String url = "";
            String datum = "";
            try {
                meldung("*" + strUrlFeed);
                seite = getUrl.getUri_Utf(senderName, strUrlFeed, seite, "Thema: " + thema);
                pos = seite.indexOf(MUSTER_THEMA_1);
                if (pos != -1) {
                    pos = pos + MUSTER_THEMA_1.length();
                }
                posEnd = seite.indexOf(MUSTER_THEMA_2);
                if (posEnd != -1) {
                    if (pos < posEnd) {
                        thema = seite.substring(pos, posEnd);
                        if (thema.contains(" ")) {
                            thema = thema.substring(0, thema.indexOf(" "));
                        }
                    }
                }
                pos = seite.indexOf(MUSTER_TITEL_1, pos); //start der Einträge, erster Eintrag ist der Titel
                if (pos != -1) {
                    pos = pos + MUSTER_TITEL_1.length();
                    while ((pos = seite.indexOf(MUSTER_TITEL_1, pos)) != -1) {
                        url = "";
                        pos += MUSTER_TITEL_1.length();
                        posEnd = seite.indexOf(MUSTER_TITEL_2, pos);
                        if (posEnd != -1) {
                            titel = seite.substring(pos, posEnd);
                            titel = titel.trim();
                        }
                        if (titel.contains("vom")) {
                            datum = titel.substring(titel.indexOf("vom") + 3).trim();
                        }
                        pos = seite.indexOf(MUSTER_URL_1, pos);
                        if (pos == -1) {
                            break;
                        }
                        pos += MUSTER_URL_1.length();
                        posEnd = seite.indexOf(MUSTER_URL_2, pos);
                        if (posEnd != -1) {
                            url = seite.substring(pos, posEnd);
                        }
                        if (url.equals("")) {
                            Log.fehlerMeldung("MediathekSfPod.addFilme", "keine URL: " + strUrlFeed);
                        } else {
//                            urlorg = urlorg.replace("%20", "\u0020;");
                            addFilm(new DatenFilm(senderName, thema, strUrlFeed, titel, url, datum, ""/* zeit */));
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekSfPod.addFilme", ex);
            }
        }
    }
}
