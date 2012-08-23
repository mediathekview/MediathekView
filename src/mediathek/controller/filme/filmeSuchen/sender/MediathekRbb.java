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
package mediathek.controller.filme.filmeSuchen.sender;

import mediathek.Daten;
import mediathek.Log;
import mediathek.controller.filme.filmeSuchenSender.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;

public class MediathekRbb extends MediathekReader implements Runnable {

    public static final String SENDER = "RBB";
    final String ROOTADR = "http://mediathek.rbb-online.de";

    public MediathekRbb(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        int pos1 = 0;
        int pos2 = 0;
        StringBuffer seite1 = new StringBuffer();
        StringBuffer seite2 = new StringBuffer();
        final String ADRESSE = "http://mediathek.rbb-online.de/fernsehen";
        final String ITEM_1 = "<a href=\"/rbb/servlet/ajax-cache/";
        final String ITEM_URL = "http://mediathek.rbb-online.de/rbb/servlet/ajax-cache/";
        try {
            seite1 = getUrlIo.getUri_Utf(nameSenderMReader, ADRESSE, seite1, "");
            while ((pos1 = seite1.indexOf(ITEM_1, pos1)) != -1) {
                pos1 = pos1 + ITEM_1.length();
                if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                    String url = ITEM_URL + seite1.substring(pos1, pos2).replace("view=switch", "view=list");
                    if (!url.equals("")) {
                        seite2 = getUrlIo.getUri_Utf(nameSenderMReader, url, seite2, "");
                        int lpos1 = 0;
                        int lpos2 = 0;
                        final String LIST_ITEM = "<h3 class=\"mt-title\"><a href=\"";
                        while ((lpos1 = seite2.indexOf(LIST_ITEM, lpos1)) != -1) {
                            lpos1 = lpos1 + LIST_ITEM.length();
                            lpos2 = seite2.indexOf("\"", lpos1);
                            String listurl = ROOTADR + seite2.substring(lpos1, lpos2);
                            if (!listurl.equals("")) {
                                String[] add = new String[]{listurl, ""};
                                listeThemen.addUrl(add);
                            }
                        }
                    }
                } else {
                    Log.fehlerMeldungMReader(-894562036, "MediathekRBB.addToList", "keine URL");
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(-398214058, "MediathekRBB.addToList", ex);
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                listeSort(listeThemen, 1);
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
        private StringBuffer seite3 = new StringBuffer();

        @Override
        public void run() {
            try {
                meldungAddThread();
                String link[];
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    addFilme(link[0] /* url */);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung(-794625882, "MediathekRBB.ThemaLaden.run", ex);
            }
        }

        void addFilme(String url) {
            try {
                // Hierin nun einen RSS feed URL extrahieren
                final String RSS_ITEM = "<a href=\"/rbb/servlet/export/rss/";
                seite1.setLength(0);
                seite1 = getUrlIo.getUri_Utf(nameSenderMReader, url, seite1, "");
                int rpos = seite1.indexOf(RSS_ITEM);
                if (rpos > 0) {
                    int rpos1 = rpos + 9;
                    int rpos2 = seite1.indexOf("\"", rpos1);
                    String rssurl = ROOTADR + seite1.substring(rpos1, rpos2);

                    // Diesen RSS feed laden
                    seite2.setLength(0);
                    seite2 = getUrlIo.getUri_Utf(nameSenderMReader, rssurl, seite2, "");

                    rpos = 0;
                    int count = 0;
                    while ((rpos = seite2.indexOf("<link>", rpos)) != -1) {
                        if (!suchen.allesLaden) {
                            // beim Update nur die neuesten Laden
                            ++count;
                            if (count > 10) {
                                break;
                            }
                        }
                        rpos1 = rpos + 6;
                        rpos2 = seite2.indexOf("</link>", rpos1);
                        String showurl = seite2.substring(rpos1, rpos2);

                        // Wir haben den URL der Sendung
                        seite3.setLength(0);
                        seite3 = getUrlIo.getUri_Utf(nameSenderMReader, showurl, seite3, "");
                        meldung(showurl);

                        // Titel
                        int tpos = seite3.indexOf("<title>");
                        if (tpos > 0) {
                            int tpos2 = seite3.indexOf("</title>", tpos);
                            String title = seite3.substring(tpos + 7, tpos2);
                            title = title.substring(15); // " rbb Mediathek: " abschneiden
                            String datum = title.substring(title.length() - 26, title.length() - 16);
                            String thema = title.substring(0, title.indexOf(" - "));
                            title = title.substring(title.indexOf(" - ") + 3, title.indexOf(" - ", thema.length() + 3));

                            int mpos = seite3.indexOf("mp4:");
                            int mpos2 = seite3.indexOf("\"", mpos);
                            String filmurl = seite3.substring(mpos, mpos2);
                            String urlRtmp = "--host ondemand.rbb-online.de --app ondemand/ --playpath " + filmurl;
                            String urlOrg = addsUrl("rtmp://ondemand.rbb-online.de/ondemand/", filmurl);
                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, showurl, title, urlOrg, urlOrg, urlRtmp, datum, ""/* zeit */);
                            addFilm(film);
                        }
                        rpos = rpos2; // hinter Element gehts weiter
                    }
                }
            } catch (Exception ex) {
            }
        }
    }
}