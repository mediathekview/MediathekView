/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 *
 * thausherr
 *
 *
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
import mediathek.controller.filme.filmeSuchenSender.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;

/**
 *
 * @author
 */
public class MediathekOrf extends MediathekReader implements Runnable {

    public static final String SENDER = "ORF";

    /**
     *
     * @param ddaten
     */
    public MediathekOrf(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 1000, startPrio);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://tvthek.orf.at/topics"; //TH
        final String MUSTER_URL1 = "<a href=\""; //TH
        final String MUSTER_URL2 = "/programs/";
        final String MUSTER_URL2b = "/topics/"; //TH
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        seite = getUrlIo.getUri_Utf(nameSenderMReader, ADRESSE, seite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        String thema = "";
        //Podcasts auslesen
        while ((pos = seite.indexOf(MUSTER_URL1, pos)) != -1) {
            try {
                pos += MUSTER_URL1.length();
                //TH
                String m = MUSTER_URL2;
                int p = seite.indexOf(m, pos);
                if (p == -1) {
                    // Plan B
                    m = MUSTER_URL2b;
                    p = seite.indexOf(m, pos);
                }
                pos = p;
                //TH ende
                if (pos != -1) {
                    pos += m.length(); //TH
                    pos1 = pos;
                    pos2 = seite.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite.substring(pos1, pos2);
                    }
                    //TH neu: " title="ZIB 24: Spott aus Litauen">
                    pos1 = seite.indexOf("title=\"", pos) + 6; //TH
                    pos2 = seite.indexOf("\">", pos); //TH
                    if (pos1 != -1 && pos2 != -1 && pos1 < pos2) {
                        thema = seite.substring(pos1 + 1, pos2);
                        //TH
                        if (thema.endsWith(" aufrufen...")) {
                            thema = thema.replace(" aufrufen...", "");
                        }
                    }
                    if (url.equals("")) {
                        continue;
                    }
                    String[] add = new String[]{
                        "http://tvthek.orf.at" + m + url, thema //TH
                    };
                    if (!istInListe(listeThemen, add[0], 0)) {
                        listeThemen.add(add);
                    }
                } else {
                    break; //TH muss sein da muster 2 manchmal nicht fündig - dann Endlosschleife
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-469732500, "MediathekOrf.addToList", ex);
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                listeSort(listeThemen, 1);
                for (int t = 0; t < maxThreadLaufen; ++t) {
                    new Thread(new OrfThemaLaden()).start();
                }
            }
        }
    }

    private class OrfThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    try {
                        meldungProgress(link[0]);
                        feedEinerSeiteSuchen(link[0] /* url */, link[1] /* Thema */);
                    } catch (Exception ex) {
                        Log.fehlerMeldung(-795633581, "MediathekOrf.OrfThemaLaden.run", ex);
                    }
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung(-554012398, "MediathekOrf.OrfThemaLaden.run", ex);
            }
        }

        void feedEinerSeiteSuchen(String strUrlFeed, String thema) {
            //<param name="URL" value="/programs/1306-Newton/episodes/1229327-Newton/1231597-Signation---Themenuebersicht.asx" />
            //<title> ORF TVthek: a.viso - 28.11.2010 09:05 Uhr</title>
            final String MUSTER = "<param name=\"URL\" value=\"";
            final String MUSTER_SET = "http://tvthek.orf.at";
            final String MUSTER_DATUM_1 = "<span>"; //TH
            final String MUSTER_DATUM_2 = "Uhr</span>"; //TH
            seite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite1, "Thema: " + thema);
            int pos = 0;
            int pos1;
            int pos2;
            String url = "";
            String datum = "";
            String zeit = "";
            String tmp;
            if ((pos1 = seite1.indexOf(MUSTER_DATUM_1)) != -1) {
                pos1 += MUSTER_DATUM_1.length();
                if ((pos2 = seite1.indexOf(MUSTER_DATUM_2, pos1)) != -1) {
                    tmp = seite1.substring(pos1, pos2);
                    if (tmp.contains("-")) {
                        tmp = tmp.substring(tmp.lastIndexOf("-") + 1).trim();
                        if (tmp.contains(" ")) {
                            datum = tmp.substring(0, tmp.indexOf(" ")).trim();
                            zeit = tmp.substring(tmp.indexOf(" "));
                            zeit = zeit.replace("Uhr", "").trim() + ":00";
                        }
                    }
                }
            }
            if ((pos = seite1.indexOf(MUSTER, pos)) != -1) {
                try {
                    pos += MUSTER.length();
                    pos1 = pos;
                    pos2 = seite1.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite1.substring(pos1, pos2);
                    }
                    if (!url.equals("")) {
                        //TH ggf. Trennen in Thema und Titel
                        String titel = thema;
                        int dp = thema.indexOf(": ");
                        if (dp != -1) {
                            titel = thema.substring(dp + 2);
                            thema = thema.substring(0, dp);
                        }//TH titel und thema getrennt
                        addFilm(new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, MUSTER_SET + url, datum, zeit));
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(-336987510, "MediathekOrf.feedEinerSeiteSuchen", ex);
                }
            }
        }
    }
}
