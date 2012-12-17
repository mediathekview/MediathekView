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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.DatumZeit;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

/**
 *
 *        @author
 */
public class MediathekSfPod extends MediathekReader implements Runnable {

    public static final String SENDER = "SF.Podcast";
    private StringBuffer seite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

    /**
     *
     *        @param ddaten
     */
    public MediathekSfPod(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 1000, startPrio);
    }

    /**
     *
     */
    @Override
    public void addToList() {
        //Liste von http://www.sf.tv/podcasts/index.php holen
        //http://www.podcast.sf.tv/Podcasts/al-dente
        // class="" href="/Podcasts/al-dente" rel="2" >
        final String MUSTER_1 = "value=\"http://feeds.sf.tv/podcast";
        final String MUSTER_2 = "value=\"http://pod.drs.ch/";
        String addr1 = "http://www.podcast.sf.tv/";
        listeThemen.clear();
        meldungStart();
        seite = getUrlIo.getUri_Utf(nameSenderMReader, addr1, seite, "");
        int pos = 0;
        int pos1 = 0;
        int pos2 = 0;
        String url = "";
        while (!Daten.filmeLaden.getStop() && (pos = seite.indexOf(MUSTER_1, pos)) != -1) {
            pos += MUSTER_1.length();
            pos1 = pos;
            pos2 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
                url = "http://feeds.sf.tv/podcast" + url;
            }
            if (url.equals("")) {
                Log.fehlerMeldungMReader(-698875503, "MediathekSfPod.addToList", "keine URL");
            } else {
                String[] add = new String[]{url, ""};
                listeThemen.addUrl(add);
            }
        }
        pos = 0;
        while (!Daten.filmeLaden.getStop() && (pos = seite.indexOf(MUSTER_2, pos)) != -1) {
            pos += MUSTER_2.length();
            pos1 = pos;
            pos2 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
                url = "http://pod.drs.ch/" + url;
            }
            if (url.equals("")) {
                Log.fehlerMeldungMReader(-698875503, "MediathekSfPod.addToList", "keine URL");
            } else {
                String[] add = new String[]{url, ""};
                listeThemen.addUrl(add);
            }
        }
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new SfThemaLaden()).start();
            }
        }
    }

    private class SfThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public void run() {
            try {
                meldungAddThread();
                String link[];
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    meldungProgress(link[0] /* url */);
                    addFilme(link[1], link[0] /* url */);
                }
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-286931004, "MediathekSfPod.SfThemaLaden.run", ex.getMessage());
            }
            meldungThreadUndFertig();
        }

        void addFilme(String thema, String strUrlFeed) {
            //<title>al dente - Podcasts - Schweizer Fernsehen</title>
            // <h2 class="sf-hl1">al dente vom 28.06.2010</h2>
            // <li><a href="
            // <pubDate>Wed, 21 Nov 2012 00:02:00 +0100</pubDate>
            final String MUSTER_THEMA_1 = "<title>";
            final String MUSTER_THEMA_2 = "</title>";
            final String MUSTER_URL_1 = "url=\"http://";
            final String MUSTER_DATE = "<pubDate>";
            int pos = 0, pos1;
            int pos2;
            String titel;
            String url = "";
            String datum, zeit = "";
            try {
                meldung(strUrlFeed);
                seite = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite, "Thema: " + thema);
                if ((pos1 = seite.indexOf(MUSTER_THEMA_1)) != -1) {
                    pos1 = pos1 + MUSTER_THEMA_1.length();
                }
                if ((pos2 = seite.indexOf(MUSTER_THEMA_2, pos1)) != -1) {
                    thema = seite.substring(pos1, pos2).trim();
//                        if (thema.contains(" ")) {
//                            thema = thema.substring(0, thema.indexOf(" "));
//                        }
                }
                while ((pos = seite.indexOf(MUSTER_THEMA_1, pos)) != -1) { //start der Einträge, erster Eintrag ist der Titel
                    pos += MUSTER_THEMA_1.length();
                    pos1 = pos;
                    if ((pos2 = seite.indexOf(MUSTER_THEMA_2, pos1)) != -1) {
                        titel = seite.substring(pos1, pos2).trim();
                        datum = "";
                        zeit = "";
                        if (titel.contains("vom")) {
                            datum = titel.substring(titel.indexOf("vom") + 3).trim();
                        } else {
                            int p1, p2;
                            if ((p1 = seite.indexOf(MUSTER_DATE, pos1)) != -1) {
                                p1 += MUSTER_DATE.length();
                                if ((p2 = seite.indexOf("<", p1)) != -1) {
                                    String tmp = seite.substring(p1, p2);
                                    datum = DatumZeit.convertDatum(tmp);
                                    zeit = DatumZeit.convertTime(tmp);
                                }
                            }
                        }
                        if ((pos1 = seite.indexOf(MUSTER_URL_1, pos1)) != -1) {
                            pos1 += MUSTER_URL_1.length();
                            if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                                url = seite.substring(pos1, pos2);
                                url = "http://" + url;
                            }
                            if (url.equals("")) {
                                Log.fehlerMeldungMReader(-463820049, "MediathekSfPod.addFilme", "keine URL: " + strUrlFeed);
                            } else {
//                            urlorg = urlorg.replace("%20", "\u0020;");
                                addFilm(new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit));
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-496352007, "MediathekSfPod.addFilme", ex.getMessage());
            }
        }
    }

    public static String[] convertDatum(String ddatum) {
        // <pubDate>Wed, 21 Nov 2012 00:02:00 +0100</pubDate>
        //<pubDate>Mon, 03 Jan 2011 17:06:16 +0100</pubDate>
        String datum = "", zeit = "";
        try {
            Date filmDate = new SimpleDateFormat("dd, MMM yyyy HH:mm:ss Z", Locale.US).parse(ddatum);
            datum = new SimpleDateFormat("dd.MM.yyyy").format(filmDate);
            zeit = new SimpleDateFormat("HH:mm:ss").format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(-979451236, "MediathekArdPodcast.convertDatum", ex);
        }
        return new String[]{datum, zeit};
    }
}
