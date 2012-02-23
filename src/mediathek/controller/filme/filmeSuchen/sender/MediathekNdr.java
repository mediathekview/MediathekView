/*
 *     MediathekView
 *     Copyright (C) 2008 W. Xaver
 *     W.Xaver[at]googlemail.com
 *     http://zdfmediathk.sourceforge.net/
 *
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     any later version.
 *
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *     GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.filme.filmeSuchen.sender;

import java.util.LinkedList;
import mediathek.Daten;
import mediathek.daten.DatenFilm;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.controller.io.GetUrl;
import mediathek.Log;

/**
 *
 *     @author
 */
public class MediathekNdr extends MediathekReader implements Runnable {

    public static final String SENDER = "NDR";
    final int MAX_SEITEN_LANG = 150;
    final int MAX_SEITEN_KURZ = 20;

    /**
     *
     * @param ddaten
     */
    public MediathekNdr(FilmeSuchen ssearch) {
        super(ssearch, /* name */ SENDER, /* text */ "NDR  (ca. 6 MB, 500 Filme)", /* threads */ 2, /* urlWarten */ 1000);
    }

    @Override
    void addToList() {
        // http://www.ndr.de/mediathek/videoliste100_glossaryPage-1.html
        // <a href="/fernsehen/sendungen/hallo_niedersachsen/media/hallonds159.html"  >
        // http://www.ndr.de/fernsehen/sendungen/hallo_niedersachsen/media/hallonds159.html
        final String ADRESSE = "http://www.ndr.de/mediathek/videoliste100_glossaryPage-1.html";
        final String ADRESSE_TEIL = "http://www.ndr.de/mediathek/videoliste100_glossaryPage-";
        final String MUSTER_ANZAHL = "Zeige Seite";
        int maxSeiten = MAX_SEITEN_KURZ;
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        int pos = 0;
        int pos1 = 0;
        int pos2 = 0;
        String url = "";
        String max = "";
        seite = getUrlIo.getUri_Utf(senderName, ADRESSE, seite, "");
        if (suchen.allesLaden) {
            // wenn alle Seiten ermitteln und gesamtzahl noch nicht bekannt
            if ((pos = seite.lastIndexOf(MUSTER_ANZAHL)) != -1) {
                pos += MUSTER_ANZAHL.length();
                pos1 = pos;
                if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                    try {
                        max = seite.substring(pos1, pos2);
                        max = max.trim();
                        maxSeiten = Integer.parseInt(max);
                        if (maxSeiten > MAX_SEITEN_LANG) {
                            maxSeiten = MAX_SEITEN_LANG;
                        }
                    } catch (Exception ex) {
                        maxSeiten = MAX_SEITEN_KURZ;
                    }
                }
            }
        }
        for (int i = 1; i < maxSeiten; ++i) {
            String[] add = new String[]{ADRESSE_TEIL + i + ".html", ""};
            listeThemen.add(add);
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                for (int t = 0; t < senderMaxThread; ++t) {
                    new Thread(new ThemaLaden()).start();
                }
            }
        }
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl1 = new GetUrl( senderWartenSeiteLaden);
        GetUrl getUrl2 = new GetUrl( senderWartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();
        private StringBuffer seite2 = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    try {
                  meldungProgress(link[0]);
                        finden(link[0] /* url */);
                    } catch (Exception ex) {
                        Log.fehlerMeldung("MediathekNdr.ThemaLaden.run.1", ex);
                    }
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekNdr.ThemaLaden.run.2", ex);
            }
        }

        private void finden(String urlSeite) {
            // http://www.ndr.de/mediathek/videoliste100_glossaryPage-1.html
            // erst: <div class="subline">28.11.2010 | 11:30 Uhr</div>
            // dann:
            // <a href="/fernsehen/sendungen/hallo_niedersachsen/media/hallonds159.html"  >
            // http://www.ndr.de/fernsehen/sendungen/hallo_niedersachsen/media/hallonds159.html
            LinkedList<String> hammerSchon = new LinkedList<String>();
            final String MUSTER_DATUM = "<div class=\"subline\">";
            final String MUSTER_URL1 = "<a href=\"/fernsehen/";
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String datum = "";
            String zeit = "";
            String url = "";
            String thema = "";
            //Podcasts auslesen
            seite1 = getUrl1.getUri_Utf(senderName, urlSeite, seite1, "");
            while ((pos = seite1.indexOf(MUSTER_DATUM, pos)) != -1) {
                datum = "";
                zeit = "";
                url = "";
                thema = "";
                pos += MUSTER_DATUM.length();
                try {
                    if ((pos1 = seite1.indexOf("|", pos)) != -1) {
                        datum = seite1.substring(pos, pos1).trim();
                    }
                    if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                        zeit = seite1.substring(pos1 + 1, pos2).trim();
                        if (zeit.contains("Uhr")) {
                            zeit = zeit.replace("Uhr", "").trim() + ":00";
                        }
                    }
                    pos1 = 0;
                    pos2 = 0;
                    if ((pos1 = seite1.indexOf(MUSTER_URL1, pos)) != -1) {
                        pos1 += MUSTER_URL1.length();
                        if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                            pos = pos2;
                            url = seite1.substring(pos1, pos2);
                        }
                        if (url.contains("/")) {
                            String posTh = url.substring(url.indexOf("sendungen/") + "sendungen/".length());
                            if (posTh.contains("/")) {
                                thema = posTh.substring(0, posTh.indexOf("/"));
                            }
                        }
                        if (!themaLaden(senderName, thema)) {
                            //nur Abos laden
                            continue;
                        }
                        if (url.equals("")) {
                            continue;
                        }
                        if (!hammerSchon.contains(url)) {
                            hammerSchon.add(url);
                            feedEinerSeiteSuchen("http://www.ndr.de/fernsehen/" + url, thema, datum, zeit);
                        }
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung("MediathekNdr.finden", ex);
                }
            }
        }

        void feedEinerSeiteSuchen(String urlFilm, String thema, String datum, String zeit) {
            // <title>Demo: Zehntausende im Wendland | NDR.de - Fernsehen - Sendungen A - Z - Hallo Niedersachsen - media</title>
            //<span class='footer_link'><a href="mms://ndr.wmod.llnwd.net/a3715/d1/msmedia/2010/1106/TV-20101106-2048-4701.wm.hq.wmv">Windows Media Stream (hohe Qualit&auml;t) im externen Player &ouml;ffnen</a></span>
            //<span class='footer_link'><a href="mms://ndr.wmod.llnwd.net/a3715/d1/msmedia/2010/1106/TV-20101106-2048-4701.wm.hi.wmv">Windows Media Stream (mittlere Qualit&auml;t) im externen Player &ouml;ffnen</a></span>
            //<span class='footer_link'><a href="mms://ndr.wmod.llnwd.net/a3715/d1/msmedia/2010/1106/TV-20101106-2048-4701.wm.lo.wmv">Windows Media Stream (niedrige Qualit&auml;t) im externen Player &ouml;ffnen</a></span>

            final String MUSTER_URL = "<span class='footer_link'><a href=\"";
            final String MUSTER_TITEL = "<title>";
            seite2 = getUrl2.getUri_Utf(senderName, urlFilm, seite2, "strUrlFilm: " + urlFilm);
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            String titel = "";
            //String tmp = seite2.toString();
            try {
                if ((pos = seite2.indexOf(MUSTER_TITEL, pos)) != -1) {
                    pos += MUSTER_TITEL.length();
                    pos1 = pos;
                    pos2 = seite2.indexOf("<", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        titel = seite2.substring(pos1, pos2);
                        if (titel.contains("|")) {
                            titel = titel.substring(0, titel.indexOf("|"));
                            titel = titel.trim();
                        }
                    }
                }
                pos = 0;
                if ((pos = seite2.indexOf(MUSTER_URL, pos)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    if ((pos2 = seite2.indexOf("\"", pos)) != -1) {
                        url = seite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel)
//                            daten.filmeLaden.listeFilmeSchattenliste.addSenderRtmp(new DatenFilm(daten, sender, thema, urlFilm, titel,
//                                    leitungAendern__(daten, url), url/*urlOrg*/, ""/*urlRtmp*/, ""/*urlHd*/));
                            if (thema.equals("")) {
                                thema = titel;
                            }
                            addFilm(new DatenFilm(senderName, thema, urlFilm, titel, url, datum, zeit));
                        } else {
                            Log.fehlerMeldung("MediathekNdr.feedEinerSeiteSuchen", "keine Url: " + urlFilm);
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekNdr.feedEinerSeiteSuchen", ex);
            }
        }
    }
}
