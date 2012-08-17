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
import mediathek.Konstanten;
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
    private final String ROOTURL = "http://tvthek.orf.at";
    private final String TOPICURL = "http://tvthek.orf.at/topics";

    /**
     *
     * @param ddaten
     */
    public MediathekOrf(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 3, /* urlWarten */ 1000, startPrio);
    }

    @Override
    void addToList() {
        listeThemen.clear();
        bearbeiteAdresse(TOPICURL);

        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/archiv");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/monday");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/tuesday");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/wednesday");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/thursday");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/friday");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/saturday");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/sunday");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/monday_prev");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/tuesday_prev");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/wednesday_prev");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/thursday_prev");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/friday_prev");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/saturday_prev");
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/sunday_prev");

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

    /**
     * @param adresse Starter-URL von dem aus Sendungen gefunden werden
     */
    private void bearbeiteAdresse(String adresse) {
        //System.out.println("bearbeiteAdresse: " + adresse);
        final String MUSTER_URL1 = "<a href=\""; //TH
        final String MUSTER_URL2 = "/programs/";
        final String MUSTER_URL2b = "/topics/"; //TH
        StringBuffer seite = getUrlIo.getUri(nameSenderMReader, adresse, Konstanten.KODIERUNG_UTF, 3, new StringBuffer(), "");
        int pos = 0;
        int pos1 = 0;
        int pos2 = 0;
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
                        //TH 31.7.2012
                        if (thema.endsWith(" ansehen...")) {
                            thema = thema.replace(" ansehen...", "");
                        }
                    }
                    if (url.equals("")) {
                        continue;
                    }
                    String[] add = new String[]{
                        ROOTURL + m + url, thema //TH
                    };
                    if (!istInListe(listeThemen, add[0], 0)) {
                        //System.out.println ("URL: " + add[0] + ", Thema: " + add[1]);
                        listeThemen.add(add);
                    }
                } else {
                    break; //TH muss sein da muster 2 manchmal nicht fündig - dann Endlosschleife
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-896234580, "MediathekOrf.addToList", ex);
            }
        }

        //TH 31.7.2012 Rekursive Sonderrunde für weitere topics ("alle Anzeigen")
        if (adresse.equals(TOPICURL)) {
            final String MUSTERURL_MORE = "<a class=\"more\" href=\"";
            pos = 0;
            while ((pos = seite.indexOf(MUSTERURL_MORE, pos)) != -1) {
                try {
                    pos += MUSTERURL_MORE.length();
                    pos1 = pos;
                    pos2 = seite.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = ROOTURL + seite.substring(pos1, pos2);
                        if (!url.equals(adresse)) {
                            bearbeiteAdresse(url);
                        }
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(-468320478, "MediathekOrf.addToList", ex);
                }
            }
        }
    }

    private class OrfThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();
        private StringBuffer seiteAsx = new StringBuffer();

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
            // System.out.println (this.toString() + "\nfeedEinerSeiteSuchen: " + strUrlFeed + ", Thema: " + thema);
            // <param name="URL" value="/programs/1306-Newton/episodes/1229327-Newton/1231597-Signation---Themenuebersicht.asx" />
            // <title> ORF TVthek: a.viso - 28.11.2010 09:05 Uhr</title>
            final String MUSTER = "<param name=\"URL\" value=\"";
            final String MUSTER_DATUM_1 = "<span>"; //TH
            final String MUSTER_DATUM_2 = "Uhr</span>"; //TH
            seite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite1, "Thema: " + thema);
            // System.out.println ("###"+seite1.substring(0, 500) + "###\n");           
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

                        //TH 1.8.2012 -Signation Problematik
                        if (!Daten.filmeLaden.getStop() && url.indexOf("-Signation") != -1) {
                            meldung(ROOTURL + url);
                            bearbeiteAsx(ROOTURL + url, thema, strUrlFeed, datum, zeit);
                        } else {
                            addFilm(new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, ROOTURL + url, datum, zeit));
                        }
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(-632105897, "MediathekOrf.feedEinerSeiteSuchen", ex);
                }
            }
        }

        void bearbeiteAsx(String asxUrl, String thema, String strUrlFeed, String datum, String zeit) {
            seiteAsx = getUrlIo.getUri_Iso(nameSenderMReader, asxUrl, seiteAsx, "");
            //System.out.println(this.toString() + "\nASX: " + asxUrl + ":\n" + seiteAsx + "\n");
            final String MUSTER_TITEL_OPEN = "<title>";
            final String MUSTER_TITEL_CLOSE = "</title>";
            final String MUSTER_REF = "<ref href=\"";
            int tpos = seiteAsx.indexOf(MUSTER_TITEL_CLOSE); // Ersten Titel (Haupttitel) überspringen
            if (tpos == -1) {
                tpos = 0;
            }
            while ((tpos = seiteAsx.indexOf(MUSTER_TITEL_OPEN, tpos)) != -1) {
                tpos += MUSTER_TITEL_OPEN.length();
                int endpos = seiteAsx.indexOf(MUSTER_TITEL_CLOSE, tpos);
                if (endpos == -1) {
                    break;
                }
                String titel = seiteAsx.substring(tpos, endpos);
                endpos += MUSTER_TITEL_CLOSE.length();

                int ref = seiteAsx.indexOf(MUSTER_REF, endpos);
                ref += MUSTER_REF.length();
                // ? oder wenigstens "
                int quote = seiteAsx.indexOf("\"", ref);
                if (quote == -1) {
                    break;
                }
                int quest = seiteAsx.indexOf("?", ref);
                if (quest != -1 && quest < quote) {
                    quote = quest;
                }
                String mms = seiteAsx.substring(ref, quote);
                // System.out.println("tit: " + titel);
                // System.out.println("mms: " + mms);
                addFilm(new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, mms, datum, zeit));

                tpos = quote;
            }
        }
    }
}
