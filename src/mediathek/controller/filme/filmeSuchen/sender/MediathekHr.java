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
import mediathek.controller.filme.filmeSuchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;

/**
 *
 * @author
 */
public class MediathekHr extends MediathekReader implements Runnable {

    public static final String SENDER = "HR";
    private StringBuffer seite = new StringBuffer();

    /**
     *
     * @param ddaten
     */
    public MediathekHr(FilmeSuchenSender ssearch) {
        super(ssearch, /* name */ SENDER,  /* threads */ 2, /* urlWarten */ 1000);
    }

    /**
     *
     */
    @Override
    public void addToList() {
        final String MUSTER = "sendEvent('load','";
        listeThemen.clear();
        seite = getUrlIo.getUri_Utf(nameSenderMReader, "http://www.hr-online.de/website/fernsehen/sendungen/index.jsp", seite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url;
        while ((pos = seite.indexOf(MUSTER, pos)) != -1) {
            pos += MUSTER.length();
            pos1 = pos;
            pos2 = seite.indexOf("'", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
                if (!url.equals("")) {
                    String[] add = new String[]{url, ""};
                    if (!istInListe(listeThemen, url, 0)) {
                        listeThemen.add(add);
                    }
                } else {
                    Log.fehlerMeldung(-456933258, "MediathekHr.addToList", "keine URL");
                }
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                for (int t = 0; t < senderMaxThread; ++t) {
                    new Thread(new HrThemaLaden()).start();
                }
            }
        }
    }

    private class HrThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(senderWartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();
        //private StringBuffer seite2 = new StringBuffer();

        @Override
        public void run() {
            try {
                meldungAddThread();
                String link[];
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    meldungProgress(link[0] /* url */);
                    seite.setLength(0);
                    addFilme(link[1], link[0] /* url */);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung(-894330854, "MediathekHr.HrThemaLaden.run", ex);
            }
        }

        private void addFilme(String thema, String strUrlFeed) {
            final String MUSTER_TITEL = "<title>"; //<title>alle wetter! vom 03.01.2011</title>
            final String MUSTER_URL_1 = "<jwplayer:streamer>";//<jwplayer:streamer>rtmp://gffstream.fcod.llnwd.net/a792/e4</jwplayer:streamer>
            final String MUSTER_URL_2 = "\" url=\"";//<media:content duration="00:29:42" type="video/mp4" url="mp4:flash/fs/hessenschau/20110103_1930" />
            final String MUSTER_ITEM_1 = "<item>";
            final String MUSTER_DATUM = "<pubDate>"; //<pubDate>03.01.2011</pubDate>
            meldung("*" + strUrlFeed);
            seite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite1, "");
            try {
                int posItem1 = 0;
                int pos1;
                int pos2;
                String url;
                String url1;
                String url2;
                String datum = "";
                String titel = "";
                while (!Daten.filmeLaden.getStop() && (posItem1 = seite1.indexOf(MUSTER_ITEM_1, posItem1)) != -1) {
                    posItem1 += MUSTER_ITEM_1.length();
                    //posItem2 = seite1.indexOf(MUSTER_ITEM_2, posItem1);
                    if ((pos1 = seite1.indexOf(MUSTER_DATUM, posItem1)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            datum = seite1.substring(pos1, pos2);
                        }
                    }
                    if ((pos1 = seite1.indexOf(MUSTER_TITEL, posItem1)) != -1) {
                        pos1 += MUSTER_TITEL.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            titel = seite1.substring(pos1, pos2);
                            thema = titel;
                        }
                    }
                    if ((pos1 = seite1.indexOf(MUSTER_URL_1, posItem1)) != -1) {
                        pos1 += MUSTER_URL_1.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            url1 = seite1.substring(pos1, pos2);
                            if (!url1.equals("")) {
                                if ((pos1 = seite1.indexOf(MUSTER_URL_2, pos2)) != -1) {
                                    pos1 += MUSTER_URL_2.length();
                                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                                        url2 = seite1.substring(pos1, pos2);
                                        url = addsUrl(url1, url2);
                                        String furl = "-r " + url + " -y " + url2;
                                        // DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String uurlRtmp, String datum, String zeit) {
                                        DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, url, furl, datum, "");
                                        addFilm(film);
                                    } else {
                                        Log.fehlerMeldung(-649882036, "MediathekHr.addFilme", "keine URL");
                                    }
                                }
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-487774126, "MediathekHr.addFilme", ex);
            }
        }
    }
}
