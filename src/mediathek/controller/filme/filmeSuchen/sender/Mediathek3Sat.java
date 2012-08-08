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
import mediathek.controller.filme.filmeSuchenSender.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;
import mediathek.tool.DatumZeit;

public class Mediathek3Sat extends MediathekReader implements Runnable {

    public static final String SENDER = "3Sat";
    private final String MUSTER_ALLE = "http://www.3sat.de/mediathek/rss/mediathek.xml";

    public Mediathek3Sat(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 3, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://www.3sat.de/page/?source=/specials/133576/index.html";
        final String MUSTER_URL = "<a href=\"/mediaplayer/rss/mediathek";
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        //seite = new GetUrl(daten).getUriArd(ADRESSE, seite, "");
        seite = getUrlIo.getUri_Iso(nameSenderMReader, ADRESSE, seite, "");
        int pos1 = 0;
        int pos2;
        String url = "";
        while ((pos1 = seite.indexOf(MUSTER_URL, pos1)) != -1) {
            try {
                pos1 += MUSTER_URL.length();
                if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                    url = seite.substring(pos1, pos2);
                }
                if (url.equals("")) {
                    continue;
                }
                // in die Liste eintragen
                String[] add = new String[]{"http://www.3sat.de/mediaplayer/rss/mediathek" + url, ""};
                listeThemen.addUrl(add);
            } catch (Exception ex) {
                Log.fehlerMeldung(-498653287, "Mediathek3sat.addToList", ex);
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            listeSort(listeThemen, 1);
            // noch den RSS für alles anfügen
            // Liste von http://www.3sat.de/mediathek/rss/mediathek.xml holen
            String[] add = new String[]{MUSTER_ALLE, ""};
            listeThemen.add(0, add); // alle nachfolgenden Filme ersetzen Filme die bereits in der Liste sind
            meldungStart(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new ThemaLaden()).start();
            }

        }
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();
        private StringBuffer seite2 = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    laden(link[0] /* url */, link[1] /* Thema */);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-987452384, "Mediathek3Sat.ThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }

        void laden(String url_rss, String thema_rss) {
            // <title>3sat.schweizweit: Mediathek-Beiträge</title>
            final String MUSTER_URL = "type=\"video/x-ms-asf\" url=\"";
            final String MUSTER_TITEL = "<title>";
            final String MUSTER_DATUM = "<pubDate>";
            final String MUSTER_LINK = "<link>";
            boolean urlAlle = url_rss.equals(MUSTER_ALLE);
            seite1 = getUrlIo.getUri_Utf(nameSenderMReader, url_rss, seite1, "");
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            String thema = "3sat";
            String link;
            String datum;
            String zeit;
            String titel;
            String tmp;
            if ((pos = seite1.indexOf(MUSTER_TITEL, pos)) == -1) {
                return;
            } else {//für den HTML-Titel
                pos += MUSTER_TITEL.length();
                pos1 = pos;
                if (!urlAlle) {
                    if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                        thema = seite1.substring(pos1, pos2);
                        thema = thema.replace("3sat.", "");
                        if (thema.contains(":")) {
                            thema = thema.substring(0, thema.indexOf(":"));
                        }
                    }
                }

            }
            while ((pos = seite1.indexOf(MUSTER_TITEL, pos)) != -1) {
                pos += MUSTER_TITEL.length();
                url = "";
                link = "";
                datum = "";
                zeit = "";
                titel = "";
                try {
                    pos1 = pos;
                    if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                        titel = seite1.substring(pos1, pos2);
//                        if (titel.contains("Cern")) {
//                            System.out.print("");
//                        } else {
//                            continue;
//                        }
                        if (titel.contains(":") && (titel.indexOf(":") + 1) < titel.length()) {
                            //enthält : und ist nicht das letztes zeichen
                            if (urlAlle) {
                                thema = titel.substring(0, titel.indexOf(":"));
                            }
                            titel = titel.substring(titel.indexOf(":") + 1);
                        }
                        titel = titel.trim();
                        if (!titel.equals("")) {
                            while (titel.charAt(0) == '\u00A0') {
                                titel = titel.substring(1);
                                if (titel.equals("")) {
                                    break;
                                }
                            }
                        }
                    }
                    if ((pos1 = seite1.indexOf(MUSTER_DATUM, pos)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            //<pubDate>Mon, 03 Jan 2011 17:06:16 +0100</pubDate>
                            tmp = seite1.substring(pos1, pos2);
                            if (tmp.equals("")) {
                                Log.fehlerMeldungMReader(-987453983, "Mediathek3Sat.addToList", "keine Datum");
                            } else {
                                datum = DatumZeit.convertDatum(tmp);
                                zeit = DatumZeit.convertTime(tmp);
                            }
                        }
                    }
                    if ((pos1 = seite1.indexOf(MUSTER_LINK, pos)) != -1) {
                        pos1 += MUSTER_LINK.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            //<link>http://www.3sat.de/mediathek/?obj=20937</link>
                            link = seite1.substring(pos1, pos2);
                        }
                    }
                    if ((pos1 = seite1.indexOf(MUSTER_URL, pos)) != -1) {
                        pos1 += MUSTER_URL.length();
                        if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                            url = seite1.substring(pos1, pos2);
                        }
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldungMReader(-532169764, "Mediathek3Sat.addToList", "keine URL");
                    } else {
                        url = url.replace("/300/", "/veryhigh/");
                        //    public DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum) {
                        //    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
                        //addFilm(new DatenFilm(nameSenderMReader, (thema_rss.equals("") ? thema : thema_rss), link, titel, url, datum, zeit));
                        if (!url.endsWith("asx")) {
                            Log.fehlerMeldungMReader(-896325047, "Mediathek3sat.filmHolen-2", "keine URL: " + url);
                        } else {
                            flashHolen(thema, titel, link, url, datum, zeit);
                        }
                    }



                } catch (Exception ex) {
                    Log.fehlerMeldung(-823694892, "Mediathek3Sat.laden", ex);
                }
            } //while, die ganz große Schleife
        }

        private void flashHolen(String thema, String titel, String urlThema, String urlFilm, String datum, String zeit) {
            //<param name="app" value="ondemand" />
            //<param name="host" value="cp125301.edgefcs.net" />
            //<param name="protocols" value="rtmp,rtmpt" />
            //<video dur="00:29:33" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/07/120724_mann_bin_ich_schoen_37g_l.mp4" system-bitrate="62000">
            //<param name="quality" value="low" />
            //</video>
            //
            //<video dur="00:29:33" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/07/120724_mann_bin_ich_schoen_37g_h.mp4" system-bitrate="700000">
            //<param name="quality" value="high" />
            //</video>
            //
            //<video dur="00:29:33" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/07/120724_mann_bin_ich_schoen_37g_vh.mp4" system-bitrate="1700000">
            //<param name="quality" value="veryhigh" />
            //</video>

            //http://wstreaming.zdf.de/3sat/veryhigh/ ... _hitec.asx
            //http://fstreaming.zdf.de/3sat/veryhigh/ ... hitec.smil
            //rtmpt://cp125301.edgefcs.net/ondemand/mp4:zdf/12/07/120724_mann_bin_ich_schoen_37g_vh.mp4

            final String MUSTER_HOST = "<param name=\"host\" value=\"";
            final String MUSTER_PROT = "<param name=\"protocols\" value=\"";
            final String MUSTER_APP = "<param name=\"app\" value=\"";
            final String MUSTER_URL = "src=\"";
            final String MUSTER_URL_L = "l.mp4";
            final String MUSTER_URL_H = "h.mp4";
            final String MUSTER_URL_VH = "vh.mp4";
            String orgUrl = urlFilm;
            String host = "";
            String app = "";
            String prot;
            String url = "", tmpUrl = "";
            int pos1;
            int pos2;
            try {
                meldung(orgUrl);
                orgUrl = orgUrl.replace("http://wstreaming.zdf.de", "http://fstreaming.zdf.de");
                orgUrl = orgUrl.replace("http://wgeostreaming.zdf.de", "http://fgeostreaming.zdf.de");
                orgUrl = orgUrl.replace(".asx", ".smil");
                seite2 = getUrl.getUri_Utf(nameSenderMReader, orgUrl, seite2, "urlThema: " + urlThema);
                if ((pos1 = seite2.indexOf(MUSTER_HOST, 0)) != -1) {
                    pos1 += MUSTER_HOST.length();
                    if ((pos2 = seite2.indexOf("\"", pos1)) != -1) {
                        host = seite2.substring(pos1, pos2);
                    }
                }
                if ((pos1 = seite2.indexOf(MUSTER_APP, 0)) != -1) {
                    pos1 += MUSTER_APP.length();
                    if ((pos2 = seite2.indexOf("\"", pos1)) != -1) {
                        app = seite2.substring(pos1, pos2);
                    }
                }
                pos1 = 0;
                while ((pos1 = seite2.indexOf(MUSTER_URL, pos1)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = seite2.indexOf("\"", pos1)) != -1) {
                        tmpUrl = seite2.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        url = tmpUrl;
                    }
                    if (!url.contains(MUSTER_URL_VH) && tmpUrl.contains(MUSTER_URL_H)) {
                        url = tmpUrl;
                    }
                    if (tmpUrl.contains(MUSTER_URL_VH)) {
                        url = tmpUrl;
                    }
                }
                if (url.equals("")) {
                    // dann die alte URL eintragen
                    addFilm(new DatenFilm(nameSenderMReader, thema, urlThema, titel, urlFilm, urlFilm/* urlOrg */, ""/* urlRtmp */, datum, zeit));
                    Log.fehlerMeldungMReader(-563204752, "Mediathek3sat.flashHolen-1", "keine URL: " + urlFilm);
                } else if (host.equals("")) {
                    // dann die alte URL eintragen
                    addFilm(new DatenFilm(nameSenderMReader, thema, urlThema, titel, urlFilm, urlFilm/* urlOrg */, ""/* urlRtmp */, datum, zeit));
                    Log.fehlerMeldungMReader(-698532098, "Mediathek3sat.flashHolen-2", "kein Host: " + urlFilm);
                } else {
                    url = "rtmpt://" + host + "/" + app + "/" + url;
                    addFilm(new DatenFilm(nameSenderMReader, thema, urlThema, titel, url, url/* urlOrg */, ""/* urlRtmp */, datum, zeit));
                }

            } catch (Exception ex) {
                Log.fehlerMeldung(-45410240, "Mediathek3sat.filmHolen", ex, urlFilm);
            }
        }

        private synchronized String[] getListeThemen() {
            return listeThemen.pollFirst();
        }
    }
}
