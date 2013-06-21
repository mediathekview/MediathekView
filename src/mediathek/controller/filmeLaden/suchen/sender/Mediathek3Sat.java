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

import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.DatumZeit;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class Mediathek3Sat extends MediathekReader implements Runnable {

    public static final String SENDER = "3Sat";
    private final String MUSTER_ALLE = "http://www.3sat.de/mediathek/rss/mediathek.xml";

    public Mediathek3Sat(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://www.3sat.de/page/?source=/specials/133576/index.html";
        final String MUSTER_URL = "<a href=\"/mediaplayer/rss/mediathek";
        listeThemen.clear();
        StringBuffer seite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        meldungStart();
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
                Log.fehlerMeldung(-498653287, Log.FEHLER_ART_MREADER, "Mediathek3sat.addToList", ex);
            }
        }
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            listeSort(listeThemen, 1);
            // noch den RSS für alles anfügen
            // Liste von http://www.3sat.de/mediathek/rss/mediathek.xml holen
            String[] add = new String[]{MUSTER_ALLE, ""};
            listeThemen.add(0, add); // alle nachfolgenden Filme ersetzen Filme die bereits in der Liste sind
            meldungAddMax(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new ThemaLaden()).start();
            }

        }
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer seite2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

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
                Log.fehlerMeldung(-987452384, Log.FEHLER_ART_MREADER, "Mediathek3Sat.ThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }

        void laden(String url_rss, String thema_rss) {
            // <title>3sat.schweizweit: Mediathek-Beiträge</title>
            final String MUSTER_URL = "type=\"video/x-ms-asf\" url=\"";
            final String MUSTER_URL_QUICKTIME_1 = "type=\"video/quicktime\" url=\"http://hstreaming.zdf.de/3sat/veryhigh";
            final String MUSTER_URL_QUICKTIME_2 = "type=\"video/quicktime\" url=\"http://hstreaming.zdf.de/3sat/300";
            final String MUSTER_TITEL = "<title>";
            final String MUSTER_DESCRIPTION = "<media:description>";
            final String MUSTER_IMAGE = "<media:thumbnail url=\"";
            final String MUSTER_DURATION = "media:content duration=\"";
            final String MUSTER_DATUM = "<pubDate>";
            final String MUSTER_LINK = "<link>";
            boolean urlAlle = url_rss.equals(MUSTER_ALLE);
            seite1 = getUrlIo.getUri_Utf(nameSenderMReader, url_rss, seite1, "");
            int pos = 0;
            int pos1;
            int pos2;
            int ende = 0;
            String url;
            String thema = "3sat";
            String link;
            String datum;
            String zeit;
            long duration;
            String description;
            String imageUrl;
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
            while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_TITEL, pos)) != -1) {
                pos += MUSTER_TITEL.length();
                ende = seite1.indexOf(MUSTER_TITEL, pos); // beginnt der nächste Film
                url = "";
                link = "";
                datum = "";
                zeit = "";
                titel = "";
                duration = 0;
                description = "";
                imageUrl = "";
                try {
                    pos1 = pos;
                    if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                        titel = seite1.substring(pos1, pos2);
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
                    if ((pos1 = seite1.indexOf(MUSTER_DESCRIPTION, pos)) != -1) {
                        pos1 += MUSTER_DESCRIPTION.length();
                        if ((pos2 = seite1.indexOf("</media:description>", pos1)) != -1) {
                            description = seite1.substring(pos1, pos2);
                        }
                    }

                    if ((pos1 = seite1.indexOf(MUSTER_DURATION, pos)) != -1) {
                        pos1 += MUSTER_DURATION.length();
                        if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                            try {
                                duration = Long.parseLong(seite1.substring(pos1, pos2));
                            } catch (Exception ex) {
                                Log.fehlerMeldung(-363524108, Log.FEHLER_ART_MREADER, "Mediathek3Sat.addToList", "duration");
                            }
                        }
                    }

                    if ((pos1 = seite1.indexOf(MUSTER_IMAGE, pos)) != -1) {
                        pos1 += MUSTER_IMAGE.length();
                        if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                            imageUrl = seite1.substring(pos1, pos2);
                        }
                    }

                    if ((pos1 = seite1.indexOf(MUSTER_DATUM, pos)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            //<pubDate>Mon, 03 Jan 2011 17:06:16 +0100</pubDate>
                            tmp = seite1.substring(pos1, pos2);
                            if (tmp.equals("")) {
                                Log.fehlerMeldung(-987453983, Log.FEHLER_ART_MREADER, "Mediathek3Sat.addToList", "keine Datum");
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
                    pos1 = seite1.indexOf(MUSTER_URL, pos);
                    if (pos1 == -1 || (ende != -1 && pos1 > ende)) {
                        // dann nach Quicktime-Link suchen
                        if ((pos1 = seite1.indexOf(MUSTER_URL_QUICKTIME_1, pos)) != -1) {
                            pos1 += MUSTER_URL_QUICKTIME_1.length();
                            if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                                url = seite1.substring(pos1, pos2);
                            }
//                            } else {
//                                // 2. Möglichkeit von Quicktime
//                                if ((pos1 = seite1.indexOf(MUSTER_URL_QUICKTIME_2, pos)) != -1) {
//                                    pos1 += MUSTER_URL_QUICKTIME_1.length();
//                                    if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
//                                        url = seite1.substring(pos1, pos2);
//                                    }
//                                }
                        }
                        if (url.equals("")) {
                            Log.fehlerMeldung(-398745632, Log.FEHLER_ART_MREADER, "Mediathek3Sat.addToList-quicktime", "keine URL" + url_rss);
                        } else {
                            url = "http://hstreaming.zdf.de/3sat/veryhigh" + url;
                            if (!url.endsWith("mov")) {
                                Log.fehlerMeldung(-798965430, Log.FEHLER_ART_MREADER, "Mediathek3sat.filmHolen", "keine URL: " + url_rss);
                            } else {
                                quicktimeHolen(thema, titel, link, url, datum, zeit, duration, description, imageUrl);
                            }
                        }
                    } else if (pos1 != -1 && ((ende != -1 && pos1 < ende) || ende == -1)) {
                        // asx
                        pos1 += MUSTER_URL.length();
                        if ((pos2 = seite1.indexOf("\"", pos1)) != -1) {
                            url = seite1.substring(pos1, pos2);
                        }
                        if (url.equals("")) {
                            Log.fehlerMeldung(-532169764, Log.FEHLER_ART_MREADER, "Mediathek3Sat.addToList-asx", "keine URL");
                        } else {
                            url = url.replace("/300/", "/veryhigh/");
                            //    public DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum) {
                            //    public DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
                            //addFilm(new DatenFilm(nameSenderMReader, (thema_rss.equals("") ? thema : thema_rss), link, titel, url, datum, zeit));
                            if (!url.endsWith("asx")) {
                                Log.fehlerMeldung(-896325047, Log.FEHLER_ART_MREADER, "Mediathek3sat.filmHolen-2", "keine URL: " + url_rss);
                            } else {
                                //flashHolen(thema, titel, link, url, datum, zeit);
                                flashHolen(thema, titel, link, url, datum, zeit, duration, description, imageUrl);
                            }
                        }
                    } else {
                        Log.fehlerMeldung(-978741398, Log.FEHLER_ART_MREADER, "Mediathek3sat.filmHolen", "kann nicht sein, URL: " + url_rss);
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(-823694892, Log.FEHLER_ART_MREADER, "Mediathek3Sat.laden", ex);
                }
            } //while, die ganz große Schleife
        }

        //private void flashHolen(String thema, String titel, String urlThema, String urlFilm, String datum, String zeit) {
        private void flashHolen(String thema, String titel, String urlThema, String urlFilm, String datum, String zeit, long durationInSeconds, String description, String imageUrl) {
            meldung(urlFilm);
            //DatenFilm f = MediathekZdf.flash(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit);
            DatenFilm f = MediathekZdf.flash(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit, durationInSeconds, description, "", imageUrl, new String[]{});
            if (f != null) {
                addFilm(f);
            }
        }

        private void quicktimeHolen(String thema, String titel, String urlThema, String urlFilm, String datum, String zeit, long durationInSeconds, String description, String imageUrl) {
            meldung(urlFilm);
            //DatenFilm f = MediathekZdf.flash(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit);
            DatenFilm f = quicktime(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit, durationInSeconds, description, "", imageUrl, new String[]{});
            if (f != null) {
                addFilm(f);
            }
        }

        private DatenFilm quicktime(GetUrl getUrl, StringBuffer seiteQuicktime, String senderName, String thema, String titel, String filmWebsite, String urlFilm, String datum, String zeit, long durationInSeconds, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
            // RTSPtext
            // rtsp://a1966.v1252936.c125293.g.vq.akamaistream.net/7/1966/125293/v0001/mp4.od.origin.zdf.de.gl-systemhaus.de/none/3sat/13/06/130614_meyer_kuz_1596k_p13v9.mp4
            // 
            // daraus wird:
            // http://rodl.zdf.de / none/3sat/13/06/130621_news_kuz_436k_p9v11.mp4
            DatenFilm ret = null;
            final String MUSTER_URL = "rtsp://";
            final String MUSTER_TAUSCH = "gl-systemhaus.de";
            final String MUSTER_ENDE = ".mp4";
            String orgUrl = urlFilm;
            String url, tmpUrl = "";
            int pos1;
            int pos2;
            try {
                seiteQuicktime = getUrl.getUri_Utf(senderName, orgUrl, seiteQuicktime, "filmWebsite: " + filmWebsite);
                String strSeiteQuicktime = seiteQuicktime.toString();
                if ((pos1 = strSeiteQuicktime.indexOf(MUSTER_URL, 0)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = strSeiteQuicktime.indexOf(MUSTER_ENDE, pos1)) != -1) {
                        tmpUrl = strSeiteQuicktime.substring(pos1, pos2);
                    }
                }
                if (tmpUrl.equals("")) {
                    Log.fehlerMeldung(-679893014, Log.FEHLER_ART_MREADER, "Mediathek3sat.quicktime", "!gefunden: " + urlFilm);
                } else {
                    tmpUrl = MUSTER_URL + tmpUrl + MUSTER_ENDE;
                    if ((pos1 = tmpUrl.indexOf(MUSTER_TAUSCH)) != -1) {
                        pos1 += MUSTER_TAUSCH.length();
                        url = "http://rodl.zdf.de" + tmpUrl.substring(pos1);
                        //ret = new DatenFilm(senderName, thema, urlThema, titel, url, ""/* urlRtmp */, datum, zeit);
                        ret = new DatenFilm(senderName, thema, filmWebsite, titel, url, ""/* urlRtmp */, datum, zeit, durationInSeconds, description, thumbnailUrl, imageUrl, keywords);
                    } else {
                        Log.fehlerMeldung(-918596307, Log.FEHLER_ART_MREADER, "Mediathek3sat.quicktime", "url passt nicht: " + urlFilm);
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-265847128, Log.FEHLER_ART_MREADER, "MediathekZdf.flash" + senderName, ex, urlFilm);
            }
            return ret;
        }

        private synchronized String[] getListeThemen() {
            return listeThemen.pollFirst();
        }
    }
}
