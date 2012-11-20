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
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

/**
 *
 * @author
 */
public class MediathekArd extends MediathekReader implements Runnable {

    public static final String SENDER = "ARD";

    /**
     *
     * @param ddaten
     */
    public MediathekArd(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 6, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://www.ardmediathek.de/ard/servlet/ajax-cache/3551682/view=module/index.html";
        final String MUSTER_URL = "?documentId=";
        final String MUSTER_THEMA = "{ \"titel\": \"";
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        meldungStart();
        seite = getUrlIo.getUri(nameSenderMReader, ADRESSE, Konstanten.KODIERUNG_UTF, 5 /* versuche */, seite, "" /* Meldung */);
        if (seite.length() == 0) {
            Log.systemMeldung("ARD: Versuch 2");
            warten();
            seite = getUrlIo.getUri(nameSenderMReader, ADRESSE, Konstanten.KODIERUNG_UTF, 5 /* versuche */, seite, "" /* Meldung */);
            if (seite.length() == 0) {
                Log.fehlerMeldungMReader(-104689736, "MediathekArd.addToList", "wieder nichts gefunden");
            }
        }
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        String thema = "";
        while ((pos = seite.indexOf(MUSTER_THEMA, pos)) != -1) {
            try {
                pos += MUSTER_THEMA.length();
                pos1 = pos;
                pos2 = seite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1) {
                    thema = seite.substring(pos1, pos2);
                }
                pos2 = seite.indexOf("}", pos);
                if (pos1 != -1 && pos2 != -1) {
                    String tmp = seite.substring(pos1, pos2);
                    if (tmp.contains("/podcast/")) {
                        // ist dann auch im ARD.Podcast
                        continue;
                    }
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
                String[] add = new String[]{"http://www.ardmediathek.de/ard/servlet/ajax-cache/3516962/view=list/documentId=" + url + "/index.html", thema};
                listeThemen.addUrl(add);
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-698732167, "MediathekArd.addToList", new String[]{ex.getMessage(), "kein Thema"});
            }
        }
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            listeSort(listeThemen, 1);
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new ArdThemaLaden()).start();
            }
        }
    }

    private synchronized void warten() {
        try {
            // war wohl nix, warten und dann nochmal
            // timeout: the maximum time to wait in milliseconds.
            long warten = 60 * 1000;
            this.wait(warten);
        } catch (InterruptedException ex) {
            Log.fehlerMeldungMReader(-369502367, "MediathekArd.warten", new String[]{ex.getMessage(), "2. Versuch"});
        }
    }

    private class ArdThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);

        public ArdThemaLaden() {
        }
        private StringBuffer seite1 = new StringBuffer();
        private StringBuffer seiteFehler = new StringBuffer();
        private StringBuffer seiteWeiter = new StringBuffer();
        private StringBuffer seite2 = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    feedSuchen(link[0] /* url */, link[1] /* Thema */);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-487326921, "MediathekArdThemaLaden.run", ex.getMessage());
            }

        }

        private void feedSuchen(String strUrlFeed, String thema) {
            //weitere Seiten:
            //wenn vorhanden: <option value="
            //<option value="/ard/servlet/ajax-cache/3516962/view=list/documentId=1175574/goto=2/index.html">2</option>
            //URL: http://www.ardmediathek.de/ard/servlet/ajax-cache/3516962/view=list/documentId=4106/index.html
            final String MUSTER = "<option value=\"";
            seite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite1, "Thema: " + thema);
            int pos;
            int pos1;
            int pos2;
            String url;
            //erst mal die erste Seite holen
            if (!feedEinerSeiteSuchen(seite1, strUrlFeed, thema, false)) {
                // http://www.ardmediathek.de/ard/servlet/ajax-cache/3516962/view=list/documentId=3322404/index.html
                // http://www.ardmediathek.de/ard/servlet/ajax-cache/3516992/view=switch/documentId=3322404/index.html
                String tmp = strUrlFeed.replace("ajax-cache/3516962/view=list", "ajax-cache/3516992/view=switch");
                seiteFehler = getUrl.getUri_Utf(nameSenderMReader, tmp, seiteFehler, "Thema: " + thema);
                feedEinerSeiteSuchen(seiteFehler, tmp, thema, false);
            }
            //nach weitern Seiten schauen
            if (suchen.allesLaden) {
                pos = 0;
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER, pos)) != -1) {
                    try {
                        pos += MUSTER.length();
                        pos1 = pos;
                        pos2 = seite1.indexOf("\"", pos);
                        if (pos1 != -1 && pos2 != -1) {
                            url = seite1.substring(pos1, pos2);
                        } else {
                            break;
                        }
                        if (url.equals("")) {
                            continue;
                        }
                        seiteWeiter = getUrl.getUri_Utf(nameSenderMReader, "http://www.ardmediathek.de" + url, seiteWeiter, "Thema: " + thema);
                        feedEinerSeiteSuchen(seiteWeiter, strUrlFeed, thema, true);
                    } catch (Exception ex) {
                        Log.fehlerMeldungMReader(-497321681, "MediathekArd.feedSuchen", new String[]{ex.getMessage(), "Weitere Seiten suchen"});
                    }
                }
            }
        }

        private boolean feedEinerSeiteSuchen(StringBuffer seite, String strUrlFeed, String thema, boolean alt) {
            //url: http://www.ardmediathek.de/ard/servlet/ajax-cache/3516962/view=list/documentId=443668/index.html
            //Feed eines Themas laden
            //<h3 class="mt-title"><a href="/ard/servlet/content/3517136?documentId=3743644"

            //<a href="/ard/servlet/ajax-cache/3516938/view=switch/documentId=3398614/index.html" class="mt-box_preload mt-box-overflow"></a>

            //weitere Seiten:
            //wenn vorhanden: <option value="

            //<option value="/ard/servlet/ajax-cache/3516962/view=list/documentId=1175574/goto=2/index.html">2</option>
            //<span class="mt-airtime">22.11.10
            //<span class="mt-icon mt-icon-toggle_arrows"></span>              Sendung vom 22.11.10 | 23:00            </a>
            boolean ret = false;
            boolean gefunden = false;
            final String TITEL = "\">";
            //final String MUSTER = "<h3 class=\"mt-title\"><a href=\"";
            final String MUSTER = "<a href=\"";
            final String MUSTER_SET = "http://www.ardmediathek.de";
            final String MUSTER_DATUM_1 = "<span class=\"mt-icon mt-icon-toggle_arrows\"></span>";
            final String MUSTER_DATUM_2 = "</a>";
            int pos;
            int posDatum1 = 0;
            int posDatum2;
            int pos1;
            int pos2;
            String url = "";
            String titel = "";
            String datum = "";
            String zeit = "";
            String tmp;
            //erst mal nach weitern Seiten schauen
            while (!Daten.filmeLaden.getStop() && (posDatum1 = seite.indexOf(MUSTER_DATUM_1, posDatum1)) != -1) {
                posDatum1 += MUSTER_DATUM_1.length();
                if ((pos1 = seite.indexOf(MUSTER_DATUM_2, posDatum1)) != -1) {
                    tmp = seite.substring(posDatum1, pos1).trim();
                    if (tmp.contains("Sendung vom")) {
                        tmp = tmp.replace("Sendung vom", "");
                    }
                    if (tmp.contains("|")) {
                        datum = tmp.substring(0, tmp.indexOf("|")).trim();
                        if (datum.length() == 8) {
                            datum = datum.substring(0, 6) + "20" + datum.substring(6);
                        }
                        zeit = tmp.substring(tmp.indexOf("|") + 1) + ":00";
                    }
                }
                pos = posDatum1;
                posDatum2 = seite.indexOf(MUSTER_DATUM_1, posDatum1);
                while (!Daten.filmeLaden.getStop() && (pos = seite.indexOf(MUSTER, pos)) != -1) {
                    if (posDatum2 != -1) {
                        // nur im Bereich des Datums suchen
                        if (pos > posDatum2) {
                            break;
                        }
                    }
                    try {
                        pos += MUSTER.length();
                        pos1 = pos;
                        pos2 = seite.indexOf("\"", pos);
                        if (pos1 != -1 && pos2 != -1) {
                            url = seite.substring(pos1, pos2);
                        }
                        if (url.equals("")) {
                            continue;
                        }
                        if ((pos = seite.indexOf(TITEL, pos)) != -1) {
                            pos1 = pos += TITEL.length();
                            pos2 = seite.indexOf("<", pos);
                            if (pos1 != -1 && pos2 != -1) {
                                titel = seite.substring(pos1, pos2);
                            }
                        }
                        gefunden = true;
                        ret = filmLaden(strUrlFeed, MUSTER_SET + url, thema, titel, datum, zeit, alt);
                    } catch (Exception ex) {
                        Log.fehlerMeldungMReader(-321648296, "MediathekArd.feedEinerSeiteSuchen-1", new String[]{ex.getMessage(), "Thema hat keine Links"});
                    }
                }
            }
            if (!gefunden) {
                //dann nochmal ohne Datum
                //07.10.10
                final String DAT = "<span class=\"mt-airtime\">";
                pos = 0;
                while (!Daten.filmeLaden.getStop() && (pos = seite.indexOf(MUSTER, pos)) != -1) {
                    try {
                        pos += MUSTER.length();
                        pos1 = pos;
                        pos2 = seite.indexOf("\"", pos);
                        if (pos1 != -1 && pos2 != -1) {
                            url = seite.substring(pos1, pos2);
                        }
                        if (url.equals("")) {
                            continue;
                        }
                        if ((pos = seite.indexOf(TITEL, pos)) != -1) {
                            pos1 = pos += TITEL.length();
                            pos2 = seite.indexOf("<", pos);
                            if (pos1 != -1 && pos2 != -1) {
                                titel = seite.substring(pos1, pos2);
                            }
                        }
                        // noch das Datum versuchen
                        if ((pos1 = seite.indexOf(DAT, pos)) != -1) {
                            pos1 += DAT.length();
                            if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                                datum = seite.substring(pos1, pos2);
                                if (datum.length() > 10) {
                                    datum = datum.substring(0, 9);
                                    datum = datum.substring(0, 6) + "20" + datum.substring(6);
                                }
                            }
                        }
                        ret = filmLaden(strUrlFeed, MUSTER_SET + url, thema, titel, datum, zeit, alt);
                    } catch (Exception ex) {
                        Log.fehlerMeldungMReader(-487369532, "MediathekArd.feedEinerSeiteSuchen-2", new String[]{ex.getMessage(), "Thema hat keine Links"});
                    }
                }

            }
            return ret;
        }

        boolean filmLaden(String urlFeed, String urlFilm, String thema, String titel, String datum, String zeit, boolean alt) {
            //mediaCollection.addMediaStream(0, 0, "rtmp://vod.daserste.de/ardfs/", "mp4:videoportal/Film/c_110000/115698/format117354.f4v?sen=Beckmann&amp;for=Web-S&amp;clip=Simone+Rethel+%FCber+Altern+in+W%FCrde");
            //mediaCollection.addMediaStream(0, 1, "rtmp://vod.daserste.de/ardfs/", "mp4:videoportal/Film/c_110000/115698/format117353.f4v?sen=Beckmann&amp;for=Web-M&amp;clip=Simone+Rethel+%FCber+Altern+in+W%FCrde");
            //mediaCollection.addMediaStream(0, 2, "rtmp://vod.daserste.de/ardfs/", "mp4:videoportal/Film/c_110000/115698/format117383.f4v?sen=Beckmann&amp;for=Web-L&amp;clip=Simone+Rethel+%FCber+Altern+in+W%FCrde");

            //mediaCollection.addMediaStream(0, 1, "", "http://www.hr.gl-systemhaus.de/flash/fs/ctmagazin/20100206_ct_schnurer_hilft.flv");
            //mediaCollection.addMediaStream(0, 2, "", "http://www.hr.gl-systemhaus.de/mp4/ARDmediathek/20100206_ct_schnurer_hilft.mp4");

            //mediaCollection.addMediaStream(2, 0, "", "http://lsd.newmedia.nacamar.net/bb/redirect.lsc?stream=swr/extra/abgeordnet/301131.s.wmv&content=content&media=ms");
            //mediaCollection.addMediaStream(2, 1, "", "http://lsd.newmedia.nacamar.net/bb/redirect.lsc?stream=swr/extra/abgeordnet/301131.m.wmv&content=content&media=ms");

            //mediaCollection.addMediaStream(0, 0, "rtmp://swr.fcod.llnwd.net/a4332/e6/", "swr-fernsehen/dasding-tv/2010/02-13/337138.s");

            //url bauen:
            //flvstreamer --host vod.daserste.de --app ardfs --playpath mp4:videoportal/Film/c_100000/106579/format106899.f4v > bla.flv

            final String MUSTER_URL1a = "mediaCollection.addMediaStream(0, 2, \"rtmp";
            final String MUSTER_URL1b = "mediaCollection.addMediaStream(0, 1, \"rtmp";
            final String MUSTER_URL1c = "mediaCollection.addMediaStream(0, 0, \"rtmp";

            final String MUSTER_URL2a = "mediaCollection.addMediaStream(0, 2, \"\"";
            final String MUSTER_URL2b = "mediaCollection.addMediaStream(2, 2, \"\"";
            final String MUSTER_URL2c = "mediaCollection.addMediaStream(0, 1, \"\"";
            final String MUSTER_URL2d = "mediaCollection.addMediaStream(2, 1, \"\"";

            boolean ret = false;
            meldung(urlFilm);
            seite2 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite2, "urlFeed: " + urlFeed);
            int pos1;
            int pos1Tmp;
            int pos1a;
            int pos2;
            int pos3;
            String protokoll = "";
            String url1a = "";
            String url1b = "";
            String url2 = "";
            boolean flash = true;
            //Flashfilme
            if ((pos1 = seite2.indexOf(MUSTER_URL1a)) != -1) {
                pos1 += MUSTER_URL1a.length();
            } else if ((pos1 = seite2.indexOf(MUSTER_URL1b)) != -1) {
                pos1 += MUSTER_URL1b.length();
            } else if ((pos1 = seite2.indexOf(MUSTER_URL1c)) != -1) {
                pos1 += MUSTER_URL1c.length();
            }
            if (pos1 != -1) {
                //wenn Flash und eine url gefunden, dann ...
                if ((pos1a = seite2.indexOf("//", pos1)) != -1) {
                    pos1a += 2;
                    if ((pos2 = seite2.indexOf("\"", pos1a)) != -1) {
                        protokoll = seite2.substring(pos1, pos1a);
                        protokoll = "rtmp" + protokoll;
                        url1a = seite2.substring(pos1a, pos2);
                        if (!url1a.equals("")) {
                            url1b = url1a.substring(url1a.indexOf("/") + 1);
                            url1a = url1a.substring(0, url1a.indexOf("/"));
                        }
                        pos1 = pos2;
                    }
                }
            }
            if (pos1 == -1) {
                //reguläre Filme
                flash = false;
                if ((pos1 = seite2.indexOf(MUSTER_URL2a, 0)) != -1) {
                    pos1 += MUSTER_URL2a.length();
                } else if ((pos1 = seite2.indexOf(MUSTER_URL2b)) != -1) {
                    pos1 += MUSTER_URL2b.length();
                } else if ((pos1 = seite2.indexOf(MUSTER_URL2c)) != -1) {
                    pos1 += MUSTER_URL2c.length();
                    //falls es einen weiteren gibt (für Tagesschau..)
                    if ((pos1Tmp = seite2.indexOf(MUSTER_URL2c, pos1)) != -1) {
                        pos1 = pos1Tmp + MUSTER_URL2c.length();
                    }
                } else if ((pos1 = seite2.indexOf(MUSTER_URL2d)) != -1) {
                    pos1 += MUSTER_URL2d.length();
                }
            }
            //############################
            //Teil 2 der URL finden
            if (pos1 != -1) {
                //wenn eine url gefunden, dann ...
                pos1 = pos1 + 1;
                pos1 = seite2.indexOf("\"", pos1); //Anfang
                pos1 += 1;
                pos2 = seite2.indexOf("?", pos1); //entweder Ende
                pos3 = seite2.indexOf("\"", pos1); // oder da zu Ende
                if (flash) {
                    if (pos2 < pos3) {
                        if (pos1 != -1 && pos2 != -1) {
                            url2 = seite2.substring(pos1, pos2);
                        }
                    } else {
                        if (pos1 != -1 && pos3 != -1) {
                            url2 = seite2.substring(pos1, pos3);
                        }
                    }
                } else {
                    //kein Flash, dann die ganze Zeile nehmen
                    if (pos1 != -1 && pos3 != -1) {
                        url2 = seite2.substring(pos1, pos3);
                    }
                }
                DatenFilm film;
                if (flash) {
                    String urlOrg = addsUrl(protokoll + url1a, url1b);
                    urlOrg = addsUrl(urlOrg, url2);
                    String urlRtmp = "--host " + url1a + " --app " + url1b + " --playpath " + url2;
                    //flvstreamer --host vod.daserste.de --app ardfs --playpath mp4:videoportal/Film/c_100000/106579/format106899.f4v > bla.flv
                    //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String uurlRtmp, String zziel)
                    film = new DatenFilm(nameSenderMReader, thema, urlFeed, titel, urlOrg, urlRtmp, datum, zeit);
                } else {
                    film = new DatenFilm(nameSenderMReader, thema, urlFeed, titel, url2 /* url */, "" /* urlRtmp */, datum, zeit);
                }
                addFilm(film);
                ret = true;
            } else {
                Log.fehlerMeldungMReader(-673219867, "MediathekArd.filmLaden", "keine Url für: " + urlFilm);
            }
            return ret;
        }

        private synchronized String[] getListeThemen() {
            return listeThemen.pollFirst();
        }
    }
}
