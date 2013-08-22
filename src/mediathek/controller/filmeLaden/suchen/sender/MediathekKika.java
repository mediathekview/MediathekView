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
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.MVStringBuilder;
import org.apache.commons.lang3.StringEscapeUtils;

public class MediathekKika extends MediathekReader implements Runnable {

    public static final String SENDER = "KiKA";
    private MVStringBuilder seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
    LinkedListUrl listeThemenHttp = new LinkedListUrl();

    public MediathekKika(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 500, startPrio);
    }

//    @Override
//    synchronized void meldungThreadUndFertig() {
//        if (threads <= 1) {
//            // vorher noch laufen
//            addToListHttp();
//        }
//        super.meldungThreadUndFertig();
//    }
    @Override
    void addToList() {
        meldungStart();
        addToListNormal();
        addToListHttp();
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0 && listeThemenHttp.size() == 0) {
            meldungThreadUndFertig();
        } else {
            listeSort(listeThemen, 1);
            listeSort(listeThemenHttp, 1);
            meldungAddMax(listeThemen.size() + listeThemenHttp.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                //new Thread(new ThemaLaden()).start();
                Thread th = new Thread(new ThemaLaden());
                th.setName(nameSenderMReader + t);
                th.start();
            }

        }
    }

    void addToListNormal() {
        //<strong style="margin-left:10px;">Sesamstraße präsentiert: Eine Möhre für Zwei</strong><br />
        //<a style="margin-left:20px;" href="?programm=168&amp;id=14487&amp;ag=5" title="Sendung vom 10.10.2012" class="overlay_link">42. Ordnung ist das halbe Chaos</a><br />
        //<a style="margin-left:20px;" href="?programm=168&amp;id=14485&amp;ag=5" title="Sendung vom 10.10.2012" class="overlay_link">41. Über den Wolken</a><br />
        final String ADRESSE = "http://kikaplus.net/clients/kika/kikaplus/";
        final String MUSTER_URL = "<a style=\"margin-left:20px;\" href=\"";
        final String MUSTER_THEMA = "<strong style=\"margin-left:10px;\">";
        final String MUSTER_DATUM = "title=\"Sendung vom ";
        listeThemen.clear();
        seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        seite = getUrlIo.getUri(nameSenderMReader, ADRESSE, Konstanten.KODIERUNG_UTF, 3, seite, "KiKA: Startseite");
        int pos = 0;
        int pos1, pos2, stop, pDatum1, pDatum2, pTitel1, pTitel2;
        String url, thema, datum, titel;
        while ((pos = seite.indexOf(MUSTER_THEMA, pos)) != -1) {
            try {
                thema = "";
                pos += MUSTER_THEMA.length();
                stop = seite.indexOf(MUSTER_THEMA, pos);
                pos1 = pos;
                if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                    thema = seite.substring(pos1, pos2);
                }
                while ((pos1 = seite.indexOf(MUSTER_URL, pos1)) != -1) {
                    titel = "";
                    datum = "";
                    if (stop != -1 && pos1 > stop) {
                        // dann schon das nächste Thema
                        break;
                    }
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                        url = seite.substring(pos1, pos2);
                        //if (!url.equals("")) {
                        url = StringEscapeUtils.unescapeXml(url);
                        if (!url.equals("") && !url.startsWith("http://") && !url.startsWith("/")) {
                            // Datum
                            if ((pDatum1 = seite.indexOf(MUSTER_DATUM, pos2)) != -1) {
                                pDatum1 += MUSTER_DATUM.length();
                                if ((pDatum2 = seite.indexOf("\"", pDatum1)) != -1) {
                                    if (stop != -1 && pDatum1 < stop && pDatum2 < stop) {
                                        // dann schon das nächste Thema
                                        datum = seite.substring(pDatum1, pDatum2);
                                    }
                                }
                            }

                            // Titel
                            if ((pTitel1 = seite.indexOf(">", pos2)) != -1) {
                                pTitel1 += 1;
                                if ((pTitel2 = seite.indexOf("<", pTitel1)) != -1) {
                                    //if (stop != -1 && pTitel1 > stop && pTitel2 > stop) {
                                    if (stop != -1 && pTitel1 < stop && pTitel2 < stop) {
                                        titel = seite.substring(pTitel1, pTitel2);
                                    }
                                }
                            }
                            // in die Liste eintragen
                            String[] add = new String[]{ADRESSE + url, thema, titel, datum};
                            listeThemen.addUrl(add);
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-302025469, Log.FEHLER_ART_MREADER, "MediathekKiKA.addToList", ex, "");
            }
        }
    }

    void addToListHttp() {
        //http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=az&age=3&page=all
        //http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=az&age=6&page=all
        //http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=az&age=10&page=all

        final String ADRESSE_1 = "http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=az&age=";
        final String ADRESSE_2 = "&page=all";
        final String MUSTER_START = "var programletterlist = {\"";
        final String[] ALTER = {"3", "6", "10"};
        listeThemenHttp.clear();
        seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        int pos1 = 0, pos2, start = 0, stop = 0;
        String id = "", thema = "", url = "";
        for (String alter : ALTER) {
            seite = getUrlIo.getUri(nameSenderMReader, ADRESSE_1 + alter + ADRESSE_2, Konstanten.KODIERUNG_UTF, 3, seite, "KiKA: Startseite");
            if ((start = seite.indexOf(MUSTER_START)) != -1) {
                start += MUSTER_START.length();
                pos1 = start;
                if ((stop = seite.indexOf("}};", start)) != -1) {
                    while ((pos1 = seite.indexOf("\"", pos1)) != -1) {
                        thema = "";
                        id = "";
                        pos1 += 1;
                        if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                            id = seite.substring(pos1, pos2);
                            try {
                                // Testen auf Zehl
                                int z = Integer.parseInt(id);
                            } catch (Exception ex) {
                                continue;
                            }
                            ++pos2;
                            if ((pos1 = seite.indexOf("\"", pos2)) != -1) {
                                ++pos1;
                                if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                                    thema = seite.substring(pos1, pos2);
                                }
                            }
                            // http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=program&id=165&age=10
                            url = "http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=program&id=" + id + "&age=" + alter;
                            // in die Liste eintragen
                            String[] add = new String[]{url, thema, alter};
                            listeThemenHttp.addUrl(add);
                        }
                    }
                }
            }
        }
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private MVStringBuilder seite1 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    laden(link[0] /* url */, link[1] /* Thema */, link[2] /* Titel */, link[3] /*Datum*/);
                }
                while (!Daten.filmeLaden.getStop() && (link = listeThemenHttp.getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    ladenHttp(link[0] /* url */, link[1] /* Thema */, link[2] /* alter */);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-987452384, Log.FEHLER_ART_MREADER, "Mediathek3Sat.ThemaLaden.run", ex, "");
            }
            meldungThreadUndFertig();
        }

        void laden(String filmWebsite, String thema, String titel, String datum) {
            //so.addVariable("pfad","rtmp://88.198.74.226/vod/mp4:1348908081-7b8b1356b478db154fdbf8bf6a01fc1f.mp4");
            //so.addVariable("fullscreenPfad", "rtmp://88.198.74.226/vod/mp4:1348908081-7b8b1356b478db154fdbf8bf6a01fc1f-01.mp4");
            //xajax.config.requestURI = "http://kikaplus.net/clients/kika/kikaplus/?programm=270&id=33172&ag=5";
            final String MUSTER_URL_1 = "so.addVariable(\"fullscreenPfad\", \"";
            final String MUSTER_URL_2 = "so.addVariable(\"pfad\", \"";
            final String MUSTER_ID = "xajax.config.requestURI = \"";
            seite1 = getUrlIo.getUri(nameSenderMReader, filmWebsite, Konstanten.KODIERUNG_UTF, 1, seite1, thema + " " + titel);
            String id;
            String urlFilm = "";
            id = seite1.extract(MUSTER_ID, "\"");
            urlFilm = seite1.extract(MUSTER_URL_1, "\"");
            if (urlFilm.isEmpty()) {
                urlFilm = seite1.extract(MUSTER_URL_2, "\"");
            }
            if (!urlFilm.equals("")) {
                meldung(urlFilm);
                addFilm(new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, urlFilm, "-r " + urlFilm + " --flashVer WIN11,4,402,265"/* urlRtmp */,
                        datum/*datum*/, ""/*zeit*/, 0, "", "", "", new String[]{""}));
            } else {
                Log.fehlerMeldung(-912036789, Log.FEHLER_ART_MREADER, "MediathekKika", "keine URL: " + filmWebsite);
            }
        }

        void ladenHttp(String filmWebsite, String thema, String alter) {
            // erst die Nummer suchen
            // >25420<4<165<23. Die b
            // <http://www.kikaplus.net/clients/kika/mediathek/previewpic/1355302530-f80b463888fd4602d925b2ef2c861928 9.jpg<
            final String MUSTER_BILD = "<http://www.";
            seite1 = getUrlIo.getUri(nameSenderMReader, filmWebsite, Konstanten.KODIERUNG_UTF, 1, seite1, thema);
            String nummer, url, bild, urlFilm, titel;
            nummer = seite1.extract(">", "<");
            bild = seite1.extract(MUSTER_BILD, "<");
            if (!bild.isEmpty()) {
                bild = "http://www." + bild;
            }
            // die URL bauen:
            // http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=video&age=10&id=25420&page=az
            if (!nummer.isEmpty()) {
                url = "http://www.kikaplus.net/clients/kika/kikaplus/hbbtv/index.php?cmd=video&age=" + alter + "&id=" + nummer + "&page=az";
                seite1 = getUrlIo.getUri(nameSenderMReader, url, Konstanten.KODIERUNG_UTF, 1, seite1, thema);
                final String MUSTER_URL = "var videofile = \"";
                final String MUSTER_TITEL = "var videotitle = \"";
                urlFilm = seite1.extract(MUSTER_URL, "\"");
                titel = seite1.extract(MUSTER_TITEL, "\"");
                if (!urlFilm.equals("")) {
                    meldung(urlFilm);
                    // DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String uurlRtmp,
                    //     String datum, String zeit,
                    //     long dauerSekunden, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
                    DatenFilm film = null;
                    thema = GuiFunktionen.utf8(thema);
                    titel = GuiFunktionen.utf8(titel);
                    if ((film = istInFilmListe(nameSenderMReader, thema, titel)) != null) {
                        film.addUrlKlein(urlFilm, "");
                    } else {
                        // dann gibs ihn noch nicht
                        addFilm(new DatenFilm(nameSenderMReader, thema, "http://www.kika.de/fernsehen/mediathek/index.shtml", titel, urlFilm, ""/* urlRtmp */,
                                ""/*datum*/, ""/*zeit*/,
                                0, "", bild, "", new String[]{""}));
                    }
                } else {
                    Log.fehlerMeldung(-915263078, Log.FEHLER_ART_MREADER, "MediathekKika", "keine URL: " + filmWebsite);
                }
            }
        }
    }
}
