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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.concurrent.Semaphore;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;
import mediathek.tool.DatumZeit;

/**
 *
 * @author
 */
public class MediathekArte7 extends MediathekReader implements Runnable {

    public static final String SENDER_ARTE_DE = "ARTE.DE";
    public static final String SENDER_ARTE_FR = "ARTE.FR";
    static boolean arte_de = true;
    static boolean arte_fr = true;
    static boolean lSchon = false;
    private final String THEMA_ARTE_7 = "Arte+7";
    private int themenLaufen = 0;
    private int MAX_SEITEN = 15;
    private LinkedList<String[]> listeFilmseiten = new LinkedList<String[]>();
    private LinkedList<String[]> listeFilmseitenFertig = new LinkedList<String[]>();
    private boolean de;
    private Semaphore sem = new Semaphore(1);

    /**
     *
     * @param ddaten
     * @param dde
     */
    public MediathekArte7(FilmeSuchen ssearch, boolean dde) {
        super(ssearch, /* name */ "", /* text */ "", /* threads */ 6, /* urlWarten */ 500);
        de = dde;
        if (de) {
            senderText = "Arte DE (bis ca. 200 MB, bis 2000 Filme)";
            senderName = SENDER_ARTE_DE;
        } else {
            senderText = "Arte FR (bis ca. 200 MB, bis 2000 Filme)";
            senderName = SENDER_ARTE_FR;
        }
    }

    //===================================
    // public
    //===================================
    @Override
    public synchronized void addToList() {
        if (!laufeSchon()) {
            addToList_de_fr();
        }
    }

    private static synchronized boolean laufeSchon() {
        if (!lSchon) {
            lSchon = true;
            return false;
        } else {
            return true;
        }
    }

    private void addToList_de_fr() {
        try {
            threads = 0;
            arte_de = suchen.senderAn(SENDER_ARTE_DE);
            arte_fr = suchen.senderAn(SENDER_ARTE_FR);
            listeThemen.clear();
            listeFilmseiten.clear();
            listeFilmseitenFertig.clear();
            //immer erst mal Arte7!!
            String[] add = new String[]{"http://videos.arte.tv/de/videos", THEMA_ARTE_7};
            listeThemen.add(add);
            //und der Rest nur auf Wunsch
            if (suchen.allesLaden) {
                //erst mal alle Themen suchen um die Filme nicht doppelt zu suchen
                addToList__("http://videos.arte.tv/de/videos/sendungen");
                addToList__("http://videos.arte.tv/de/videos/alleVideos");
            }
            meldungStart(listeThemen.size());
            new Thread(new ArteThemaLaden()).start(); // da reicht einer
        } catch (Exception ex) {
            Log.fehlerMeldung("MediathekArte7.addToList_de_fr", ex);
        }
    }

    private boolean addToList__(String ADRESSE) {
        //Theman suchen
        boolean ret = false;
        final String START = "<div class=\"navTop\"></div>";
        final String STOP = "<div id=\"content\">";
        final String MUSTER_URL = "<li><a href=\"";
        final String URL_THEMA_PREFIX = "http://videos.arte.tv";
        //url-Thema
        // http://videos.arte.tv/de/videos/sendungen/360_geo/index-3188704.html
        //url
        // <li><a href="/de/videos/sendungen/360_geo/index-3188704.html">360° - GEO-Reportage<span id="3188704"></span></a></li>
//        de = Boolean.parseBoolean(daten.system[Konstanten.SYSTEM_ARTE_DE_NR]);
//        fr = Boolean.parseBoolean(daten.system[Konstanten.SYSTEM_ARTE_FR_NR]);
        StringBuffer strSeite = new StringBuffer();
        //strSeite = getUrlIo.getUri_Utf(senderName, ADRESSE, strSeite, "");
        strSeite = getUrlIo.getUri(senderName, ADRESSE, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 5 /* versuche */, strSeite, "" /* Meldung */);
        int pos = 0;
        int pos1;
        int pos2;
        int ende = strSeite.indexOf(STOP);
        String url;
        String thema;
        if ((pos = strSeite.indexOf(START, pos)) != -1) {
            while (!Daten.filmeLaden.getStop() && (pos = strSeite.indexOf(MUSTER_URL, pos)) != -1) {
                if (pos > ende) {
                    //Themenbereich zu Ende
                    break;
                }
                thema = "";
                pos += MUSTER_URL.length();
                pos1 = pos;
                pos2 = strSeite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                    url = strSeite.substring(pos1, pos2);
                    pos1 = strSeite.indexOf("\">", pos);
                    pos2 = strSeite.indexOf("<", pos);
                    if (!url.equals("")) {
                        ret = true;
                    }
                    if (pos1 != -1 && pos2 != -1) {
                        thema = strSeite.substring(pos1 + 2, pos2);
                        //nur Abos laden
                        if (arte_de && !themaLaden(SENDER_ARTE_DE, thema) || arte_fr && !themaLaden(SENDER_ARTE_FR, thema)) {
                            continue;
                        }
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldung("MediathekArte.addToList__", "keine URL" + senderName + thema);
                    } else {
                        String[] add = new String[]{URL_THEMA_PREFIX + url, thema};
                        if (!istInListe(listeThemen, url, 0)) {
                            listeThemen.add(add);
                        }
                    }
                }
            }
        }
        return ret;
    }

    //===================================
    // private
    //===================================
    private class ArteThemaLaden implements Runnable {

        GetUrl getUrl7 = new GetUrl(senderWartenSeiteLaden);
        GetUrl getUrl = new GetUrl(senderWartenSeiteLaden);
        String[] link = null;
        private StringBuffer strSeite1 = new StringBuffer();

        @Override
        public void run() {
            try {
                getSem();
                ++themenLaufen;
                ++threads;
                meldung("");
                relSem();
                // dieser Thread läuft nur 1x
                for (int t = 0; t < senderMaxThread; ++t) {
                    new Thread(new ArteFilmseitenLaden()).start();
                }
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    themenSeitenSuchen(link[0] /* url */, link[1] /* Thema */);
                    meldungProgress(link[0]);
                }
                getSem();
                --threads;
                --themenLaufen;
                meldung("");
                relSem();
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekArte7.ArteThemaLaden.run", ex);
            }
        }

        private void themenSeitenSuchen(String strUrlFeed, String thema) {
            //weitere Seiten
            //<div class="pagination">
            //<ul class="list">
            //<li><a href="/de/videos/artejournal?hash=fr%2Fthumb%2F%2F%2F1%2F20%2F" class="current">1</a></li>
            //
            //<li><a href="/de/videos/artejournal?hash=fr%2Fthumb%2F%2F%2F2%2F20%2F">2</a></li>
            //<li><a href="/de/videos/artejournal?hash=fr%2Fthumb%2F%2F%2F3%2F20%2F">3</a></li>
            //<li><a href="/de/videos/artejournal?hash=fr%2Fthumb%2F%2F%2F4%2F20%2F">4</a></li>
            //<li><a href="/de/videos/artejournal?hash=fr%2Fthumb%2F%2F%2F2%2F20%2F">Weiter</a></li>

            final String MUSTER_THEMA_START = "<div class=\"pagination";
            final String MUSTER_THEMA_STOP = "Weiter</a></li>";

            final String MUSTER_THEMA_URL = "<li><a href=\"";
            final String MUSTER_URL = "<h2><a href=\"";
            final String URL_THEMA_PREFIX = "http://videos.arte.tv";

            LinkedList<String> themenseiten = new LinkedList<String>();
            themenseiten.add(strUrlFeed); //erste Themenseite
            int count = 0;
            int pos;
            int pos1;
            int pos2;
            int start;
            int ende;
            String url;
            String titel;
            String seite;
            boolean gefunden;
            boolean ersteSeite = true;
            seite = strUrlFeed;
            if (suchen.allesLaden || thema.startsWith(THEMA_ARTE_7)) {
                //auch nach älteren Beiträgen suchen
                do {
                    gefunden = false;
                    meldung("*" + seite);
                    if (thema.startsWith(THEMA_ARTE_7)) {
                        //strSeite1 = getUrl7.getUri_Utf(senderName, seite, strSeite1, "");
                        strSeite1 = getUrl7.getUri(senderName, seite, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 5 /* versuche */, strSeite1, "" /* Meldung */);
                    } else {
                        //strSeite1 = getUrl.getUri_Utf(senderName, seite, strSeite1, "");
                        strSeite1 = getUrl.getUri(senderName, seite, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 5 /* versuche */, strSeite1, "" /* Meldung */);
                    }
                    if ((start = strSeite1.indexOf(MUSTER_THEMA_START)) != -1) {
                        ende = strSeite1.indexOf(MUSTER_THEMA_STOP);
                        pos = start;
                        while (!Daten.filmeLaden.getStop() && (pos = strSeite1.indexOf(MUSTER_THEMA_URL, pos)) != -1 && pos < ende) {
                            pos += MUSTER_THEMA_URL.length();
                            pos1 = pos;
                            pos2 = strSeite1.indexOf("\"", pos);
                            if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                                url = strSeite1.substring(pos1, pos2);
                                //url = "http://videos.arte.tv/" + url.replaceAll("%2", "/");
                                url = "http://videos.arte.tv/" + url;
                                if (ersteSeite) {
                                    ersteSeite = false;
                                } else if (!themenseiten.contains(url)) {
                                    themenseiten.add(url);
                                    gefunden = true;
                                    ++count;
                                }
                            }
                        }
                    }
                    seite = themenseiten.getLast();
                } while (!Daten.filmeLaden.getStop() && gefunden && count < MAX_SEITEN);
            }
            pos = 0;
            Iterator<String> it = themenseiten.iterator();
            while (!Daten.filmeLaden.getStop() && it.hasNext()) {
                seite = it.next();
                if (thema.startsWith(THEMA_ARTE_7)) {
                    //strSeite1 = getUrl7.getUri_Utf(senderName, seite, strSeite1, "");
                    strSeite1 = getUrl7.getUri(senderName, seite, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 5 /* versuche */, strSeite1, "" /* Meldung */);
                } else {
                    //strSeite1 = getUrl.getUri_Utf(senderName, seite, strSeite1, "");
                    strSeite1 = getUrl.getUri(senderName, seite, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 5 /* versuche */, strSeite1, "" /* Meldung */);
                }
                while (!Daten.filmeLaden.getStop() && (pos = strSeite1.indexOf(MUSTER_URL, pos)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    pos2 = strSeite1.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                        url = strSeite1.substring(pos1, pos2);
                        if (!url.equals("")) {
                            pos1 = strSeite1.indexOf(">", pos);
                            pos2 = strSeite1.indexOf("</a", pos);
                            if (pos1 != -1 && pos2 != -1) {
                                titel = strSeite1.substring(pos1 + 1, pos2);
                                //weiter gehts
                                String[] add = new String[]{strUrlFeed, thema, titel, URL_THEMA_PREFIX + url};
                                if (!istInListeFilmseiteFertig(add)) {
                                    getAddListeFilmseiten(add);
                                    //addFilme2(strUrlFeed, thema, titel, URL_THEMA_PREFIX + url);
                                }
                            }
                        }
                    } else {
                        Log.fehlerMeldung("MediathekArte.themenSeiteSuchen", "keine Url addFilme" + senderName + thema);
                    }
                }
            }
        }
    }

    private class ArteFilmseitenLaden implements Runnable {

        GetUrl getUrl7 = new GetUrl(senderWartenSeiteLaden);
        GetUrl getUrl = new GetUrl(5000, senderWartenSeiteLaden);
        private StringBuffer strSeite2 = new StringBuffer();
        private StringBuffer strSeite3 = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                getSem();
                ++threads;
                meldung("");
                relSem();
                String[] filmseite;
                do {
                    try {
                        while (!Daten.filmeLaden.getStop() && (filmseite = getAddListeFilmseiten(null)) != null) {
                            //addFilme2(String strUrlFeed, String thema, String titel, String urlFilm, String alt);
                            addFilme2(filmseite[0], filmseite[1], filmseite[2], filmseite[3]);
                            meldungProgress(filmseite[3]);
                        }
                        //da ist die Themenliste noch nicht fertig
                        this.wait(500);
                    } catch (Exception ex) {
                        Log.fehlerMeldung("MediathekArte7.ArteFilmseitenLaden.run", ex);
                    }
                } while (!Daten.filmeLaden.getStop() && themenLaufen > 0);
                getSem();
                --threads;
                meldung("");
                meldungThreadUndFertig();
                relSem();
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekArte7.ArteFilmseitenLaden.run", ex);
            }
        }

        private void addFilme2(String strUrlFeed, String thema, String titel, String urlFilm) {
            //Film eines Themas laden
            // Muster zum Suchen:
            //vars_player.videorefFileUrl = "http://videos.arte.tv/de/do_delegate/videos/virtuelle_welten_in_cannes-3220966,view,asPlayerXml.xml";
            //zur Auth:
            //var url_player = "http://videos.arte.tv/blob/web/i18n/view/player_8-3188338-data-4797751.swf";

            final String MUSTER_URL = "vars_player.videorefFileUrl = \"";
            final String MUSTER_AUTH = "var url_player = \"";
            meldung("*" + urlFilm);
            if (thema.startsWith(THEMA_ARTE_7)) {
                //strSeite2 = getUrl7.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrl7.getUri(senderName, urlFilm, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 3 /* versuche */, strSeite2, "" /* Meldung */);
           } else {
                //strSeite2 = getUrl.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrl.getUri(senderName, urlFilm, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 3 /* versuche */, strSeite2, "" /* Meldung */);
            }
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            String authurl = "";
            //auth suchen
            if (!Daten.filmeLaden.getStop() && (pos = strSeite2.indexOf(MUSTER_AUTH, pos)) != -1) {
                pos += MUSTER_AUTH.length();
                pos1 = pos;
                pos2 = strSeite2.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                    authurl = strSeite2.substring(pos1, pos2);
                    if (authurl.equals("")) {
                        Log.fehlerMeldung("MediathekArte.addFilme2", "keine Auth-Url" + senderName + thema);
                    }
                }
            }
            //url suchen
            pos = 0;
            if ((pos = strSeite2.indexOf(MUSTER_URL, pos)) != -1) {
                pos += MUSTER_URL.length();
                pos1 = pos;
                pos2 = strSeite2.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                    url = strSeite2.substring(pos1, pos2);
                    if (!url.equals("")) {
                        //weiter gehts
                        addFilme3(strUrlFeed, thema, titel, url, authurl);
                    } else {
                        Log.fehlerMeldung("MediathekArte.addFilme2", "keine Url" + senderName + thema);
                    }
                }
            }
        }

        private void addFilme3(String strUrlFeed, String thema, String titel, String urlFilm, String authurl) {
            //Film eines Themas laden
            // Muster zum Suchen:
            // de
            //<video lang="de" ref="
            // fr
            //<video lang="fr" ref="
            //urlFilm
            //<video lang="de" ref="http://videos.arte.tv/de/do_delegate/videos/virtuelle_welten_in_cannes-3220944,view,asPlayerXml.xml"/>

            final String MUSTER_URL_DE = "<video lang=\"de\" ref=\"";
            final String MUSTER_URL_FR = "<video lang=\"fr\" ref=\"";
            if (thema.startsWith(THEMA_ARTE_7)) {
                //strSeite2 = getUrl7.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrl7.getUri(senderName, urlFilm, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 3 /* versuche */, strSeite2, "" /* Meldung */);
            } else {
                //strSeite2 = getUrl.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrl.getUri(senderName, urlFilm, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 3 /* versuche */, strSeite2, "" /* Meldung */);
            }
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            String MUSTER_URL;
            if (!Daten.filmeLaden.getStop() && arte_de) {
                MUSTER_URL = MUSTER_URL_DE;
                if ((pos = strSeite2.indexOf(MUSTER_URL, pos)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    pos2 = strSeite2.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                        url = strSeite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            //weiter gehts jetzt wirklich zum Film
                            addFilme4(SENDER_ARTE_DE, strUrlFeed, thema, titel, url, authurl);
                        } else {
                            Log.fehlerMeldung("MediathekArte.addFilme3-1", "keine Url" + senderName + thema);
                        }
                    }
                }
            }
            if (!Daten.filmeLaden.getStop() && arte_fr) {
                MUSTER_URL = MUSTER_URL_FR;
                if ((pos = strSeite2.indexOf(MUSTER_URL, pos)) != -1) {
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    pos2 = strSeite2.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                        url = strSeite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            //weiter gehts jetzt wirklich zum Film
                            addFilme4(SENDER_ARTE_FR, strUrlFeed, thema, titel, url, authurl);
                        } else {
                            Log.fehlerMeldung("MediathekArte.addFilme3-2", "keine Url" + senderName + thema);
                        }
                    }
                }
            }
        }

        private void addFilme4(String senderArte, String strUrlFeed, String thema, String titel, String urlFilm, String authurl) {
            //Film eines Themas laden
            // Muster zum Suchen:
            //<url quality="hd">
            // rtmp://artestras.fcod.llnwd.net/a3903/o35/MP4:geo/videothek/ALL/arteprod/2010/05/16/ARTE3220966_DE_28919_16by9_800_MP4?h=db7388494aa3ec5cda2a7decfb083ce4
            // </url>
            //<dateVideo>Thu, 25 Nov 2010 18:06:32 +0100</dateVideo>
            String tmp;
            String datum = "";
            String zeit = "";
            final String MUSTER_DATUM = "<dateVideo>";
            final String MUSTER_URL = "<url quality=\"hd\">";
            if (thema.startsWith(THEMA_ARTE_7)) {
                //strSeite3 = getUrl7.getUri_Utf(senderName, urlFilm, strSeite3, "");
                strSeite3 = getUrl7.getUri(senderName, urlFilm, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 3 /* versuche */, strSeite3, "" /* Meldung */);
            } else {
                //strSeite3 = getUrl.getUri_Utf(senderName, urlFilm, strSeite3, "");
                strSeite3 = getUrl.getUri(senderName, urlFilm, Konstanten.KODIERUNG_UTF, 3000 /* timeout */, 3 /* versuche */, strSeite3, "" /* Meldung */);
            }
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            if ((pos = strSeite3.indexOf(MUSTER_DATUM, pos)) != -1) {
                pos1 = pos + MUSTER_DATUM.length();
                if ((pos2 = strSeite3.indexOf("<", pos1)) != -1) {
                    //<dateVideo>Thu, 25 Nov 2010 18:06:32 +0100</dateVideo>
                    tmp = strSeite3.substring(pos1, pos2);
                    if (tmp.equals("")) {
                        Log.fehlerMeldung("MediathekArte.addFilme4-1", "keine Datum" + senderName + thema);
                    } else {
                        datum = DatumZeit.convertDatum(tmp);
                        zeit = DatumZeit.convertTime(tmp);
                    }
                }
            }
            pos = 0;
            if ((pos = strSeite3.indexOf(MUSTER_URL, pos)) != -1) {
                pos += MUSTER_URL.length();
                pos1 = pos;
                pos2 = strSeite3.indexOf("</url>", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                    url = strSeite3.substring(pos1, pos2);
                    if (!url.equals("")) {
                        // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                        DatenFilm film = new DatenFilm(senderArte, thema, strUrlFeed, titel, url, url/* urlOrg */, ""/* urlRtmp */, datum, zeit);
                        film.arr[DatenFilm.FILM_URL_AUTH_NR] = authurl;
                        addFilm(film);
                    } else {
                        Log.fehlerMeldung("MediathekArte.addfilme4-2", "keine Url" + senderName + thema);
                    }
                }
            }
        }
    }

    private synchronized String[] getAddListeFilmseiten(String[] add) {
        if (add != null) {
            this.meldungAddMax(1);
            listeFilmseiten.add(add);
            return null;
        } else {
            return listeFilmseiten.pollFirst();
        }
    }

    private synchronized boolean istInListeFilmseiteFertig(String[] link) {
        boolean ret = false;
        //(String strUrlFeed, String thema, String titel, String urlFilm) {
        if (!link[1].startsWith(THEMA_ARTE_7)) {
            if (istInListe(listeFilmseitenFertig, link[3], 3)) {
                ret = true;
            } else {
                listeFilmseitenFertig.add(link);
            }
        }
        return ret;
    }

    @Override
    void meldungThreadUndFertig() {
        //wird erst ausgeführt wenn alle Threads beendet sind
        if (threads <= 0) { // sonst läuft noch was
            lSchon = false;
        }
        super.meldungThreadUndFertig();
    }

    private void getSem() {
        try {
            sem.acquire();
        } catch (InterruptedException ex) {
            Log.fehlerMeldung("MediathekArte7.getSem", ex, senderName);
        }
    }

    private void relSem() {
        sem.release();
    }
}
