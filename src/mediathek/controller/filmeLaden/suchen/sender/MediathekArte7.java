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

import java.util.Iterator;
import java.util.LinkedList;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.DatumZeit;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class MediathekArte7 extends MediathekReader implements Runnable {

    public static final String SENDER_ARTE_DE = "ARTE.DE";
    public static final String SENDER_ARTE_FR = "ARTE.FR";
    public static final String SENDER_ARTE = "ARTE";
    private final String THEMA_ARTE_7 = "Arte+7";
    private int themenLaufen = 0;
    // Seiten bei den Themen 
    private int MAX_SEITEN = 10;
    private LinkedList<String[]> listeFilmseiten = new LinkedList<String[]>();
    private LinkedList<String[]> listeFilmseitenFertig = new LinkedList<String[]>();

    /**
     *
     * @param ddaten
     * @param dde
     */
    public MediathekArte7(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ "", /* threads */ 5, /* urlWarten */ 500, startPrio);
        nameSenderMReader = SENDER_ARTE;
        getUrlIo.setTimeout(15000);
    }

    //===================================
    // public
    //===================================
    @Override
    public String[] getNameSenderFilmliste() {
        return new String[]{SENDER_ARTE_DE, SENDER_ARTE_FR};
    }

    @Override
    public boolean checkNameSenderFilmliste(String name) {
        return SENDER_ARTE_DE.equalsIgnoreCase(name) || SENDER_ARTE_FR.equalsIgnoreCase(name) || SENDER_ARTE.equalsIgnoreCase(name);
    }

    @Override
    public synchronized void addToList() {
        try {
            listeThemen.clear();
            listeFilmseiten.clear();
            listeFilmseitenFertig.clear();
            meldungStart();
            //nur Arte7!!
            String[] add = new String[]{"http://videos.arte.tv/de/videos", THEMA_ARTE_7};
            listeThemen.add(add);
            meldungAddMax(listeThemen.size());
            // für die Themenseiten
            new Thread(new ArteThemaLaden()).start(); // 
        } catch (Exception ex) {
            Log.fehlerMeldung(-782563914, Log.FEHLER_ART_MREADER, "MediathekArte7.addToList_de_fr", ex, "");
        }
    }

    //===================================
    // private
    //===================================
    private class ArteThemaLaden implements Runnable {

        String[] link = null;
        private StringBuffer strSeite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public void run() {
            try {
                meldungAddThread(); // im ThemenThread
                getAddThemenLaufen(1);
                meldung("");
                // für die Filmseiten
                for (int t = 0; t < maxThreadLaufen; ++t) {
                    new Thread(new ArteFilmseitenLaden()).start();
                }
                //
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    themenSeitenSuchen(link[0] /* url */, link[1] /* Thema */);
                    meldungProgress(link[0]);
                }
                getAddThemenLaufen(-1);
            } catch (Exception ex) {
                Log.fehlerMeldung(-251436904, Log.FEHLER_ART_MREADER, "MediathekArte7.ArteThemaLaden.run", ex, "");
            }
            meldungThreadUndFertig();
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
            if (suchen.senderAllesLaden || thema.startsWith(THEMA_ARTE_7)) {
                //auch nach älteren Beiträgen suchen
                do {
                    gefunden = false;
                    meldung(seite);
                    if (thema.startsWith(THEMA_ARTE_7)) {
                        //strSeite1 = getUrl7.getUri_Utf(senderName, seite, strSeite1, "");
                        strSeite1 = getUrlIo.getUri(nameSenderMReader, seite, Konstanten.KODIERUNG_UTF, 5 /* versuche */, strSeite1, "" /* Meldung */);
                    } else {
                        //strSeite1 = getUrl.getUri_Utf(senderName, seite, strSeite1, "");
                        strSeite1 = getUrlIo.getUri(nameSenderMReader, seite, Konstanten.KODIERUNG_UTF, 3 /* versuche */, strSeite1, "" /* Meldung */);
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
                    strSeite1 = getUrlIo.getUri(nameSenderMReader, seite, Konstanten.KODIERUNG_UTF, 5 /* versuche */, strSeite1, "" /* Meldung */);
                } else {
                    //strSeite1 = getUrl.getUri_Utf(senderName, seite, strSeite1, "");
                    strSeite1 = getUrlIo.getUri(nameSenderMReader, seite, Konstanten.KODIERUNG_UTF, 3 /* versuche */, strSeite1, "" /* Meldung */);
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
                        Log.fehlerMeldung(-203654812, Log.FEHLER_ART_MREADER, "MediathekArte.themenSeiteSuchen", "keine Url addFilme" + nameSenderMReader + thema);
                    }
                }
            }
        }
    }

    private class ArteFilmseitenLaden implements Runnable {

        private StringBuffer strSeite2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer strSeite3 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public synchronized void run() {
            try {
                meldungAddThread(); // für den Seitenthread
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
                        Log.fehlerMeldung(-561408963, Log.FEHLER_ART_MREADER, "MediathekArte7.ArteFilmseitenLaden.run", ex, "");
                    }
                } while (!Daten.filmeLaden.getStop() && getAddThemenLaufen(0) > 0);
                meldung("");
            } catch (Exception ex) {
                Log.fehlerMeldung(-381069831, Log.FEHLER_ART_MREADER, "MediathekArte7.ArteFilmseitenLaden.run", ex, "");
            }
            meldungThreadUndFertig(); // und im SeitenThread gelöscht
        }

        private void addFilme2(String strUrlFeed, String thema, String titel, String urlFilm) {
            //Film eines Themas laden
            // Muster zum Suchen:
            //vars_player.videorefFileUrl = "http://videos.arte.tv/de/do_delegate/videos/virtuelle_welten_in_cannes-3220966,view,asPlayerXml.xml";
            //zur Auth:
            //var url_player = "http://videos.arte.tv/blob/web/i18n/view/player_8-3188338-data-4797751.swf";

            final String MUSTER_URL = "vars_player.videorefFileUrl = \"";
            final String MUSTER_AUTH = "var url_player = \"";
            meldung(urlFilm);
            if (Daten.filmeLaden.getStop()) {
                return;
            }
            if (thema.startsWith(THEMA_ARTE_7)) {
                //strSeite2 = getUrl7.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrlIo.getUri(nameSenderMReader, urlFilm, Konstanten.KODIERUNG_UTF, 3 /* versuche */, strSeite2, "" /* Meldung */);
            } else {
                //strSeite2 = getUrl.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrlIo.getUri(nameSenderMReader, urlFilm, Konstanten.KODIERUNG_UTF, 2 /* versuche */, strSeite2, "" /* Meldung */);
            }
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            String authurl = "";
            //auth suchen
            if ((pos = strSeite2.indexOf(MUSTER_AUTH, pos)) != -1) {
                pos += MUSTER_AUTH.length();
                pos1 = pos;
                pos2 = strSeite2.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                    authurl = strSeite2.substring(pos1, pos2);
                    if (authurl.equals("")) {
                        Log.fehlerMeldung(-450983058, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme2", "keine Auth-Url" + nameSenderMReader + thema);
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
                        Log.fehlerMeldung(-462037812, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme2", "keine Url" + nameSenderMReader + thema);
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
            if (Daten.filmeLaden.getStop()) {
                return;
            }
            final String MUSTER_URL_DE = "<video lang=\"de\" ref=\"";
            final String MUSTER_URL_FR = "<video lang=\"fr\" ref=\"";
            if (thema.startsWith(THEMA_ARTE_7)) {
                //strSeite2 = getUrl7.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrlIo.getUri(nameSenderMReader, urlFilm, Konstanten.KODIERUNG_UTF, 3 /* versuche */, strSeite2, "" /* Meldung */);
            } else {
                //strSeite2 = getUrl.getUri_Utf(senderName, urlFilm, strSeite2, "");
                strSeite2 = getUrlIo.getUri(nameSenderMReader, urlFilm, Konstanten.KODIERUNG_UTF, 2 /* versuche */, strSeite2, "" /* Meldung */);
            }
            int pos1;
            int pos2;
            String url;
            String MUSTER_URL;
            // ARTE.DE
            MUSTER_URL = MUSTER_URL_DE;
            if ((pos1 = strSeite2.indexOf(MUSTER_URL)) != -1) {
                pos1 += MUSTER_URL.length();
                if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                    url = strSeite2.substring(pos1, pos2);
                    if (!url.equals("")) {
                        //weiter gehts jetzt wirklich zum Film
                        addFilme4(SENDER_ARTE_DE, strUrlFeed, thema, titel, url, authurl);
                    } else {
                        Log.fehlerMeldung(-983269760, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme3-1", "keine Url für DE" + SENDER_ARTE_DE + " " + urlFilm);
                    }
                }
            } else {
                Log.fehlerMeldung(-782268046, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme3-1", "keine Url " + SENDER_ARTE_DE + " " + urlFilm);
            }
            // ARTE.FR
            MUSTER_URL = MUSTER_URL_FR;
            if ((pos1 = strSeite2.indexOf(MUSTER_URL)) != -1) {
                pos1 += MUSTER_URL.length();
                if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                    url = strSeite2.substring(pos1, pos2);
                    if (!url.equals("")) {
                        //weiter gehts jetzt wirklich zum Film
                        addFilme4(SENDER_ARTE_FR, strUrlFeed, thema, titel, url, authurl);
                    } else {
                        Log.fehlerMeldung(-965884360, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme3-2", "keine Url für FR" + SENDER_ARTE_FR + " " + urlFilm);
                    }
                }
            } else {
                Log.fehlerMeldung(-693258440, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme3-2", "keine Url " + SENDER_ARTE_FR + " " + urlFilm);
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
            String name = "";
            final String MUSTER_DATUM = "<dateVideo>";
            final String MUSTER_URL = "<url quality=\"hd\">";
            final String MUSTER_NAME = "<name>";
            if (Daten.filmeLaden.getStop()) {
                return;
            }
            if (thema.startsWith(THEMA_ARTE_7)) {
                //strSeite3 = getUrl7.getUri_Utf(senderName, urlFilm, strSeite3, "");
                strSeite3 = getUrlIo.getUri(nameSenderMReader, urlFilm, Konstanten.KODIERUNG_UTF, 3 /* versuche */, strSeite3, "" /* Meldung */);
            } else {
                //strSeite3 = getUrl.getUri_Utf(senderName, urlFilm, strSeite3, "");
                strSeite3 = getUrlIo.getUri(nameSenderMReader, urlFilm, Konstanten.KODIERUNG_UTF, 2 /* versuche */, strSeite3, "" /* Meldung */);
            }
            int pos1;
            int pos2;
            String url;
            if ((pos1 = strSeite3.indexOf(MUSTER_NAME)) != -1) {
                pos1 += MUSTER_NAME.length();
                if ((pos2 = strSeite3.indexOf("<", pos1)) != -1) {
                    //<name>Au détour d'un chantier</name>
                    tmp = strSeite3.substring(pos1, pos2);
                    if (tmp.equals("")) {
                        Log.fehlerMeldung(-699834409, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme4-1", "keine Name: " + senderArte + " " + urlFilm);
                        name = titel;
                    } else {
                        name = tmp;
                    }
                }
            }
            if ((pos1 = strSeite3.indexOf(MUSTER_DATUM)) != -1) {
                pos1 += MUSTER_DATUM.length();
                if ((pos2 = strSeite3.indexOf("<", pos1)) != -1) {
                    //<dateVideo>Thu, 25 Nov 2010 18:06:32 +0100</dateVideo>
                    tmp = strSeite3.substring(pos1, pos2);
                    if (tmp.equals("")) {
                        Log.fehlerMeldung(-446983087, Log.FEHLER_ART_MREADER, "MediathekArte.addFilme4-1", "keine Datum: " + senderArte + " " + urlFilm);
                    } else {
                        datum = DatumZeit.convertDatum(tmp);
                        zeit = DatumZeit.convertTime(tmp);
                    }
                }
            }
            if ((pos1 = strSeite3.indexOf(MUSTER_URL)) != -1) {
                pos1 += MUSTER_URL.length();
                if ((pos2 = strSeite3.indexOf("</url>", pos1)) != -1) {
                    url = strSeite3.substring(pos1, pos2);
                    if (url.equals("")) {
                        Log.fehlerMeldung(-306921883, Log.FEHLER_ART_MREADER, "MediathekArte.addfilme4-2", "keine Url: " + senderArte + " " + urlFilm);
                    } else {
                        // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                        DatenFilm film = new DatenFilm(senderArte, thema, strUrlFeed, name, url, ""/* urlRtmp */, datum, zeit);
                        film.arr[DatenFilm.FILM_URL_AUTH_NR] = authurl;
                        addFilm(film);
                    }
                }
            }
        }
    }

    private synchronized int getAddThemenLaufen(int addThema) {
        themenLaufen += addThema;
        return themenLaufen;
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
}
