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
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.filmeSuchenSender.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;

/**
 *
 * @author
 */
public class MediathekZdf extends MediathekReader implements Runnable {

    public static final String SENDER = "ZDF";
    private StringBuffer seite = new StringBuffer();
    private final int ANZAHL_ZDF_ALLE = 500;
    private final int ANZAHL_ZDF_UPDATE = 20;

    /**
     *
     * @param ddaten
     */
    public MediathekZdf(FilmeSuchenSender ssearch) {
        super(ssearch, /* name */ SENDER,  4 /* threads */, 500 /* urlWarten */);
    }

    /**
     *
     */
    @Override
    public void addToList() {
        listeThemen.clear();
        //Liste von http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-a-bis-z/saz0 bis sat8 holen
        String addr = "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-a-bis-z/saz";
        for (int i = 0; i <= 8; ++i) {
            addToList_addr(addr + String.valueOf(i));
        }
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1209122"); // zdf-neo
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1209120"); // zdf-info
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1317640"); // zdf-kultur
        addToList_kurz();
        if (!Daten.filmeLaden.getStop() && listeThemen.size() > 0) {
            meldungStart(listeThemen.size());
            //alles auswerten
            for (int t = 0; t < senderMaxThread; ++t) {
                new Thread(new ZdfThemaLaden()).start();
            }
        }
    }

    private void addToList_addr(String addr) {
        final String MUSTER_URL = "<p><b><a href=\"/ZDFmediathek/kanaluebersicht/aktuellste/";
        //GetUrl(int ttimeout, long wwartenBasis) {
        GetUrl getUrl = new GetUrl(5000, senderWartenSeiteLaden);
        seite = getUrl.getUri(nameSenderMReader, addr, Konstanten.KODIERUNG_UTF, 2000 /* timeout */, 6 /* versuche */, seite, "" /* Meldung */);
        if (seite.length() == 0) {
            Log.fehlerMeldung(-596004563,"MediathekZdf.addToList_addr", "Leere Seite für URL: " + addr);
        }
        int pos = 0;
        int pos1;
        int pos2;
        int pos3;
        String url = "";
        String urlorg;
        String thema = "";
        while ((pos = seite.indexOf(MUSTER_URL, pos)) != -1) {
            pos += MUSTER_URL.length();
            pos1 = pos;
            pos2 = seite.indexOf("?", pos);
            pos3 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1 && pos3 != -1 && pos2 < pos3) {
                //pos2 > pos3 dann hat der Link kein ?
                url = seite.substring(pos1, pos2);
            }
            pos1 = seite.indexOf("\">", pos);
            pos2 = seite.indexOf("<", pos);
            if (pos1 != -1 && pos2 != -1) {
                thema = seite.substring(pos1 + 2, pos2);
            }
            if (url.equals("")) {
                Log.fehlerMeldung(-946325890,"MediathekZdf.addToList_addr", "keine URL: " + addr);
            } else {
                url = "http://www.zdf.de/ZDFmediathek/kanaluebersicht/aktuellste/" + url;
                urlorg = url;
                if (suchen.allesLaden) {
                    url += "?teaserListIndex=" + ANZAHL_ZDF_ALLE;
                } else {
                    url += "?teaserListIndex=" + ANZAHL_ZDF_UPDATE;
                }
                addThemenliste(urlorg, url, thema);
            }
        }
    }

    private void addToList_kurz() {
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day0", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day0", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day1", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day1", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day2", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day2", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day3", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day3", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day4", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day4", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day5", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day5", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day6", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day6", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day7", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day7", "");
    }

    private synchronized void addThemenliste(String urlorg, String url, String thema) {
        String[] add = new String[]{urlorg, url, thema};
        if (!istInListe(listeThemen, url, 0)) {
            listeThemen.add(add);
        }
    }

    private class ZdfThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(senderWartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();
        private StringBuffer seite2 = new StringBuffer();

        @Override
        public void run() {
            try {
                String link[];
                meldungAddThread();
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    seite1.setLength(0);
                    addFilme(link[2]/* Thema */, link[1]/* url */, link[0]/* urlorg */);
                    meldungProgress(link[0]);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung(-496583200,"MediathekZdf.ZdfThemaLaden.run", ex);
            }
        }

        private void addFilme(String thema, String urlThema, String urlorg) {
            final String MUSTER_URL_1 = "<p><b><a href=\"/ZDFmediathek/beitrag/video/";
            String titel = "";
            String url = "";
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            int pos3 = 0;
            int anz = 0;
            try {
                //seite1 = getUrl.getUri(urlThema + "?bc=saz", seite1);
                seite1 = getUrl.getUri_Utf(nameSenderMReader, urlThema, seite1, "Thema: " + thema);
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_URL_1, pos)) != -1) {
                    ++anz;
                    if (!suchen.allesLaden) {
                        if (anz > ANZAHL_ZDF_UPDATE) {
                            // dann reichts
                            break;
                        }
                    }
                    pos += MUSTER_URL_1.length();
                    pos1 = pos;
                    pos2 = seite1.indexOf("?", pos);
                    pos3 = seite1.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1 && pos3 != -1 && pos2 < pos3) {
                        //pos2 > pos3 dann hat der Link kein ?
                        url = seite1.substring(pos1, pos2);
                    } else {
                        url = seite1.substring(pos1, pos3);
                    }
                    pos1 = seite1.indexOf("\">", pos);
                    pos2 = seite1.indexOf("<", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        titel = seite1.substring(pos1 + 2, pos2);
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldung(-643269690,"MediathekZdf.addFilme", "keine URL: " + urlThema);
                    } else {
                        url = "http://www.zdf.de/ZDFmediathek/beitrag/video/" + url;
                        filmHolen(thema, titel, urlorg, url);
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-796325800,"MediathekZdf.addFilme", ex, urlThema);
            }
        }

        private void filmHolen(String thema, String titel, String urlThema, String urlFilm) {
            final String MUSTER_URL_1 = "<li>DSL 2000 <a href=\"http://wstreaming.zdf.de/zdf/veryhigh/";
            final String MUSTER_URL_2 = "<li>DSL 2000 <a href=\"http://wgeostreaming.zdf.de/zdf/veryhigh/";
            final String MUSTER_TITEL_1 = "<title>";
            final String MUSTER_TITEL_2 = "</title>";
            final String MUSTER_DATUM_1 = "<p class=\"datum\">";
            final String MUSTER_DATUM_2 = "</p>";
            String muster = "";
            String url = "";
            String datum = "";
            String zeit = "";
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            try {
                meldung("*" + urlFilm);
                seite2 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite2, "urlThema: " + urlThema);
                if (titel.equals("")) {
                    //<title>Neu im Kino - &quot;Fair Game&quot; - ZDFneo - ZDFmediathek - ZDF Mediathek</title>
                    //<title>Trinkwasser aus dem Eisberg - Abenteuer Wissen - ZDFmediathek - ZDF Mediathek</title>
                    //<p class="datum">Abenteuer Wissen, 24.11.2010</p>

                    pos1 = seite2.indexOf(MUSTER_TITEL_1, 0) + MUSTER_TITEL_1.length();
                    pos2 = seite2.indexOf(MUSTER_TITEL_2, MUSTER_TITEL_1.length());
                    if (pos1 != -1 && pos2 != -1) {
                        titel = seite2.substring(pos1, pos2);
                        titel.replace("", " - ZDFmediathek - ZDF Mediathek");
                        if (titel.contains("-")) {
                            titel = titel.substring(titel.lastIndexOf("-"));
                        }
                    }
                }
                pos = 0;
                pos1 = 0;
                pos2 = 0;
                if ((pos1 = seite2.indexOf(MUSTER_DATUM_1, 0)) != -1) {
                    if ((pos2 = seite2.indexOf(MUSTER_DATUM_2, pos1)) != -1) {
                        pos1 += MUSTER_DATUM_1.length();
                        datum = seite2.substring(pos1, pos2);
                        if (datum.contains(",")) {
                            if (thema.equals("")) {
                                thema = datum.substring(0, datum.lastIndexOf(","));
                            }
                            datum = datum.substring(datum.lastIndexOf(",") + 1).trim();
                            if (datum.contains(" ")) {
                                zeit = datum.substring(datum.lastIndexOf(" ")).trim() + ":00";
                                datum = datum.substring(0, datum.lastIndexOf(" ")).trim();
                            }
                        }
                    }
                }
                pos = 0;
                pos1 = 0;
                pos2 = 0;
                if (seite2.indexOf(MUSTER_URL_1) != -1) {
                    muster = MUSTER_URL_1;
                } else {
                    muster = MUSTER_URL_2;
                }
                if (!Daten.filmeLaden.getStop() && (pos = seite2.indexOf(muster, pos)) != -1) {
                    pos += muster.length();
                    pos1 = pos;
                    pos2 = seite2.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite2.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldung(-690048078,"MediathekZdf.filmHolen-1", "keine URL: " + urlFilm);
                    } else {
                        url = "http://wstreaming.zdf.de/zdf/veryhigh/" + url;
                        if (!url.endsWith("asx")) {
                            Log.fehlerMeldung(-200480752,"MediathekZdf.filmHolen-2", "keine URL: " + urlFilm);
                        } else {
//                            if (thema.equals("Terra X")) { //bisher einziges Thema
//                                if (Funktionen.urlExists(url.replace("/veryhigh/", "/hd/"))) {
//                                    urlHd = url.replace("/veryhigh/", "/hd/");
//                                }
//                            }
                            //DatenFilm(ddaten, ssender, tthema, urlThema, ttitel, uurl, uurlorg, zziel)
//                            daten.filmeLaden.listeFilmeSchattenliste.addSenderRtmp(new DatenFilm(daten, Konstanten.SENDER_ZDF,
//                                    thema, urlThema, titel,
//                                    leitungAendern__(daten, url, urlHd), url/*urlOrg*/, ""/*urlRtmp*/, urlHd, alt));
                            addFilm(new DatenFilm(nameSenderMReader,
                                    thema, urlThema, titel, url, url/* urlOrg */, ""/* urlRtmp */, datum, zeit));
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-860248073,"MediathekZdf.filmHolen", ex, urlFilm);
            }
        }

        private synchronized String[] getListeThemen() {
            return listeThemen.pollFirst();
        }
    }
}
