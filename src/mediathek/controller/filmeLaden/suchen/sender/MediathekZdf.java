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
public class MediathekZdf extends MediathekReader implements Runnable {

    public static final String SENDER = "ZDF";
    private StringBuffer seite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
    private final int ANZAHL_ZDF_ALLE = 500;
    private final int ANZAHL_ZDF_UPDATE = 20;
    private final int ANZAHL_ZDF_KURZ = 10;

    /**
     *
     * @param ddaten
     */
    public MediathekZdf(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, 8 /* threads */, 500 /* urlWarten */, startPrio);
    }

    /**
     *
     */
    @Override
    public void addToList() {
        listeThemen.clear();
        meldungStart();
        // Liste von http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-a-bis-z/saz0 bis sat8 holen
        String addr = "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-a-bis-z/saz";
        for (int i = 0; i <= 8; ++i) {
            addToList_addr(addr + String.valueOf(i), suchen.allesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE);
        }
        // Spartenkanäle einfügen
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1209122", suchen.allesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE); // zdf-neo
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1209120", suchen.allesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE); // zdf-info
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1317640", suchen.allesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE); // zdf-kultur
        //Rubriken einfügen
        addToList_Rubrik("http://www.zdf.de/ZDFmediathek/hauptnavigation/rubriken");
        // letzte Woche eingügen
        addToList_kurz();
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            //alles auswerten
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new ZdfThemaLaden()).start();
            }
        }
    }

    private void addToList_Rubrik(String addr) {
        final String MUSTER_URL = "<p><b><a href=\"/ZDFmediathek/kanaluebersicht/aktuellste/";
        //GetUrl(int ttimeout, long wwartenBasis) {
        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        StringBuffer seiteR = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        seiteR = getUrl.getUri(nameSenderMReader, addr, Konstanten.KODIERUNG_UTF, 6 /* versuche */, seiteR, "" /* Meldung */);
        if (seiteR.length() == 0) {
            Log.fehlerMeldung(-774200364, Log.FEHLER_ART_MREADER, "MediathekZdf.addToList_addr", "Leere Seite für URL: " + addr);
        }
        int pos = 0;
        int pos1;
        int pos2;
        int pos3;
        String url = "";
        while ((pos = seiteR.indexOf(MUSTER_URL, pos)) != -1) {
            pos += MUSTER_URL.length();
            pos1 = pos;
            pos2 = seiteR.indexOf("?", pos);
            pos3 = seiteR.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1 && pos3 != -1 && pos2 < pos3) {
                //pos2 > pos3 dann hat der Link kein ?
                url = seiteR.substring(pos1, pos2);
            }
            if (url.equals("")) {
                Log.fehlerMeldung(-754126900, Log.FEHLER_ART_MREADER, "MediathekZdf.addToList_addr", "keine URL: " + addr);
            } else {
                url = "http://www.zdf.de/ZDFmediathek/kanaluebersicht/aktuellste/" + url + "?bc=rub";
                addToList_addr(url, ANZAHL_ZDF_UPDATE); // immer nur eine "kurz"
            }
        }
    }

    private void addToList_addr(String addr, int anz) {
        final String MUSTER_URL = "<p><b><a href=\"/ZDFmediathek/kanaluebersicht/aktuellste/";
        //GetUrl(int ttimeout, long wwartenBasis) {
        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        seite = getUrl.getUri(nameSenderMReader, addr, Konstanten.KODIERUNG_UTF, 6 /* versuche */, seite, "" /* Meldung */);
        if (seite.length() == 0) {
            Log.fehlerMeldung(-596004563, Log.FEHLER_ART_MREADER, "MediathekZdf.addToList_addr", "Leere Seite für URL: " + addr);
        }
        int pos = 0;
        int pos1;
        int pos2;
        int pos3;
        String url = "";
        String urlThema;
        String thema = "";
        while ((pos = seite.indexOf(MUSTER_URL, pos)) != -1) {
            pos += MUSTER_URL.length();
            pos1 = pos;
            pos2 = seite.indexOf("?", pos);
            pos3 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1 && pos3 != -1 && pos2 < pos3) {
                //pos2 > pos3 dann hat der Link kein ?
                url = seite.substring(pos1, pos2);
            } else {
                pos2 = seite.indexOf("\"", pos);
                pos3 = seite.indexOf("<", pos);
                if (pos1 != -1 && pos2 != -1 && pos3 != -1 && pos2 < pos3) {
                    //pos2 > pos3 dann hat der Link kein ? zB bei "Rubiken"
                    url = seite.substring(pos1, pos2);
                }
            }
            pos1 = seite.indexOf("\">", pos);
            pos2 = seite.indexOf("<", pos);
            if (pos1 != -1 && pos2 != -1) {
                thema = seite.substring(pos1 + 2, pos2);
            }
            if (url.equals("")) {
                Log.fehlerMeldung(-946325890, Log.FEHLER_ART_MREADER, "MediathekZdf.addToList_addr", "keine URL: " + addr);
            } else {
                url = "http://www.zdf.de/ZDFmediathek/kanaluebersicht/aktuellste/" + url;
                urlThema = url;
                url += "?teaserListIndex=" + String.valueOf(anz);
                addThemenliste(url, urlThema, thema);
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

    private synchronized void addThemenliste(String url, String urlThema, String thema) {
        String[] add = new String[]{url, urlThema, thema};
        listeThemen.addUrl(add);
    }

    private class ZdfThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer seite2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public void run() {
            try {
                String link[];
                meldungAddThread();
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    seite1.setLength(0);
                    addFilme(link[0]/* url */, link[1]/* urlThema */, link[2]/* Thema */);
                    meldungProgress(link[0]);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-496583200, Log.FEHLER_ART_MREADER, "MediathekZdf.ZdfThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }

        private void addFilme(String url, String urlThema, String thema) {
            final String MUSTER_URL_1 = "<p><b><a href=\"/ZDFmediathek/beitrag/video/";
            String titel = "";
            String urlFilm = "";
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            int pos3 = 0;
            int anz = 0;
            try {
                //seite1 = getUrl.getUri(urlThema + "?bc=saz", seite1);
                seite1 = getUrl.getUri_Utf(nameSenderMReader, url, seite1, "Thema: " + thema);
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_URL_1, pos)) != -1) {
                    ++anz;
                    if (!suchen.allesLaden) {
                        if (anz > ANZAHL_ZDF_KURZ) {
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
                        urlFilm = seite1.substring(pos1, pos2);
                    } else {
                        urlFilm = seite1.substring(pos1, pos3);
                    }
                    pos1 = seite1.indexOf("\">", pos);
                    pos2 = seite1.indexOf("<", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        titel = seite1.substring(pos1 + 2, pos2);
                    }
                    if (urlFilm.equals("")) {
                        Log.fehlerMeldung(-643269690, Log.FEHLER_ART_MREADER, "MediathekZdf.addFilme", "keine URL: " + url);
                    } else {
                        urlFilm = "http://www.zdf.de/ZDFmediathek/beitrag/video/" + urlFilm;
                        filmHolen(thema, titel, urlThema, urlFilm);
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-796325800, Log.FEHLER_ART_MREADER, "MediathekZdf.addFilme", ex, url);
            }
        }

        private void filmHolen(String thema, String titel, String urlThema, String uurlFilm) {
            final String MUSTER_URL_1 = "<li>DSL 2000 <a href=\"http://wstreaming.zdf.de/zdf/veryhigh/";
            final String MUSTER_URL_2 = "<li>DSL 2000 <a href=\"http://wgeostreaming.zdf.de/zdf/veryhigh/";
            final String MUSTER_TITEL_1 = "<title>";
            final String MUSTER_TITEL_2 = "</title>";
            final String MUSTER_DATUM_1 = "<p class=\"datum\">";
            final String MUSTER_DATUM_2 = "</p>";
            String muster;
            String urlFilm = "";
            String datum = "";
            String zeit = "";
            int pos;
            int pos1;
            int pos2;
            try {
                meldung(uurlFilm);
                seite2 = getUrl.getUri_Utf(nameSenderMReader, uurlFilm, seite2, "urlThema: " + urlThema);
                if (titel.equals("")) {
                    //<title>Neu im Kino - &quot;Fair Game&quot; - ZDFneo - ZDFmediathek - ZDF Mediathek</title>
                    //<title>Trinkwasser aus dem Eisberg - Abenteuer Wissen - ZDFmediathek - ZDF Mediathek</title>
                    //<p class="datum">Abenteuer Wissen, 24.11.2010</p>

                    pos1 = seite2.indexOf(MUSTER_TITEL_1, 0) + MUSTER_TITEL_1.length();
                    pos2 = seite2.indexOf(MUSTER_TITEL_2, MUSTER_TITEL_1.length());
                    if (pos1 != -1 && pos2 != -1) {
                        titel = seite2.substring(pos1, pos2);
                        titel = titel.replace("", " - ZDFmediathek - ZDF Mediathek");
                        if (titel.contains("-")) {
                            titel = titel.substring(titel.lastIndexOf("-"));
                        }
                    }
                }
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
                        urlFilm = seite2.substring(pos1, pos2);
                    }
                    if (urlFilm.equals("")) {
                        Log.fehlerMeldung(-690048078, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen-1", "keine URL: " + urlFilm);
                    } else {
                        urlFilm = "http://wstreaming.zdf.de/zdf/veryhigh/" + urlFilm;
                        if (!urlFilm.endsWith("asx")) {
                            Log.fehlerMeldung(-200480752, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen-2", "keine URL: " + urlFilm);
                        } else {
                            //addFilm(new DatenFilm(nameSenderMReader, thema, urlThema, titel, url, url/* urlOrg */, ""/* urlRtmp */, datum, zeit));
                            flashHolen(thema, titel, urlThema, urlFilm, datum, zeit);
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-860248073, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen", ex, urlFilm);
            }
        }

        private void flashHolen(String thema, String titel, String urlThema, String urlFilm, String datum, String zeit) {
            meldung(urlFilm);
            DatenFilm f = MediathekZdf.flash(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit);
            if (f != null) {
                addFilm(f);
            }
        }

        private synchronized String[] getListeThemen() {
            return listeThemen.pollFirst();
        }
    }

    public static DatenFilm flash(GetUrl getUrl, StringBuffer seiteFlash, String senderName, String thema, String titel, String urlThema, String urlFilm, String datum, String zeit) {
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
        DatenFilm ret = null;
        final String MUSTER_HOST = "<param name=\"host\" value=\"";
        final String MUSTER_APP = "<param name=\"app\" value=\"";
        final String MUSTER_URL = "src=\"";
        final String MUSTER_URL_L = "l.mp4";
        final String MUSTER_URL_H = "h.mp4";
        final String MUSTER_URL_VH = "vh.mp4";
        String orgUrl = urlFilm;
        String host = "";
        String app = "";
        String url = "", tmpUrl = "";
        int pos1;
        int pos2;
        try {
            orgUrl = orgUrl.replace("http://wstreaming.zdf.de", "http://fstreaming.zdf.de");
            orgUrl = orgUrl.replace("http://wgeostreaming.zdf.de", "http://fgeostreaming.zdf.de");
            orgUrl = orgUrl.replace(".asx", ".smil");
            seiteFlash = getUrl.getUri_Utf(senderName, orgUrl, seiteFlash, "urlThema: " + urlThema);
            String strSeiteFlash = seiteFlash.toString();
            if ((pos1 = strSeiteFlash.indexOf(MUSTER_HOST, 0)) != -1) {
                pos1 += MUSTER_HOST.length();
                if ((pos2 = strSeiteFlash.indexOf("\"", pos1)) != -1) {
                    host = strSeiteFlash.substring(pos1, pos2);
                }
            }
            if ((pos1 = strSeiteFlash.indexOf(MUSTER_APP, 0)) != -1) {
                pos1 += MUSTER_APP.length();
                if ((pos2 = strSeiteFlash.indexOf("\"", pos1)) != -1) {
                    app = strSeiteFlash.substring(pos1, pos2);
                }
            }
            pos1 = 0;
            boolean gefunden = false;
            while ((pos1 = strSeiteFlash.indexOf(MUSTER_URL, pos1)) != -1) {
                pos1 += MUSTER_URL.length();
                if ((pos2 = strSeiteFlash.indexOf("\"", pos1)) != -1) {
                    tmpUrl = strSeiteFlash.substring(pos1, pos2);
                }
                if (url.equals("")) {
                    url = tmpUrl;
                }
                if (!url.contains(MUSTER_URL_VH) && tmpUrl.contains(MUSTER_URL_H)) {
                    url = tmpUrl;
                    gefunden = true;
                }
                if (tmpUrl.contains(MUSTER_URL_VH)) {
                    url = tmpUrl;
                    gefunden = true;
                }
            }
            if (!gefunden) {
                //<video dur="00:08:02" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/09/120919_westerwelle_mom_51k_p7v9.mp4" system-bitrate="62000">
                //<param name="quality" value="low" />
                //</video>
                //<video dur="00:08:02" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/09/120919_westerwelle_mom_536k_p9v9.mp4" system-bitrate="700000">
                //<param name="quality" value="high" />
                //</video>
                //<video dur="00:08:02" paramGroup="gl-vod-rtmp" src="mp4:zdf/12/09/120919_westerwelle_mom_1596k_p13v9.mp4" system-bitrate="1700000">
                //<param name="quality" value="veryhigh" />
                //</video>
                pos1 = 0;
                while ((pos1 = strSeiteFlash.indexOf(MUSTER_URL, pos1)) != -1) {
                    int max = 0;
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = strSeiteFlash.indexOf("\"", pos1)) != -1) {
                        tmpUrl = strSeiteFlash.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        url = tmpUrl;
                    }
                    if (tmpUrl.contains("k_")) {
                        String tmp = tmpUrl.substring(0, tmpUrl.lastIndexOf("k_"));
                        if (tmp.contains("_")) {
                            tmp = tmp.substring(tmp.lastIndexOf("_") + 1);
                            try {
                                int i = Integer.parseInt(tmp);
                                if (i > max) {
                                    max = i;
                                    url = tmpUrl;
                                    gefunden = true;
                                }
                            } catch (Exception e) {
                            }
                        }
                    }
                }
            }
            if (!gefunden) {
                Log.fehlerMeldung(-302125078, Log.FEHLER_ART_MREADER, "MediathekZdf.flash-1 " + senderName, "!gefunden: " + urlFilm);
            }
            if (url.equals("")) {
                ret = null;
                Log.fehlerMeldung(-783012580, Log.FEHLER_ART_MREADER, "MediathekZdf.flash-2 " + senderName, "keine URL: " + urlFilm);
            } else if (host.equals("")) {
                ret = null;
                Log.fehlerMeldung(-356047809, Log.FEHLER_ART_MREADER, "MediathekZdf.flash-3 " + senderName, "kein Host: " + urlFilm);
            } else {
                url = "rtmpt://" + host + "/" + app + "/" + url;
                ret = new DatenFilm(senderName, thema, urlThema, titel, url, ""/* urlRtmp */, datum, zeit);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(-265847128, Log.FEHLER_ART_MREADER, "MediathekZdf.flash" + senderName, ex, urlFilm);
        }
        return ret;
    }
}
