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
import static mediathek.controller.filmeLaden.suchen.sender.MediathekReader.extractDuration;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.MVStringBuilder;

public class MediathekZdf extends MediathekReader implements Runnable {

    public static final String SENDER = "ZDF";
    private MVStringBuilder seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
    private final int ANZAHL_ZDF_ALLE = 500;
    private final int ANZAHL_ZDF_MITTEL = 50;
    private final int ANZAHL_ZDF_UPDATE = 20;
    private final int ANZAHL_ZDF_KURZ = 10;

    public MediathekZdf(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, 8 /* threads */, 500 /* urlWarten */, startPrio);
    }

    @Override
    public void addToList() {
        listeThemen.clear();
        meldungStart();
        // Liste von http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-a-bis-z/saz0 bis sat8 holen
        String addr = "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-a-bis-z/saz";
        for (int i = 0; i <= 8; ++i) {
            addToList_addr(addr + String.valueOf(i), filmeSuchenSender.senderAllesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE);
        }
        // Spartenkanäle einfügen
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1209122", filmeSuchenSender.senderAllesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE); // zdf-neo
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1209120", filmeSuchenSender.senderAllesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE); // zdf-info
        addToList_addr("http://www.zdf.de/ZDFmediathek/senderstartseite/sst1/1317640", filmeSuchenSender.senderAllesLaden ? ANZAHL_ZDF_ALLE : ANZAHL_ZDF_UPDATE); // zdf-kultur
        //Rubriken einfügen
        if (filmeSuchenSender.senderAllesLaden) {
            // da sollte eigentlich nichts Neues sein
            addToList_Rubrik("http://www.zdf.de/ZDFmediathek/hauptnavigation/rubriken");
        }
        // letzte Woche einfügen
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day0", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day0", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day1", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day1", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day2", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day2", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day3", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day3", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day4", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day4", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day5", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day5", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day6", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day6", "");
        addThemenliste("http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day7", "http://www.zdf.de/ZDFmediathek/hauptnavigation/sendung-verpasst/day7", "");
        // Spartenkanäle Übersicht
        if (filmeSuchenSender.senderAllesLaden) {
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/1209114", "http://www.zdf.de/ZDFmediathek/senderstartseite/1209114", ""); // ZDF
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/sst0/1209122?teaserListIndex=" + ANZAHL_ZDF_MITTEL,
                    "http://www.zdf.de/ZDFmediathek/senderstartseite/sst0/1209122?teaserListIndex=" + ANZAHL_ZDF_MITTEL, ""); // ZDF Neo
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/sst0/1317640?teaserListIndex=" + ANZAHL_ZDF_MITTEL,
                    "http://www.zdf.de/ZDFmediathek/senderstartseite/sst0/1317640?teaserListIndex=" + ANZAHL_ZDF_MITTEL, ""); // ZDF.kultur
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/sst0/1209120?teaserListIndex=" + ANZAHL_ZDF_MITTEL,
                    "http://www.zdf.de/ZDFmediathek/senderstartseite/sst0/1209120?teaserListIndex=" + ANZAHL_ZDF_MITTEL, ""); // ZDFinfo
        } else {
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/1209114", "http://www.zdf.de/ZDFmediathek/senderstartseite/1209114", ""); // ZDF
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/1209122", "http://www.zdf.de/ZDFmediathek/senderstartseite/1209122", ""); // ZDF Neo
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/1317640", "http://www.zdf.de/ZDFmediathek/senderstartseite/1317640", ""); // ZDF.kultur
            addThemenliste("http://www.zdf.de/ZDFmediathek/senderstartseite/1209120", "http://www.zdf.de/ZDFmediathek/senderstartseite/1209120", ""); // ZDFinfo
        }
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            //alles auswerten
            for (int t = 0; t < maxThreadLaufen; ++t) {
                //new Thread(new ThemaLaden()).start();
                Thread th = new Thread(new ThemaLaden());
                th.setName(nameSenderMReader + t);
                th.start();
            }
        }
    }

    private void addToList_Rubrik(String addr) {
        final String MUSTER_URL = "<p><b><a href=\"/ZDFmediathek/kanaluebersicht/aktuellste/";
        //GetUrl(int ttimeout, long wwartenBasis) {
        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        MVStringBuilder seiteR = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
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

    private synchronized void addThemenliste(String url, String urlThema, String thema) {
        String[] add = new String[]{url, urlThema, thema};
        listeThemen.addUrl(add);
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private MVStringBuilder seite1 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        private MVStringBuilder seite2 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

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
            boolean ok = false;
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            int pos3 = 0;
            int anz = 0;
            try {
                //seite1 = getUrl.getUri(urlThema + "?bc=saz", seite1);
                seite1 = getUrl.getUri_Utf(nameSenderMReader, url, seite1, "Thema: " + thema);
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_URL_1, pos)) != -1) {
                    ok = false;
                    ++anz;
                    if (!filmeSuchenSender.senderAllesLaden) {
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
                    if (urlFilm.isEmpty()) {
                        Log.fehlerMeldung(-643269690, Log.FEHLER_ART_MREADER, "MediathekZdf.addFilme", "keine URL: " + url);
                    } else {
                        // über die ID versuchen
                        urlFilm = "http://www.zdf.de/ZDFmediathek/beitrag/video/" + urlFilm;
                        String id = "";
                        if ((pos1 = urlFilm.indexOf("/ZDFmediathek/beitrag/video/")) != -1) {
                            pos1 += "/ZDFmediathek/beitrag/video/".length();
                            if ((pos2 = urlFilm.indexOf("/", pos1)) != -1) {
                                id = urlFilm.substring(pos1, pos2);
                                // System.out.println(id);
                            }
                        }
                        if (!id.isEmpty()) {
                            id = "http://www.zdf.de/ZDFmediathek/xmlservice/web/beitragsDetails?ak=web&id=" + id;
                            DatenFilm film = filmHolenId(getUrl, seite2, nameSenderMReader, thema, titel, urlFilm, id);
                            if (film != null) {
                                // dann wars gut
                                addFilm(film);
                                ok = true;
                            }
                        }
                        if (!ok) {
                            // dann mit der herkömmlichen Methode versuchen
                            Log.fehlerMeldung(-398012379, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen", "auf die alte Art: " + urlFilm);
//                            filmHolen(thema, titel, urlThema, urlFilm);
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-796325800, Log.FEHLER_ART_MREADER, "MediathekZdf.addFilme", ex, url);
            }
        }

//        private void filmHolen(String thema, String titel, String urlThema, String filmWebsite) {
//            final String MUSTER_URL_1 = "<li>DSL 2000 <a href=\"http://wstreaming.zdf.de/zdf/veryhigh/";
//            final String MUSTER_URL_2 = "<li>DSL 2000 <a href=\"http://wgeostreaming.zdf.de/zdf/veryhigh/";
//            final String MUSTER_URL_QUICKTIME_1 = "<li>DSL 2000 <a href=\"http://hstreaming.zdf.de/zdf/veryhigh/";
//            final String MUSTER_URL_QUICKTIME_2 = "<li>DSL 2000 <a href=\"http://hgeostreaming.zdf.de/zdf/veryhigh/";
//            // <li>DSL 2000 <a href="http://hgeostreaming.zdf.de/zdf/veryhigh/13/06/130617_aidaverona_miz.mov" class="play" target="_blank">Abspielen</a></li>
//            // <li>DSL 2000 <a href="http://hstreaming.zdf.de/zdf/veryhigh/13/06/130612_hunde4_dok.mov" class="play" target="_blank">Abspielen</a></li>
//            final String MUSTER_TITEL_1 = "<title>";
//            final String MUSTER_TITEL_2 = "</title>";
//            final String MUSTER_DATUM_1 = "<p class=\"datum\">";
//            final String MUSTER_DATUM_2 = "</p>";
//            String urlFilm = "";
//            String datum = "";
//            String zeit = "";
//            int pos;
//            int pos1;
//            int pos2;
//            try {
//                meldung(filmWebsite);
//                seite2 = getUrl.getUri_Utf(nameSenderMReader, filmWebsite, seite2, "urlThema: " + urlThema);
//                long durationInSeconds = extractDurationZDF(seite2);
//                String description = extractDescription(seite2);
//                String[] keywords = extractKeywords(seite2);
//                String imageUrl = extractImageURL(seite2);
//                if (titel.equals("")) {
//                    //<title>Neu im Kino - &quot;Fair Game&quot; - ZDFneo - ZDFmediathek - ZDF Mediathek</title>
//                    //<title>Trinkwasser aus dem Eisberg - Abenteuer Wissen - ZDFmediathek - ZDF Mediathek</title>
//                    //<p class="datum">Abenteuer Wissen, 24.11.2010</p>
//                    pos1 = seite2.indexOf(MUSTER_TITEL_1, 0) + MUSTER_TITEL_1.length();
//                    pos2 = seite2.indexOf(MUSTER_TITEL_2, MUSTER_TITEL_1.length());
//                    if (pos1 != -1 && pos2 != -1) {
//                        titel = seite2.substring(pos1, pos2);
//                        titel = titel.replace("", " - ZDFmediathek - ZDF Mediathek");
//                        if (titel.contains("-")) {
//                            titel = titel.substring(titel.lastIndexOf("-"));
//                        }
//                    }
//                }
//                if ((pos1 = seite2.indexOf(MUSTER_DATUM_1, 0)) != -1) {
//                    if ((pos2 = seite2.indexOf(MUSTER_DATUM_2, pos1)) != -1) {
//                        pos1 += MUSTER_DATUM_1.length();
//                        datum = seite2.substring(pos1, pos2);
//                        if (datum.contains(",")) {
//                            if (thema.equals("")) {
//                                thema = datum.substring(0, datum.lastIndexOf(","));
//                            }
//                            datum = datum.substring(datum.lastIndexOf(",") + 1).trim();
//                            if (datum.contains(" ")) {
//                                zeit = datum.substring(datum.lastIndexOf(" ")).trim() + ":00";
//                                datum = datum.substring(0, datum.lastIndexOf(" ")).trim();
//                            }
//                        }
//                    }
//                }
//                pos = 0;
//                String muster = "";
//                String addUrl = "";
//                if (seite2.indexOf(MUSTER_URL_1) != -1) {
//                    muster = MUSTER_URL_1;
//                    addUrl = "http://wstreaming.zdf.de/zdf/veryhigh/";
//                } else if (seite2.indexOf(MUSTER_URL_2) != -1) {
//                    muster = MUSTER_URL_2;
//                    addUrl = "http://wgeostreaming.zdf.de/zdf/veryhigh/";
//                } else if (seite2.indexOf(MUSTER_URL_QUICKTIME_1) != -1) {
//                    muster = MUSTER_URL_QUICKTIME_1;
//                    addUrl = "http://hstreaming.zdf.de/zdf/veryhigh/";
//                } else if (seite2.indexOf(MUSTER_URL_QUICKTIME_2) != -1) {
//                    muster = MUSTER_URL_QUICKTIME_2;
//                    addUrl = "http://hgeostreaming.zdf.de/zdf/veryhigh/";
//                }
//                if (!muster.isEmpty() && !Daten.filmeLaden.getStop() && (pos = seite2.indexOf(muster, pos)) != -1) {
//                    pos += muster.length();
//                    pos1 = pos;
//                    pos2 = seite2.indexOf("\"", pos);
//                    if (pos1 != -1 && pos2 != -1) {
//                        urlFilm = seite2.substring(pos1, pos2);
//                    }
//                }
//                if (urlFilm.equals("")) {
//                    Log.fehlerMeldung(-690048078, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen-1", "keine URL: " + urlFilm);
//                } else {
//                    urlFilm = addUrl + urlFilm;
//                    if (urlFilm.endsWith("asx")) {
//                        //addFilm(new DatenFilm(nameSenderMReader, thema, urlThema, titel, url, url/* urlOrg */, ""/* urlRtmp */, datum, zeit));
//                        //flashHolen(thema, titel, urlThema, urlFilm, datum, zeit);
//                        flashHolen(thema, titel, filmWebsite, urlFilm, datum, zeit, durationInSeconds, description, imageUrl, keywords);
//                    } else if (urlFilm.endsWith("mov")) {
//                        quicktimeHolen(thema, titel, filmWebsite, urlFilm, datum, zeit, durationInSeconds, description, imageUrl);
//                    } else {
//                        Log.fehlerMeldung(-200480752, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen-2", "keine URL: " + urlFilm);
//                    }
//                }
//            } catch (Exception ex) {
//                Log.fehlerMeldung(-860248073, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen", ex, urlFilm);
//            }
//        }
//
//        //private void flashHolen(String thema, String titel, String urlThema, String urlFilm, String datum, String zeit) {
//        private void flashHolen(String thema, String titel, String filmWebsite, String urlFilm, String datum, String zeit, long durationInSeconds, String description, String imageUrl, String[] keywords) {
////            meldung(urlFilm);
//            //DatenFilm f = MediathekZdf.flash(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit);
//            DatenFilm f = MediathekZdf.flash(getUrl, seite2, nameSenderMReader, thema, titel, filmWebsite, urlFilm, datum, zeit, durationInSeconds, description, imageUrl, keywords);
//            if (f != null) {
//                addFilm(f);
//            }
//        }
//
//        private void quicktimeHolen(String thema, String titel, String urlThema, String urlFilm, String datum, String zeit, long durationInSeconds, String description, String imageUrl) {
////            meldung(urlFilm);
//            //DatenFilm f = MediathekZdf.flash(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit);
//            DatenFilm f = quicktime(getUrl, seite2, nameSenderMReader, thema, titel, urlThema, urlFilm, datum, zeit, durationInSeconds, description, "", imageUrl, new String[]{});
//            if (f != null) {
//                addFilm(f);
//            }
//        }
        private synchronized String[] getListeThemen() {
            return listeThemen.pollFirst();
        }

        private long extractDurationZDF(MVStringBuilder page) {
            long durationInSeconds = 0;
            String duration = extractString(page, "<p class=\"datum\">VIDEO, ", "</p>");
            if (duration == null) {
                return 0;
            }
            try {
                String[] parts = duration.split(":");
                long power = 1;
                for (int i = parts.length - 1; i >= 0; i--) {
                    durationInSeconds += Long.parseLong(parts[i]) * power;
                    power *= 60;
                }
            } catch (Exception ex) {
                return 0;
            }
            return durationInSeconds;
        }

        private String extractDescription(MVStringBuilder page) {
            String desc = extractString(page, "<meta name=\"description\" content=\"", "\"");
            if (desc == null) {
                return "";
            }

            return desc;
        }

        private String[] extractKeywords(MVStringBuilder page) {
            String keywords = extractString(page, "<meta name=\"keywords\" content=\"", "\"");
            if (keywords == null) {
                return new String[]{""};
            }

            return keywords.split("; ");
        }

        private String extractImageURL(MVStringBuilder page) {
            String imageUrl = extractString(page, "background-image: url(/ZDFmediathek", ")");
            if (imageUrl == null) {
                return "";
            }

            return "http://www.zdf.de/ZDFmediathek" + imageUrl;
        }

        private String extractString(MVStringBuilder source, String startMarker, String endMarker) {
            int start = source.indexOf(startMarker);
            if (start == -1) {
                return null;
            }

            start = start + startMarker.length();

            int end = source.indexOf(endMarker, start);
            if (end == -1) {
                return null;
            }

            return source.substring(start, end);
        }
    }

    //public static DatenFilm flash(GetUrl getUrl, MVStringBuilder seiteFlash, String senderName, String thema, String titel, String urlThema, String urlFilm, String datum, String zeit) {
    public static DatenFilm flash(GetUrl getUrl, MVStringBuilder seiteFlash, String senderName, String thema, String titel,
            String filmWebsite, String urlFilm, String datum, String zeit, long durationInSeconds, String description,
            String imageUrl, String[] keywords) {
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
            seiteFlash = getUrl.getUri_Utf(senderName, orgUrl, seiteFlash, "filmWebsite: " + filmWebsite);
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
                //ret = new DatenFilm(senderName, thema, urlThema, titel, url, ""/* urlRtmp */, datum, zeit);
                ret = new DatenFilm(senderName, thema, filmWebsite, titel, url, ""/* urlRtmp */, datum, zeit, durationInSeconds, description,
                        imageUrl, keywords);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(-265847128, Log.FEHLER_ART_MREADER, "MediathekZdf.flash" + senderName, ex, urlFilm);
        }
        return ret;
    }

    public static DatenFilm quicktime(GetUrl getUrl, MVStringBuilder seiteQuicktime, String senderName, String thema, String titel, String filmWebsite, String urlFilm, String datum, String zeit, long durationInSeconds, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
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
                    ret = new DatenFilm(senderName, thema, filmWebsite, titel, url, ""/* urlRtmp */, datum, zeit, durationInSeconds, description,
                            imageUrl.isEmpty() ? thumbnailUrl : imageUrl, keywords);
                } else {
                    Log.fehlerMeldung(-918596307, Log.FEHLER_ART_MREADER, "Mediathek3sat.quicktime", "url passt nicht: " + urlFilm);
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(-265847128, Log.FEHLER_ART_MREADER, "MediathekZdf.flash" + senderName, ex, urlFilm);
        }
        return ret;
    }

    public static DatenFilm filmHolenId(GetUrl getUrl, MVStringBuilder strBuffer, String sender, String thema, String titel, String filmWebsite, String urlId) {
        //<teaserimage alt="Harald Lesch im Studio von Abenteuer Forschung" key="298x168">http://www.zdf.de/ZDFmediathek/contentblob/1909108/timg298x168blob/8081564</teaserimage>
        //<detail>Möchten Sie wissen, was Sie in der nächsten Sendung von Abenteuer Forschung erwartet? Harald Lesch informiert Sie.</detail>
        //<length>00:00:34.000</length>
        //<airtime>02.07.2013 23:00</airtime>
        final String BILD = "<teaserimage";
        final String BILD_ = "key=\"2";
        final String BESCHREIBUNG = "<detail>";
        final String LAENGE = "<length>";
        final String DATUM = "<airtime>";
        final String THEMA = "<originChannelTitle>";
        int pos1, pos2;
        String bild = "", beschreibung = "", laenge = "", datum = "", zeit = "", url = "", urlKlein = "", urlHd = "", urlF4m = "";
        strBuffer = getUrl.getUri_Utf(sender, urlId, strBuffer, "url: " + filmWebsite);
        if (strBuffer.length() == 0) {
            Log.fehlerMeldung(-398745601, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen", "url: " + urlId);
            return null;
        }
        bild = strBuffer.extract(BILD, BILD_, "<");
        if (bild.contains(">")) {
            bild = bild.substring(bild.indexOf(">") + 1);
        }
        beschreibung = strBuffer.extract(BESCHREIBUNG, "<");
        if (thema.isEmpty()) {
            thema = strBuffer.extract(THEMA, "<");
        }
        laenge = strBuffer.extract(LAENGE, "<");
        if (laenge.contains(".")) {
            laenge = laenge.substring(0, laenge.indexOf("."));
        }
        datum = strBuffer.extract(DATUM, "<");
        if (datum.contains(" ")) {
            zeit = datum.substring(datum.lastIndexOf(" ")).trim() + ":00";
            datum = datum.substring(0, datum.lastIndexOf(" ")).trim();
        }
        // erst mal URL in besserer Auflösung
        // <formitaet basetype="h264_aac_f4f_http_f4m_http" isDownload="false">
        // <quality>high</quality>
        // <url>http://fstreaming.zdf.de/3sat/300/13/07/130714_zkm_bonus_rundgang_museumscheck.f4m</url>
        // wenns das gibt --> bessere Auflösung
        final String QUALITAET = "<quality>high</quality>";
        final String URL_F4M_ANFANG = "<formitaet basetype=\"h264_aac_f4f_http_f4m_http\"";
        final String URL_F4M_ENDE = "</formitaet>";
        final String URL_F4M = "<url>";
        final String URL_ANFANG = "<formitaet basetype=\"h264_aac_mp4_http_na_na\"";
        final String URL_ANFANG_HD = "<formitaet basetype=\"wmv3_wma9_asf_mms_asx_http\"";
        final String URL_ENDE = "</formitaet>";
        final String URL = "<url>";
        int posAnfang = 0, posEnde = 0;
        posAnfang = 0;
        posEnde = 0;
        while (true) {
            if ((posAnfang = strBuffer.indexOf(URL_F4M_ANFANG, posAnfang)) == -1) {
                break;
            }
            posAnfang += URL_F4M_ANFANG.length();
            if ((posEnde = strBuffer.indexOf(URL_F4M_ENDE, posAnfang)) == -1) {
                break;
            }
            if ((pos1 = strBuffer.indexOf(QUALITAET, posAnfang)) != -1) {
                if (pos1 < posEnde) {
                    if ((pos1 = strBuffer.indexOf(URL_F4M, posAnfang)) != -1) {
                        pos1 += URL_F4M.length();
                        if ((pos2 = strBuffer.indexOf("<", pos1)) != -1) {
                            if (pos2 < posEnde) {
                                urlF4m = strBuffer.substring(pos1, pos2);
                                break;
                            }
                        }
                    }
                }
            }
        }
        // und noch die URL
        // <formitaet basetype="h264_aac_mp4_http_na_na" isDownload="false">
        //    <quality>veryhigh</quality>
        //    <url>http://nrodl.zdf.de/none/zdf/13/05/130528_vorschau_afo_1596k_p13v9.mp4</url>
        // </formitaet>
        posAnfang = 0;
        posEnde = 0;
        while (true) {
            if ((posAnfang = strBuffer.indexOf(URL_ANFANG, posAnfang)) == -1) {
                break;
            }
            posAnfang += URL_ANFANG.length();
            if ((posEnde = strBuffer.indexOf(URL_ENDE, posAnfang)) == -1) {
                break;
            }
            if ((pos1 = strBuffer.indexOf(QUALITAET, posAnfang)) != -1) {
                if (pos1 < posEnde) {
                    if (!urlKlein.isEmpty() && !urlKlein.contains("metafilegenerator")) {
                        continue;
                    }
                    if ((pos1 = strBuffer.indexOf(URL, posAnfang)) != -1) {
                        pos1 += URL.length();
                        if ((pos2 = strBuffer.indexOf("<", pos1)) != -1) {
                            if (pos2 < posEnde) {
                                urlKlein = strBuffer.substring(pos1, pos2);
                            }
                        }
                    }
                }
            }
            if ((pos1 = strBuffer.indexOf("<quality>veryhigh</quality>", posAnfang)) != -1) {
                if (pos1 < posEnde) {
                    if (!url.isEmpty() && !url.contains("metafilegenerator")) {
                        continue;
                    }
                    if ((pos1 = strBuffer.indexOf(URL, posAnfang)) != -1) {
                        pos1 += URL.length();
                        if ((pos2 = strBuffer.indexOf("<", pos1)) != -1) {
                            if (pos2 < posEnde) {
                                url = strBuffer.substring(pos1, pos2);
                            }
                        }
                    }
                }
            }
        }
        // und jetzt nochmal für HD
        posAnfang = 0;
        posEnde = 0;
        while (true) {
            if ((posAnfang = strBuffer.indexOf(URL_ANFANG_HD, posAnfang)) == -1) {
                break;
            }
            posAnfang += URL_ANFANG_HD.length();
            if ((posEnde = strBuffer.indexOf(URL_ENDE, posAnfang)) == -1) {
                break;
            }
            if ((pos1 = strBuffer.indexOf("<quality>hd</quality>", posAnfang)) != -1) {
                if (pos1 > posEnde) {
                    break;
                }
                if ((pos1 = strBuffer.indexOf(URL, posAnfang)) != -1) {
                    pos1 += URL.length();
                    if ((pos2 = strBuffer.indexOf("<", pos1)) != -1) {
                        if (pos2 < posEnde) {
                            urlHd = strBuffer.substring(pos1, pos2);
                            break;
                        }
                    }
                }
            }
        }
        if (url.isEmpty() && !urlKlein.isEmpty()) {
            url = urlKlein;
            urlKlein = "";
        }
        if (!urlF4m.isEmpty()) {
            String u = f4mUrlHolen(getUrl, sender, strBuffer, urlF4m);
            if (!u.isEmpty()) {
                url = u;
            }
        }
        if (url.isEmpty()) {
            Log.fehlerMeldung(-397002891, Log.FEHLER_ART_MREADER, "MediathekZdf.filmHolen", "keine URL: " + filmWebsite);
            return null;
        } else {
            DatenFilm film = new DatenFilm(sender, thema, filmWebsite, titel, url, "" /*urlRtmp*/, datum, zeit,
                    extractDuration(laenge), beschreibung, bild, new String[]{""});
            film.addUrlKlein(urlKlein, "");
            film.addUrlHd(urlHd, "");
            return film;
        }
    }

    public static String f4mUrlHolen(GetUrl getUrl, String sender, MVStringBuilder strBuffer, String urlf4m) {
        //<manifest xmlns="http://ns.adobe.com/f4m/2.0">
        //    <baseURL>http://zdf_hdflash_none-f.akamaihd.net/z/</baseURL>
        //    <media href="mp4/none/3sat/13/07/130714_zkm_bonus_rundgang_museumscheck_736k_p11v11.mp4/manifest.f4m?hdcore" bitrate="680000"/>
        //    <media href="mp4/none/3sat/13/07/130714_zkm_bonus_rundgang_museumscheck_1056k_p12v11.mp4/manifest.f4m?hdcore" bitrate="1000000"/>
        //    <media href="mp4/none/3sat/13/07/130714_zkm_bonus_rundgang_museumscheck_2256k_p14v11.mp4/manifest.f4m?hdcore" bitrate="2200000"/>
        //</manifest>
        final String URL = "<media href=\"mp4";
        String url = "";
        int pos1 = 0, pos2;
        strBuffer = getUrl.getUri_Utf(sender, urlf4m, strBuffer, "url: " + urlf4m);
        if (strBuffer.length() == 0) {
            Log.fehlerMeldung(-610123987, Log.FEHLER_ART_MREADER, "MediathekZdf.f4mUrlHolen", "url: " + urlf4m);
            return "";
        }
        while (true) {
            if ((pos1 = strBuffer.indexOf(URL, pos1)) == -1) {
                break;
            } else {
                pos1 += URL.length();
                if ((pos2 = strBuffer.indexOf("?", pos1)) == -1) {
                    break;
                } else {
                    url = strBuffer.substring(pos1, pos2);
                    if (url.contains("2256k") && url.contains("mp4")) {
                        // das draus bauen:
                        // http://rodl.zdf.de/none/3sat/13/07/130714_zkm_bonus_rundgang_museumscheck_2256k_p14v11.mp4
                        url = "http://rodl.zdf.de" + url.substring(0, url.indexOf("mp4")) + "mp4";
                        return url;
                    }
                }
            }
        }
        return "";
    }
}
