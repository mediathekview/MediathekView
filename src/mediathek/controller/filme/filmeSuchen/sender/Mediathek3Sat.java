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
import mediathek.controller.filme.FilmeLaden;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.daten.DatenFilm;
import mediathek.tool.DatumZeit;

public class Mediathek3Sat extends MediathekReader implements Runnable {

    public static final String SENDER = "3Sat";

    public Mediathek3Sat(FilmeSuchen ssearch) {
        super(ssearch, /* name */ SENDER, /* text */ "3sat  (ca. 4 MB, 1000 Filme)", /* threads */ 2, /* urlWarten */ 500);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://www.3sat.de/mediathek/mediathek.php?mode=rss";
        final String MUSTER_URL = "<a href=\"rss/mediathek_";
        final String MUSTER_THEMA = "alt=\"RSS-Feed:";
        listeThemen.clear();
        StringBuffer seite = new StringBuffer();
        //seite = new GetUrl(daten).getUriArd(ADRESSE, seite, "");
        seite = getUrlIo.getUri_Iso(senderName, ADRESSE, seite, "");
        int pos = 0;
        int pos1 = 0;
        int pos2 = 0;
        String url = "";
        String thema = "";
        String tmp = "";
        while ((pos = seite.indexOf(MUSTER_URL, pos)) != -1) {
            try {
                pos += MUSTER_URL.length();
                pos1 = pos;
                pos2 = seite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1) {
                    url = seite.substring(pos1, pos2);
                }
                if (url.equals("")) {
                    continue;
                }
                if (url.contains(".xml")) {
                    thema = url.substring(0, url.indexOf(".xml"));
                }
                // nach dem Thema schauen
                if ((pos = seite.indexOf(MUSTER_THEMA, pos)) != -1) {
                    pos += MUSTER_THEMA.length();
                    pos1 = pos;
                    pos2 = seite.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        tmp = seite.substring(pos1, pos2).trim();
                    }
                    if (!tmp.equals("")) {
                        thema = tmp;
                    }
                }
                // in die Liste eintragen
                String[] add = new String[]{"http://www.3sat.de/mediathek/rss/mediathek_" + url, thema};
                if (!istInListe(listeThemen, url, 0)) {
                    listeThemen.add(add);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("Mediathek3sat.addToList", ex);
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            listeSort(listeThemen, 1);
            // noch den RSS für alles anfügen
            // Liste von http://www.3sat.de/mediathek/rss/mediathek.xml holen
            String[] add = new String[]{"http://www.3sat.de/mediathek/rss/mediathek.xml", ""};
            listeThemen.add(0, add); // alle nachfolgenden Filme ersetzen Filme die bereits in der Liste sind
            meldungStart(listeThemen.size());
            new Thread(new ThemaLaden()).start();
        }
    }

    private class ThemaLaden implements Runnable {

        private StringBuffer seite = new StringBuffer();

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
                Log.fehlerMeldung("Mediathek3Sat.ThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }

        void laden(String url_rss, String thema_rss) {
            final String MUSTER_URL = "type=\"video/x-ms-asf\" url=\"";
            final String MUSTER_TITEL = "<title>";
            final String MUSTER_DATUM = "<pubDate>";
            final String MUSTER_LINK = "<link>";
            seite = getUrlIo.getUri_Utf(senderName, url_rss, seite, "");
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            String thema = "";
            String link = "";
            String datum = "";
            String zeit = "";
            String titel = "";
            String tmp = "";
            if ((pos = seite.indexOf(MUSTER_TITEL, pos)) == -1) {
                return;
            }//für den HTML-Titel
            pos += MUSTER_TITEL.length();
            if ((pos = seite.indexOf(MUSTER_TITEL, pos)) == -1) {
                return;
            }//für den HTML-Titel
            pos += MUSTER_TITEL.length();
            while ((pos = seite.indexOf(MUSTER_TITEL, pos)) != -1) {
                pos += MUSTER_TITEL.length();
                url = "";
                thema = "";
                link = "";
                datum = "";
                zeit = "";
                titel = "";
                tmp = "";
                try {
                    pos1 = pos;
                    if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                        titel = seite.substring(pos1, pos2);
                        thema = senderName;
                        if (titel.contains(":") && (titel.indexOf(":") + 1) < titel.length()) {
                            //enthält : und ist nicht das letztes zeichen
                            thema = titel.substring(0, titel.indexOf(":"));
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
                    if ((pos1 = seite.indexOf(MUSTER_DATUM, pos)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                            //<pubDate>Mon, 03 Jan 2011 17:06:16 +0100</pubDate>
                            tmp = seite.substring(pos1, pos2);
                            if (tmp.equals("")) {
                                Log.fehlerMeldung("Mediathek3Sat.addToList", "keine Datum");
                            } else {
                                datum = DatumZeit.convertDatum(tmp);
                                zeit = DatumZeit.convertTime(tmp);
                            }
                        }
                    }
                    if ((pos1 = seite.indexOf(MUSTER_LINK, pos)) != -1) {
                        pos1 += MUSTER_LINK.length();
                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                            //<link>http://www.3sat.de/mediathek/?obj=20937</link>
                            link = seite.substring(pos1, pos2);
                        }
                    }
                    if ((pos1 = seite.indexOf(MUSTER_URL, pos)) != -1) {
                        pos1 += MUSTER_URL.length();
                        if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                            url = seite.substring(pos1, pos2);
                        }
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldung("Mediathek3Sat.addToList", "keine URL");
                    } else {
                        url = url.replace("/300/", "/veryhigh/");
                        //    public DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum) {
                        addFilm(new DatenFilm(senderName, (thema_rss.equals("") ? thema : thema_rss), link, titel, url, datum, zeit));
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung("Mediathek3Sat.laden", ex);
                }
            } //while, die ganz große Schleife
        }
    }
}