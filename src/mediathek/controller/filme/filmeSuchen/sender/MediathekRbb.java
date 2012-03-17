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

import java.text.SimpleDateFormat;
import java.util.Date;
import mediathek.Log;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.daten.DatenFilm;

public class MediathekRbb extends MediathekReader implements Runnable {

    public static final String SENDER = "RBB";

    public MediathekRbb(FilmeSuchen ssearch) {
        super(ssearch, /* name */ SENDER, /* text */ "RBB  (ca. 3 MB, 50 Filme)", /* threads */ 2, /* urlWarten */ 500);
    }

    private class ThemaLaden implements Runnable {

        @Override
        public synchronized void run() {
            meldungStart(1);
            meldungAddThread();
            try {
                laden();
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekRbb.ThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }
    }

    @Override
    void addToList() {
        new Thread(new ThemaLaden()).start();
    }

    void laden() {
        StringBuffer seite = new StringBuffer();
        int pos = 0;
        int pos1;
        int pos2;
        String url;
        String titel;
        String datum;
        final String ADRESSE = "http://www.rbb-online.de/doku/videothek/den_film_im_tv_verpasst.html";
        final String DATUM = "id=\"rbbheadTitle\">";
        final String TITEL = "<span id=\"caption\">";
        final String MUSTER_URL = "'file': 'http:";
        // Datum: <div id="rbbheadTitle">Himmel und Erde, 17.03.12</div>
        // URL: 'file': 'http://http-stream.rbb-online.de/rbb/himmelunderde/doku/himmelunderde_20120317_groesse_zeigen_m_16_9_512x288.mp4',
        // Titel: <span id="caption">Mehr als 1000 Worte - Was unsere Stimme verrät</span>
        meldungProgress(ADRESSE);
        try {
            seite = getUrlIo.getUri_Utf(senderName, ADRESSE, seite, "");
            while ((pos = seite.indexOf(DATUM, pos)) != -1) {
                pos += DATUM.length();
                url = "";
                titel = "";
                datum = "";
                pos1 = pos;
                if ((pos2 = seite.indexOf("</", pos)) != -1) {
                    datum = seite.substring(pos1, pos2);
                    datum = datum.substring(datum.lastIndexOf(",") + 1).trim();
                    datum = convertDatum(datum);
                }
                if ((pos1 = seite.indexOf(MUSTER_URL, pos)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = seite.indexOf("'", pos1)) != -1) {
                        url = seite.substring(pos1, pos2);
                    }
                }
                if ((pos1 = seite.indexOf(TITEL, pos)) != -1) {
                    pos1 += TITEL.length();
                    if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                        titel = seite.substring(pos1, pos2);
                    }
                }
                if (url.equals("")) {
                    Log.fehlerMeldung("MediathekRbb.laden", "keine URL");
                } else {
                    url = "http:" + url;
                    // DatenFilm(String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
                    DatenFilm film = new DatenFilm(senderName, titel, ADRESSE, titel, url, datum, ""/* zeit */);
                    addFilm(film);
                }
            } //while, die ganz große Schleife
        } catch (Exception ex) {
            Log.fehlerMeldung("MediathekRbb.laden", ex);
        }
    }

    public String convertDatum(String datum) {
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("dd.mm.yy");
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("dd.MM.yyyy");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung("MediathekRbb.convertDatum", ex);
        }
        return datum;
    }
}
