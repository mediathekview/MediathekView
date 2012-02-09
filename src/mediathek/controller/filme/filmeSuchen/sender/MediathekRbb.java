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
import mediathek.daten.DatenFilm;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.Log;

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
        int pos1 = 0;
        int pos2 = 0;
        String url = "";
        String thema = "";
        String datum = "";
        String titel = "";
        final String ITEM_1 = "<div class=\"teaserFlash\">";
        final String MUSTER_URL = "verpasst.html\" href=\""; //href="/ozon/ozon_20110131_sdg_MP4H264_m_16_9_512x288.mp4">Das Geheimnis der Indios - Kohle als Klimachance?</a>
        final String ADRESSE = "http://www.rbb-online.de/doku/videothek/den_film_im_tv_verpasst.html";
        meldungProgress(ADRESSE);
        try {
            seite = getUrlIo.getUri_Utf(senderName, ADRESSE, seite, "");
            while ((pos = seite.indexOf(ITEM_1, pos)) != -1) {
                pos += ITEM_1.length();
                url = "";
                thema = "";
                datum = "";
                titel = "";
                pos1 = pos;
                if ((pos1 = seite.indexOf(MUSTER_URL, pos)) != -1) {
                    pos1 += MUSTER_URL.length();
                    if ((pos2 = seite.indexOf("\"", pos1)) != -1) {
                        url = seite.substring(pos1, pos2);
                    }
                }
                if (url.equals("")) {
                    Log.fehlerMeldung("MediathekRbb.laden", "keine URL");
                } else {
                    if ((pos1 = seite.indexOf(">", pos2)) != -1) {
                        pos1 += 1;
                        if ((pos2 = seite.indexOf("</", pos1)) != -1) {
                            thema = seite.substring(pos1, pos2);
                            titel = thema;
                        }
                    }
                    if ((pos1 = url.indexOf("201")) != -1) {
                        if (url.length() > pos1 + 8) {
                            datum = convertDatum(url.substring(pos1, pos1 + 8));
                        }
                    } else if ((pos1 = url.indexOf("200")) != -1) {
                        //für 2009
                        if (url.length() > pos1 + 8) {
                            datum = convertDatum(url.substring(pos1, pos1 + 8));
                        }
                    }
                }
                // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String datum, String zeit) {
                //DatenFilm film = new DatenFilm(daten, sender, thema, ADRESSE, titel, Funktionen.addsUrl(daten, "rtmp://stream5.rbb-online.de/rbb", url), datum, ""/*zeit*/);
                //mit den anderen flvstreamer-Einstellungen
                //String urlRtmp = "--host stream5.rbb-online.de --app=rbb/ --playpath=mp4:" + url;
                String urlRtmp;
                if (url.endsWith("mp4")) {
                    urlRtmp = "--host stream5.rbb-online.de --app rbb/ --playpath mp4:" + url;
                } else {
                    urlRtmp = "--host stream5.rbb-online.de --app rbb/ --playpath " + url;
                }
                String urlOrg = addsUrl("rtmp://stream5.rbb-online.de/rbb", url);
                // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String uurlRtmp, String datum, String zeit, boolean alt)
                DatenFilm film = new DatenFilm(senderName, thema, ADRESSE, titel, urlRtmp, urlOrg, urlRtmp, datum, ""/* zeit */);
                addFilm(film);
            } //while, die ganz große Schleife
        } catch (Exception ex) {
            Log.fehlerMeldung("MediathekRbb.laden", ex);
        }
    }

    public String convertDatum(String datum) {
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("yyyyMMdd");
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
