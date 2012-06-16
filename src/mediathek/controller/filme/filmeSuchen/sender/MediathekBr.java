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

import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.zip.ZipInputStream;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.daten.DatenFilm;

public class MediathekBr extends MediathekReader implements Runnable {

    public static final String SENDER = "BR";

    public MediathekBr(FilmeSuchen ssearch) {
        super(ssearch, /* name */ SENDER,  /* threads */ 2, /* urlWarten */ 500);
    }

    private class ThemaLaden implements Runnable {

        @Override
        public synchronized void run() {
            meldungStart(1);
            meldungAddThread();
            try {
                laden();
            } catch (Exception ex) {
                Log.fehlerMeldung(-761355948,"MediathekBr.ThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }
    }

    @Override
    void addToList() {
        new Thread(new ThemaLaden()).start();
    }

    void laden() {
        //        <video application="a792/o16" host="gffstream.fcod.llnwd.net" groesse="xlarge" stream="br/b7/b7konks25277.mp4" typ="aufzeichnung"/>
        StringBuilder seite = new StringBuilder();
        int pos = 0;
        int posEnde;
        int pos1;
        int pos2;
        String url;
        String thema;
        String link;
        String datum;
        String zeit;
        String titel;
        String tmp;
        final String ITEM_1 = "<ausstrahlung";
        final String ITEM_2 = "</ausstrahlung>";
        final String MUSTER_URL = "<video ";
        final String MUSTER_THEMA = "<titel>";
        final String MUSTER_TITEL = "<nebentitel>";
        final String MUSTER_DATUM = "<beginnPlan>";
        //final String ADRESSE = "http://rd.gl-systemhaus.de/br/b7/archive/archive.xml.zip.adler32";
        //final String ADRESSE = "http://rd.gl-systemhaus.de/br/b7/listra/archive/archive.xml.zip.adler32";
        //final String ADRESSE="http://mediathek-video.br.de/nc/archive/archive-1328897218.xml.zip.adler32";
        final String ADRESSE = "http://rd.gl-systemhaus.de/br/b7/nc/archive/archive.xml.zip.adler32";
        meldungProgress(ADRESSE);
        try {
            InputStreamReader inReader;
            int timeout = 30000;
            char[] zeichen = new char[1];
            URLConnection conn = new URL(ADRESSE).openConnection();
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            conn.setReadTimeout(timeout);
            conn.setConnectTimeout(timeout);
            ZipInputStream zipInputStream = new ZipInputStream(conn.getInputStream());
            zipInputStream.getNextEntry();
            inReader = new InputStreamReader(zipInputStream, Konstanten.KODIERUNG_UTF);
            seite.setLength(0);
            while (!Daten.filmeLaden.getStop() && inReader.read(zeichen) != -1) {
                seite.append(zeichen);
            }
            while ((pos = seite.indexOf(ITEM_1, pos)) != -1) {
                pos += ITEM_1.length();
                if ((posEnde = seite.indexOf(ITEM_2, pos)) == -1) {
                    break;
                }
                url = "";
                thema = "";
                link = "";
                datum = "";
                zeit = "";
                titel = "";
                pos1 = pos;
                while (true) {
                    pos1 = seite.indexOf(MUSTER_URL, pos1);
                    if (pos1 == -1) {
                        break;
                    } else {
                        pos1 += MUSTER_URL.length();
                        if ((pos2 = seite.indexOf("/>", pos1)) != -1) {
                            if (pos1 > posEnde || pos2 > posEnde) {
                                break;
                            }
                            url = seite.substring(pos1, pos2);
                            if (url.contains("xlarge")) {
                                break;
                            }
                        }
                    }
                }
                if (url.equals("")) {
                    //LogFilme.fehlerMeldung("MediathekBr.addToList", "keine URL");
                } else {
                    if ((pos1 = seite.indexOf(MUSTER_THEMA, pos)) != -1) {
                        pos1 += MUSTER_THEMA.length();
                        if ((pos2 = seite.indexOf("</", pos1)) != -1) {
                            if (pos1 < posEnde && pos2 < posEnde) {
                                //      <titel><![CDATA[Vom Ahorn bis zur Zwiebel]]></titel>
                                thema = seite.substring(pos1, pos2);
                                thema = thema.replace("<!", "");
                                thema = thema.replace("[", "");
                                thema = thema.replace("CDATA", "");
                                thema = thema.replace("]", "");
                                thema = thema.replace(">", "");
                            }
                        }
                    }
                    if ((pos1 = seite.indexOf(MUSTER_TITEL, pos)) != -1) {
                        pos1 += MUSTER_TITEL.length();
                        if ((pos2 = seite.indexOf("</", pos1)) != -1) {
                            if (pos1 < posEnde && pos2 < posEnde) {
                                //      <nebentitel><![CDATA[Der Spargel]]></nebentitel>
                                titel = seite.substring(pos1, pos2);
                                titel = titel.replace("<!", "");
                                titel = titel.replace("[", "");
                                titel = titel.replace("CDATA", "");
                                titel = titel.replace("]", "");
                                titel = titel.replace(">", "");
                            }
                        }
                    }
                    if (titel.equals("")) {
                        titel = thema;
                    }
                    if ((pos1 = seite.indexOf(MUSTER_DATUM, pos)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite.indexOf("<", pos1)) != -1) {
                            if (pos1 < posEnde && pos2 < posEnde) {
                                //      <beginnPlan>2010-12-09T10:55:00</beginnPlan>
                                tmp = seite.substring(pos1, pos2);
                                datum = convertDatum(tmp);
                                zeit = convertTime(tmp);
                            }
                        }
                    }
                    // <video application="a792/o16" host="gffstream.fcod.llnwd.net" groesse="xlarge" stream="br/b7/b7konks25277.mp4" typ="aufzeichnung"/>
                    // mnt/daten/software/bin/flvstreamer/flvstreamer --host gffstream.fcod.llnwd.net --app a792/o16/ --playpath mp4:br/b7/b7konks25241.mp4 -o film-3
                    int p;
                    String host = "";
                    String app = "";
                    String play = "";
                    if ((p = url.indexOf("host=\"")) != -1) {
                        p += "host=\"".length();
                        host = url.substring(p, url.indexOf("\"", p));
                    }
                    if ((p = url.indexOf("application=\"")) != -1) {
                        p += "application=\"".length();
                        app = url.substring(p, url.indexOf("\"", p));
                    }
                    if ((p = url.indexOf("stream=\"")) != -1) {
                        p += "stream=\"".length();
                        play = url.substring(p, url.indexOf("\"", p));
                    }
                    //rtmp://gffstream.fcod.llnwd.net/a792/o16/br/b7/b7konks25460.mp4
                    //mp4:konks/b7/listra/konks/1316073962-b7konks_listra_144931476_7292.mp4
//                    String urlRtmp = "--host " + host + " --app " + app + " --playpath mp4:" + play;
                    String urlOrg = "rtmp://" + host + "/" + app + "/" + play;
                    // DatenFilm(ddaten, ssender, tthema, urlThema,  ttitel, uurl, uurlorg, uurlRtmp, datum, zeit, alt)
                    DatenFilm film = new DatenFilm(senderNameMReader, thema, link, titel, urlOrg, datum, zeit);
                    addFilm(film);
                }
            } //while, die ganz große Schleife
        } catch (Exception ex) {
            Log.fehlerMeldung(-963486054,"MediathekBr.laden", ex);
        }
    }

    public String convertDatum(String datum) {
        //      <beginnPlan>2010-12-09T10:55:00</beginnPlan>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("dd.MM.yyyy");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(-210365944,"MediathekBr.convertDatum", ex);
        }
        return datum;
    }

    public String convertTime(String datum) {
        //      <beginnPlan>2010-12-09T10:55:00</beginnPlan>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("HH:mm:ss");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(-573690176,"MediatheBr.convertTime", ex);
        }
        return datum;
    }
}
