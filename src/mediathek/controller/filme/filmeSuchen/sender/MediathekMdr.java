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
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.Daten;
import mediathek.daten.DatenFilm;
import mediathek.controller.filme.filmeSuchen.FilmeSuchen;
import mediathek.controller.io.GetUrl;
import mediathek.Log;

/**
 *
 * @author
 */
public class MediathekMdr extends MediathekReader implements Runnable {

    public static final String SENDER = "MDR";
    private String addr = "http://www.mdr.de/mediathek/fernsehen/a-z/sendungenabisz100.html";

    /**
     * 
     * @param ddaten
     */
    public MediathekMdr(FilmeSuchen ssearch) {
        super(ssearch, /* name */ SENDER, /* text */ "MDR  (ca. 3 MB, 1000 Filme)", /* threads */ 2, /* urlWarten */ 1000);
    }

    /**
     * 
     */
    @Override
    public void addToList() {
        //final String MUSTER = "<a href=\"/mediathek/fernsehen/a-z/5841479-";
        //final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/5841479-";
        final String MUSTER = "<a href=\"/mediathek/fernsehen/a-z/sendungenabisz100_letter-";
        final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/sendungenabisz100_letter-";
        StringBuffer seite = new StringBuffer();
        listeThemen.clear();
        seite = getUrlIo.getUri_Utf(senderName, addr, seite, "");
        int pos = 0;
        int pos1 = 0;
        int pos2 = 0;
        String url = "";
        while ((pos = seite.indexOf(MUSTER, pos)) != -1) {
            pos += MUSTER.length();
            pos1 = pos;
            pos2 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
            }
            if (url.equals("")) {
                Log.fehlerMeldung("MediathekMdr.addToList", "keine URL");
            } else {
                url = MUSTER_ADD + url;
                if (!istInListe(listeThemen, url, 0)) {
                    listeThemen.add(new String[]{url});
                }
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size());
                listeSort(listeThemen, 0);
                for (int t = 0; t < senderMaxThread; ++t) {
                    new Thread(new MdrThemaLaden()).start();
                }
            }
        }
    }

    private class MdrThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl( senderWartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer();
        private StringBuffer seite2 = new StringBuffer();
        private StringBuffer seite3 = new StringBuffer();
        private StringBuffer seite4 = new StringBuffer();

        @Override
        public void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    addThema(link[0]);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekMdr.MdrThemaLaden.run", ex);
            }
        }

        void addThema(String strUrlFeed) {
            //<h2><a href="/mediathek/fernsehen/a-z/6946914.html" class="pfeil">artour<span class="otype"> | Videos</span></a></h2>
            //final String MUSTER_TITEL = "class=\"pfeil\">";
            final String MUSTER_TITEL = "title=\"Alle verfügbaren Sendungen anzeigen\">";
            //final String MUSTER_URL = "<h2><a href=\"/mediathek/fernsehen/a-z/";
            //final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";
            final String MUSTER_URL = "<h3><a href=\"/mediathek/fernsehen/a-z/";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";

            int pos = 0;
            int pos2 = 0;
            String thema = "";
            String url = "";
            try {
                seite1 = getUrl.getUri_Utf(senderName, strUrlFeed, seite1, "");
                while ((pos = seite1.indexOf(MUSTER_URL, pos)) != -1) {
                    url = "";
                    pos += MUSTER_URL.length();
                    pos2 = seite1.indexOf("\"", pos);
                    if (pos != -1 && pos2 != -1) {
                        url = seite1.substring(pos, pos2);
                        pos = pos2;
                        if ((pos = seite1.indexOf(MUSTER_TITEL, pos)) != -1) {
                            pos += MUSTER_TITEL.length();
                            pos2 = seite1.indexOf("<", pos);
                            if (pos != -1 && pos2 != -1) {
                                thema = seite1.substring(pos, pos2);
                                pos = pos2;
                                if (!themaLaden(senderName, thema)) {
                                    //nur Abos laden
                                    continue;
                                }
                            }
                            if (url.equals("")) {
                                Log.fehlerMeldung("MediathekMdr.addThema", "keine URL: " + strUrlFeed);
                            } else {
                                addSendug(strUrlFeed, thema, MUSTER_ADD + url);
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekMdr.addThema", ex);
            }
        }

        private void addSendug(String strUrlFeed, String thema, String urlThema) {
            //final String MUSTER = "<li><a href=\"http://www.mdr.de/Media/stream/";
            //final String MUSTER_ADD = "http://www.mdr.de/Media/stream/";
            final String MUSTER_START = "<span class=\"ressortHead\">Sendungen von A bis Z</span>";
            final String MUSTER = "<a href=\"/mediathek/fernsehen/a-z/";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";
            LinkedList<String> tmpListe = new LinkedList<String>();
            seite2 = getUrl.getUri_Utf(senderName, urlThema, seite2, "Thema: " + thema);
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            if ((pos = seite2.indexOf(MUSTER_START)) != -1) {
                while ((pos = seite2.indexOf(MUSTER, pos)) != -1) {
                    pos += MUSTER.length();
                    pos1 = pos;
                    pos2 = seite2.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite2.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldung("MediathekMdr.addSendug", new String[]{"keine URL: " + urlThema, "Thema: " + thema, "UrlFeed: " + strUrlFeed});
                    } else {
                        url = MUSTER_ADD + url;
                        if (!tmpListe.contains(url)) {
                            tmpListe.add(url);
                        }
                    }
                }
            }
            Iterator<String> it = tmpListe.iterator();
            while (it.hasNext()) {
                addFilme1(strUrlFeed, thema, it.next());
            }
        }

        void addFilme1(String strUrlFeed, String thema, String urlFilm) {

            final String MUSTER_START = "<span class=\"ressortHead\">Sendungen von A bis Z</span>";
            final String MUSTER_URL = "<a href=\"/mediathek/fernsehen/a-z/";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";
            LinkedList<String> tmpListe = new LinkedList<String>();
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            try {
                seite3 = getUrl.getUri_Utf(senderName, urlFilm, seite3, "Thema: " + thema);
                if ((pos = seite3.indexOf(MUSTER_START)) != -1) {
                    while ((pos = seite3.indexOf(MUSTER_URL, pos)) != -1) {
                        url = "";
                        pos += MUSTER_URL.length();
                        pos1 = pos;
                        pos2 = seite3.indexOf("\"", pos);
                        if (pos1 != -1 && pos2 != -1) {
                            pos = pos2;
                            url = seite3.substring(pos1, pos2);
                        }
                        if (url.equals("")) {
                            Log.fehlerMeldung("MediathekMdr.addFilme1", new String[]{"keine URL: " + urlFilm, "Thema: " + thema, " UrlFeed: " + strUrlFeed});
                        } else {
                            url = MUSTER_ADD + url;
                            if (!tmpListe.contains(url)) {
                                tmpListe.add(url);
                            }
                        }
                    }
                    Iterator<String> it = tmpListe.iterator();
                    while (it.hasNext()) {
                        addFilme2(strUrlFeed, thema, it.next());
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekMdr.addFilme1", ex);
            }
        }

        void addFilme2(String strUrlFeed, String thema, String urlFilm) {
            final String MUSTER_TITEL = "<title>";
            final String MUSTER_URL = "<a class=\"avWmLink\" href=\"";
            final String MUSTER_DATUM = "<meta name=\"date\" content=\"";
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String titel = "";
            String url = "";
            String datum = "";
            try {
                seite4 = getUrl.getUri_Utf(senderName, urlFilm, seite4, "Thema: " + thema);
                if ((pos = seite4.indexOf(MUSTER_URL)) != -1) {
                    url = "";
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    pos2 = seite4.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite4.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldung("MediathekMdr.addFilme2", new String[]{"keine URL: " + urlFilm, "Thema: " + thema, " UrlFeed: " + strUrlFeed});
                    } else {
                        if ((pos = seite4.indexOf(MUSTER_TITEL)) != -1) {
                            pos += MUSTER_TITEL.length();
                            pos1 = pos;
                            pos2 = seite4.indexOf("<", pos);
                            if (pos1 != -1 && pos2 != -1) {
                                titel = seite4.substring(pos1, pos2);
                                if (titel.contains("|")) {
                                    titel = titel.substring(0, titel.indexOf("|")).trim();
                                }
                            }
                        }
                        if ((pos = seite4.indexOf(MUSTER_DATUM)) != -1) {
                            pos += MUSTER_DATUM.length();
                            pos1 = pos;
                            pos2 = seite4.indexOf("\"", pos);
                            if (pos1 != -1 && pos2 != -1) {
                                datum = seite4.substring(pos1, pos2);
                                if (datum.contains("+")) {
                                    datum = datum.substring(0, datum.indexOf("+")).trim();
                                }
                            }
                        }
                        //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                        addFilm(new DatenFilm(senderName, thema, strUrlFeed, titel,
                                url, convertDatum(datum)/* datum */, convertTime(datum)/* zeit */));
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekMdr.addFilme2", ex);
            }
        }

        private String convertDatum(String datum) {
            //<meta name="date" content="2011-06-30T23:05:13+02:00"/>
            try {
                SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
                Date filmDate = sdfIn.parse(datum);
                SimpleDateFormat sdfOut;
                sdfOut = new SimpleDateFormat("dd.MM.yyyy");
                datum = sdfOut.format(filmDate);
            } catch (Exception ex) {
                Log.fehlerMeldung("MediathekMdr.convertDatum", ex);
            }
            return datum;
        }

        private String convertTime(String datum) {
            //<meta name="date" content="2011-06-30T23:05:13+02:00"/>
            try {
                SimpleDateFormat sdfIn = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss");
                Date filmDate = sdfIn.parse(datum);
                SimpleDateFormat sdfOut;
                sdfOut = new SimpleDateFormat("HH:mm:ss");
                datum = sdfOut.format(filmDate);
            } catch (Exception ex) {
                Log.fehlerMeldung("MediatheMdr.convertTime", ex);
            }
            return datum;
        }
    }
}
