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
import mediathek.Log;
import mediathek.controller.filme.filmeSuchenSender.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;

/**
 *
 *  @author
 */
public class MediathekMdr extends MediathekReader implements Runnable {

    public static final String SENDER = "MDR";
    private LinkedList<String> listeTage = new LinkedList<String>();
    private LinkedList<String[]> listeGesucht = new LinkedList<String[]>(); //thema,titel,datum,zeit

    /**
     *
     *  @param ddaten
     */
    public MediathekMdr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 4, /* urlWarten */ 1000, startPrio);
    }

    /**
     *
     */
    @Override
    public void addToList() {
        final String URL_SENDUNGEN = "http://www.mdr.de/mediathek/fernsehen/a-z/sendungenabisz100.html";
        final String URL_TAGE = "http://www.mdr.de/mediathek/fernsehen/index.html";
        final String MUSTER = "<a href=\"/mediathek/fernsehen/a-z/sendungenabisz100_letter-";
        final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/sendungenabisz100_letter-";
        final String MUSTER_TAGE = "<a href=\"/mediathek/fernsehen/sendungverpasst100-multiGroupClosed_boxIndex-";
        final String MUSTER_ADD_TAGE = "http://www.mdr.de/mediathek/fernsehen/sendungverpasst100-multiGroupClosed_boxIndex-";
        StringBuffer seite = new StringBuffer();
        listeThemen.clear();
        listeTage.clear();
        listeGesucht.clear();
        seite = getUrlIo.getUri_Utf(nameSenderMReader, URL_SENDUNGEN, seite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        while ((pos = seite.indexOf(MUSTER, pos)) != -1) {
            pos += MUSTER.length();
            pos1 = pos;
            pos2 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
            }
            if (url.equals("")) {
                Log.fehlerMeldungMReader(-889216307, "MediathekMdr.addToList", "keine URL");
            } else {
                url = MUSTER_ADD + url;
                if (!istInListe(listeThemen, url, 0)) {
                    listeThemen.add(new String[]{url});
                }
            }
        }
        seite = getUrlIo.getUri_Utf(nameSenderMReader, URL_TAGE, seite, "");
        pos = 0;
        url = "";
        while ((pos = seite.indexOf(MUSTER_TAGE, pos)) != -1) {
            pos += MUSTER_TAGE.length();
            pos1 = pos;
            pos2 = seite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1) {
                url = seite.substring(pos1, pos2);
            }
            if (url.equals("")) {
                Log.fehlerMeldungMReader(-461225808, "MediathekMdr.addToList-2", "keine URL");
            } else {
                url = MUSTER_ADD_TAGE + url;
                if (!istInListe(listeTage, url)) {
                    listeTage.add(url);
                }
            }
        }
        if (!Daten.filmeLaden.getStop()) {
            if (listeThemen.size() > 0) {
                meldungStart(listeThemen.size() + listeTage.size());
                listeSort(listeThemen, 0);
                for (int t = 0; t < maxThreadLaufen; ++t) {
                    new Thread(new MdrThemaLaden()).start();
                }
            }
        }
    }

    private class MdrThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
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
                String url;
                while (!Daten.filmeLaden.getStop() && (url = getListeTage()) != null) {
                    meldungProgress(url);
                    addTage(url);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldung(-115896304, "MediathekMdr.MdrThemaLaden.run", ex);
            }
        }

        void addThema(String strUrlFeed) {
            final String MUSTER_TITEL = "title=\"Alle verfügbaren Sendungen anzeigen\">";
            final String MUSTER_URL = "<h3><a href=\"/mediathek/fernsehen/a-z/";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";

            int pos = 0;
            int pos2;
            String thema = "";
            String url;
            try {
                seite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite1, "");
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_URL, pos)) != -1) {
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
                            }
                            if (url.equals("")) {
                                Log.fehlerMeldungMReader(-766250249, "MediathekMdr.addThema", "keine URL: " + strUrlFeed);
                            } else {
                                meldung(url);
                                addSendug(strUrlFeed, thema, MUSTER_ADD + url);
                            }
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-316874602, "MediathekMdr.addThema", ex);
            }
        }

        private void addSendug(String strUrlFeed, String thema, String urlThema) {
            final String MUSTER_START = "<span class=\"ressortHead\">Sendungen von A bis Z</span>";
            final String MUSTER = "<a href=\"/mediathek/fernsehen/a-z/";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";
            LinkedList<String> tmpListe = new LinkedList<String>();
            seite2 = getUrl.getUri_Utf(nameSenderMReader, urlThema, seite2, "Thema: " + thema);
            int pos;
            int pos1;
            int pos2;
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
                        Log.fehlerMeldung(-256987304, "MediathekMdr.addSendug", new String[]{"keine URL: " + urlThema, "Thema: " + thema, "UrlFeed: " + strUrlFeed});
                    } else {
                        url = MUSTER_ADD + url;
                        if (!tmpListe.contains(url)) {
                            tmpListe.add(url);
                        }
                    }
                }
            }
            Iterator<String> it = tmpListe.iterator();
            while (!Daten.filmeLaden.getStop() && it.hasNext()) {
                addFilme1(strUrlFeed, thema, it.next());
            }
        }

        void addFilme1(String strUrlFeed, String thema, String urlFilm) {

            final String MUSTER_START = "<span class=\"ressortHead\">Sendungen von A bis Z</span>";
            final String MUSTER_URL = "<a href=\"/mediathek/fernsehen/a-z/";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";
            LinkedList<String> tmpListe = new LinkedList<String>();
            int pos;
            int pos1;
            int pos2;
            String url;
            try {
                seite3 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite3, "Thema: " + thema);
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
                            Log.fehlerMeldung(-976286603, "MediathekMdr.addFilme1", new String[]{"keine URL: " + urlFilm, "Thema: " + thema, " UrlFeed: " + strUrlFeed});
                        } else {
                            url = MUSTER_ADD + url;
                            if (!tmpListe.contains(url)) {
                                tmpListe.add(url);
                            }
                        }
                    }
                    Iterator<String> it = tmpListe.iterator();
                    while (!Daten.filmeLaden.getStop() && it.hasNext()) {
                        addFilme2(strUrlFeed, thema, it.next());
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-446286970, "MediathekMdr.addFilme1", ex);
            }
        }

        void addTage(String urlSeite) {
            final String MUSTER_START_1 = "<div class=\"teaserImage\">";
            final String MUSTER_START_2 = "<h3>";
            final String MUSTER_URL = "<a href=\"/mediathek/fernsehen/";
            final String MUSTER_URL_ADD = "http://www.mdr.de/mediathek/fernsehen/";
            final String MUSTER_TITEL = "\">";
            int pos = 0;
            int pos2;
            String url;
            String thema = "";
            try {
                seite1 = getUrl.getUri_Utf(nameSenderMReader, urlSeite, seite1, "");
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_START_1, pos)) != -1) {
                    pos += MUSTER_START_1.length();
                    if ((pos = seite1.indexOf(MUSTER_START_2, pos)) == -1) {
                        break;
                    }
                    pos += MUSTER_START_2.length();
                    if ((pos = seite1.indexOf(MUSTER_URL, pos)) == -1) {
                        break;
                    }
                    pos += MUSTER_URL.length();
                    if ((pos2 = seite1.indexOf("\"", pos)) != -1) {
                        url = seite1.substring(pos, pos2);
                        pos = pos2;
                        if ((pos = seite1.indexOf(MUSTER_TITEL, pos)) == -1) {
                            break;
                        }
                        pos += MUSTER_TITEL.length();
                        pos2 = seite1.indexOf("<", pos);
                        if (pos2 != -1) {
                            thema = seite1.substring(pos, pos2).trim();
                            pos = pos2;
                        }
                        if (url.equals("")) {
                            Log.fehlerMeldungMReader(-333620478, "MediathekMdr.addThema", "keine URL: " + urlSeite);
                        } else {
                            url = MUSTER_URL_ADD + url;
                            meldung(url);
                            addTage2(urlSeite, url, thema);
                        }
                    }
                }// while
            } catch (Exception ex) {
                Log.fehlerMeldung(-556320478, "MediathekMdr.addThema", ex);
            }
        }

        void addTage2(String urlFeed, String urlSeite, String thema) {
            //<div class="teaserImage">
            //<a href="/mediathek/fernsehen/video57930_zc-7931f8bf_zs-2d7967f4.html" title="Video starten">            final String MUSTER_START_1 = "<div class=\"teaserImage\">";
            final String MUSTER_START = "<div class=\"teaserImage\">";
            final String MUSTER_URL = "<a href=\"/mediathek/fernsehen/";
            final String MUSTER_URL_ADD = "http://www.mdr.de/mediathek/fernsehen/";
            int pos = 0;
            int pos2;
            String url;
            try {
                seite2 = getUrl.getUri_Utf(nameSenderMReader, urlSeite, seite2, "");
                while (!Daten.filmeLaden.getStop() && (pos = seite2.indexOf(MUSTER_START, pos)) != -1) {
                    pos += MUSTER_START.length();
                    if ((pos = seite2.indexOf(MUSTER_URL, pos)) == -1) {
                        break;
                    }
                    pos += MUSTER_URL.length();
                    if ((pos2 = seite2.indexOf("\"", pos)) != -1) {
                        url = seite2.substring(pos, pos2);
                        pos = pos2;
                        if (url.equals("")) {
                            Log.fehlerMeldungMReader(-733652014, "MediathekMdr.addThema", "keine URL: " + urlSeite);
                        } else {
                            url = MUSTER_URL_ADD + url;
                            meldung(url);
                            addFilme2(urlFeed, thema, url);
                        }
                    }
                }// while
            } catch (Exception ex) {
                Log.fehlerMeldung(-785542103, "MediathekMdr.addThema", ex);
            }
        }

        void addFilme2(String strUrlFeed, String thema, String urlFilm) {
            final String MUSTER_TITEL = "<title>";
            final String MUSTER_URL = "<a class=\"avWmLink\" href=\"";
            final String MUSTER_DATUM = "<meta name=\"date\" content=\"";
            int pos;
            int pos1;
            int pos2;
            String titel = "";
            String url;
            String datum = "";
            try {
                seite4 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite4, "Thema: " + thema);
                if ((pos = seite4.indexOf(MUSTER_URL)) != -1) {
                    url = "";
                    pos += MUSTER_URL.length();
                    pos1 = pos;
                    pos2 = seite4.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite4.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldung(-895624708, "MediathekMdr.addFilme2", new String[]{"keine URL: " + urlFilm, "Thema: " + thema, " UrlFeed: " + strUrlFeed});
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
                        String ddatum = convertDatum(datum);
                        String zeit = convertTime(datum);
                        if (!istInListe(thema, titel, ddatum, zeit)) {
                            addInListe(thema, titel, ddatum, zeit);
                            meldung(url);
                            //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                            addFilm(new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, ddatum, zeit));
                        } else {
                            //Log.systemMeldung("MDR: Film doppelt");
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-554310287, "MediathekMdr.addFilme2", ex);
            }
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
            Log.fehlerMeldung(-259640136, "MediathekMdr.convertDatum", ex);
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
            Log.fehlerMeldung(-499637085, "MediatheMdr.convertTime", ex);
        }
        return datum;
    }

    private synchronized String getListeTage() {
        return listeTage.pollFirst();
    }

    private synchronized boolean istInListe(String thema, String titel, String datum, String zeit) {
        Iterator<String[]> it = listeGesucht.iterator();
        while (it.hasNext()) {
            String[] k = it.next();
            if (k[0].equalsIgnoreCase(thema) && k[1].equalsIgnoreCase(titel) && k[2].equalsIgnoreCase(datum) && k[3].equalsIgnoreCase(zeit)) {
                return true;
            }
        }
        return false;
    }

    private void addInListe(String thema, String titel, String datum, String zeit) {
        listeGesucht.add(new String[]{thema, titel, datum, zeit});
    }
}
