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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Log;

/**
 *
 * @author
 */
public class MediathekMdr extends MediathekReader implements Runnable {

    public static final String SENDER = "MDR";
    private LinkedList<String> listeTage = new LinkedList<String>();
    private LinkedList<String[]> listeGesucht = new LinkedList<String[]>(); //thema,titel,datum,zeit

    /**
     *
     * @param ddaten
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
                listeThemen.addUrl(new String[]{url});
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
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0 && listeTage.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungStart(listeThemen.size() + listeTage.size());
            listeSort(listeThemen, 0);
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new MdrThemaLaden()).start();
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
                Log.fehlerMeldungMReader(-115896304, "MediathekMdr.MdrThemaLaden.run", ex.getMessage());
            }
        }

        void addTage(String urlSeite) {
            final String MUSTER_START_1 = "<div class=\"teaserImage\">";
            final String MUSTER_START_2 = "<h3>";
            final String MUSTER_THEMA = "title=\"Zu den Inhalten der Sendung\">";
            final String MUSTER_XML = "{container:'mediathekStage',dataURL:'/mediathek/fernsehen";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/";
            int pos = 0;
            int pos1, pos2;
            String url;
            String thema;
            try {
                seite1 = getUrl.getUri_Utf(nameSenderMReader, urlSeite, seite1, "");
                while (!Daten.filmeLaden.getStop() && (pos = seite1.indexOf(MUSTER_START_1, pos)) != -1) {
                    url = "";
                    thema = "";
                    pos += MUSTER_START_1.length();
                    if ((pos = seite1.indexOf(MUSTER_START_2, pos)) == -1) {
                        break;
                    }
                    pos += MUSTER_START_2.length();
                    // Thema
                    if ((pos1 = seite1.indexOf(MUSTER_THEMA, pos)) != -1) {
                        pos1 += MUSTER_THEMA.length();
                        if ((pos2 = seite1.indexOf("<", pos1)) != -1) {
                            thema = seite1.substring(pos1, pos2).trim();
                        }
                    }
                    // URL
                    if ((pos1 = seite1.indexOf(MUSTER_XML, pos)) != -1) {
                        pos1 += MUSTER_XML.length();
                        if ((pos2 = seite1.indexOf("'", pos1)) != -1) {
                            url = seite1.substring(pos1, pos2);
                        }
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldungMReader(-392854069, "MediathekMdr.addTage", new String[]{"keine URL: " + urlSeite});
                    } else {
                        url = MUSTER_ADD + url;
                        meldung(url);
                        addXml(urlSeite, thema, url);
                    }
                }// while
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-556320478, "MediathekMdr.addThema", ex.getMessage());
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
                seite2 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite2, "");
                while (!Daten.filmeLaden.getStop() && (pos = seite2.indexOf(MUSTER_URL, pos)) != -1) {
                    pos += MUSTER_URL.length();
                    pos2 = seite2.indexOf("\"", pos);
                    if (pos != -1 && pos2 != -1) {
                        url = seite2.substring(pos, pos2);
                        pos = pos2;
                        if ((pos = seite2.indexOf(MUSTER_TITEL, pos)) != -1) {
                            pos += MUSTER_TITEL.length();
                            pos2 = seite2.indexOf("<", pos);
                            if (pos != -1 && pos2 != -1) {
                                thema = seite2.substring(pos, pos2);
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
                Log.fehlerMeldungMReader(-316874602, "MediathekMdr.addThema", ex.getMessage());
            }
        }

        private void addSendug(String strUrlFeed, String thema, String urlThema) {
            final String MUSTER_START = "<span class=\"ressortHead\">Sendungen von A bis Z</span>";
            final String MUSTER_XML = "{container:'mediathekStage',dataURL:'/mediathek/fernsehen/a-z";
            final String MUSTER_ADD = "http://www.mdr.de/mediathek/fernsehen/a-z/";
            LinkedList<String> tmpListe = new LinkedList<String>();
            seite3 = getUrl.getUri_Utf(nameSenderMReader, urlThema, seite3, "Thema: " + thema);
            int pos;
            int pos1;
            int pos2;
            String url = "";
            if ((pos = seite3.indexOf(MUSTER_START)) != -1) {
                while ((pos = seite3.indexOf(MUSTER_XML, pos)) != -1) {
                    pos += MUSTER_XML.length();
                    pos1 = pos;
                    if ((pos2 = seite3.indexOf("'", pos)) != -1) {
                        url = seite3.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        Log.fehlerMeldungMReader(-256987304, "MediathekMdr.addSendug", new String[]{"keine URL: " + urlThema, "Thema: " + thema, "UrlFeed: " + strUrlFeed});
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
                addXml(strUrlFeed, thema, it.next());
            }
        }

        void addXml(String strUrlFeed, String thema, String urlFilm) {
            final String MUSTER_START = "<avDocument>";
            final String MUSTER_ENDE = "</avDocument>";
            final String MUSTER_TITEL = "<title>";
            final String MUSTER_URL_1 = "<flashMediaServerApplicationURL>";
            final String MUSTER_URL_2 = "<flashMediaServerURL>";
            final String MUSTER_DATUM = "<broadcastStartDate>";
            final String MUSTER_FRABE_WIDTH = "<frameWidth>";

            //<broadcastStartDate>23.08.2012 22:05</broadcastStartDate>
            int pos = 0, posEnde;
            int pos1;
            int pos2;
            String url1, url2, rtmpUrl, url, titel, datum, zeit, width;
            int widthAlt;
            try {
                seite4 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, seite4, "Thema: " + thema);
                if ((pos = seite4.indexOf(MUSTER_START)) == -1) {
                    Log.fehlerMeldungMReader(-903656532, "MediathekMdr.addXml", urlFilm);
                    return;
                }
                while ((pos = seite4.indexOf(MUSTER_TITEL, pos)) != -1) {
                    if ((posEnde = seite4.indexOf(MUSTER_ENDE, pos)) == -1) {
                        Log.fehlerMeldungMReader(-804142536, "MediathekMdr.addXml", urlFilm);
                        continue;
                    }
                    url1 = "";
                    url2 = "";
                    titel = "";
                    datum = "";
                    zeit = "";
                    pos += MUSTER_TITEL.length();
                    pos1 = pos;
                    if ((pos2 = seite4.indexOf("<", pos)) != -1) {
                        titel = seite4.substring(pos1, pos2);
                    }
                    if ((pos1 = seite4.indexOf(MUSTER_DATUM, pos)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite4.indexOf("<", pos1)) != -1) {
                            datum = seite4.substring(pos1, pos2);
                            zeit = convertZeitXml(datum);
                            datum = convertDatumXml(datum);
                        }
                    }
                    // URL mit der besten Auflösung suchen
                    pos1 = pos;
                    widthAlt = 0;
                    while ((pos1 = seite4.indexOf(MUSTER_FRABE_WIDTH, pos1)) != -1) {
                        if (pos1 > posEnde) {
                            break;
                        }
                        pos1 += MUSTER_FRABE_WIDTH.length();
                        if ((pos2 = seite4.indexOf("<", pos1)) != -1) {
                            width = seite4.substring(pos1, pos2);
                            try {
                                int tmp = Integer.parseInt(width);
                                if (tmp <= widthAlt) {
                                    continue;
                                } else {
                                    widthAlt = tmp;
                                }
                            } catch (Exception ex) {
                            }
                        }
                        if ((pos1 = seite4.indexOf(MUSTER_URL_1, pos1)) != -1) {
                            pos1 += MUSTER_URL_1.length();
                            if ((pos2 = seite4.indexOf("<", pos1)) != -1) {
                                url1 = seite4.substring(pos1, pos2);
                            }
                        }
                        if ((pos1 = seite4.indexOf(MUSTER_URL_2, pos1)) != -1) {
                            pos1 += MUSTER_URL_2.length();
                            if ((pos2 = seite4.indexOf("<", pos1)) != -1) {
                                url2 = seite4.substring(pos1, pos2);
                            }
                        }
                    }// while
                    if (url1.equals("") || url2.equals("")) {
                        Log.fehlerMeldungMReader(-326541230, "MediathekMdr.addXml", new String[]{"keine URL: " + urlFilm, "Thema: " + thema, " UrlFeed: " + strUrlFeed});
                    } else {
                        //<flashMediaServerApplicationURL>rtmp://x4100mp4dynonlc22033.f.o.f.lb.core-cdn.net/22033mdr/ondemand</flashMediaServerApplicationURL>
                        //<flashMediaServerURL>mp4:4100mp4dynonl/FCMS-1582b584-bb95-4fd2-94d8-389e10a4e1bd-8442e17c3177.mp4</flashMediaServerURL>
                        url = addsUrl(url1, url2);
                        rtmpUrl = "-r " + url1 + " -y " + url2;
                        if (!istInListe(thema, titel, datum, zeit)) {
                            addInListe(thema, titel, datum, zeit);
                            meldung(url);
                            //DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String uurlRtmp, String zziel)
                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, url /* orgUrl */, rtmpUrl, datum, zeit);
                            addFilm(film);
                        } else {
                            Log.systemMeldung("MDR: Film doppelt");
                        }
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-446286970, "MediathekMdr.addFilme1", ex.getMessage());
            }
        }
    }

    private String convertDatumXml(String datum) {
        //<broadcastStartDate>23.08.2012 22:05</broadcastStartDate>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("dd.MM.yyyy HH:mm");
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("dd.MM.yyyy");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldungMReader(-435209987, "MediathekMdr.convertDatum", ex.getMessage());
        }
        return datum;
    }

    private String convertZeitXml(String datum) {
        //<broadcastStartDate>23.08.2012 22:05</broadcastStartDate>
        try {
            SimpleDateFormat sdfIn = new SimpleDateFormat("dd.MM.yyyy HH:mm");
            Date filmDate = sdfIn.parse(datum);
            SimpleDateFormat sdfOut;
            sdfOut = new SimpleDateFormat("HH:mm:ss");
            datum = sdfOut.format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldungMReader(-102658736, "MediathekMdr.convertDatum", ex.getMessage());
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
