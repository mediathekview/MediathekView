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

import java.util.LinkedList;
import mediathek.daten.Daten;
import mediathek.tool.Log;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import org.apache.commons.lang3.StringEscapeUtils;

/**
 *
 * @author
 */
public class MediathekSwr extends MediathekReader implements Runnable {

    public static final String SENDER = "SWR";
    public int startThema = 0;
    final int MAX_SEITEN = 500;
    public final int MAX_THEMEN_UPDATE = 4;
    public final int MAX_THEMEN_ALLES = 10;
    private int seiten = 0;

    public MediathekSwr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 1, /* urlWarten */ 1000, startPrio);
    }

    //===================================
    // public
    //===================================
    @Override
    public synchronized void addToList() {
        //nur im --nogui laufen lassen
        if (suchen.listeFilmeNeu.infos[ListeFilme.FILMLISTE_INFOS_SWR_NR_THEMA_NR].equals("")) {
            suchen.listeFilmeNeu.infos[ListeFilme.FILMLISTE_INFOS_SWR_NR_THEMA_NR] = "0";
        }
        startThema = Integer.parseInt(suchen.listeFilmeNeu.infos[ListeFilme.FILMLISTE_INFOS_SWR_NR_THEMA_NR]);
        if (Daten.nogui) {
            //Theman suchen
            seiten = 0;
            listeThemen.clear();
            meldungStart(0);
            addToList__("http://www.swrmediathek.de/tvlist.htm");
            suchen.listeFilmeNeu.alteThemenLöschen(nameSenderMReader, listeThemen);
            if (!Daten.filmeLaden.getStop() && listeThemen.size() > 0) {
                for (int t = 0; t < maxThreadLaufen; ++t) {
                    new Thread(new SenderThemaLaden()).start();
                }
            }
        } else {
            meldungStart(0);
            meldungFertig();
        }
    }

    //===================================
    // private
    //===================================
    private void addToList__(String ADRESSE) {
        //Theman suchen
        final String MUSTER_URL = "<a href=\"tvshow.htm?show=";
        final String MUSTER_THEMA = "title=\"";
        StringBuffer strSeite = new StringBuffer();
        strSeite = getUrlIo.getUri_Utf(nameSenderMReader, ADRESSE, strSeite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url;
        String thema = "";
        while (!Daten.filmeLaden.getStop() && (pos = strSeite.indexOf(MUSTER_URL, pos)) != -1) {
            pos += MUSTER_URL.length();
            pos1 = pos;
            pos2 = strSeite.indexOf("\"", pos);
            if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                url = strSeite.substring(pos1, pos2);
                pos = pos2;
                pos = strSeite.indexOf(MUSTER_THEMA, pos);
                pos += MUSTER_THEMA.length();
                pos1 = pos;
                pos2 = strSeite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1) {
                    thema = strSeite.substring(pos1, pos2);
                    thema = StringEscapeUtils.unescapeHtml4(thema.trim()); //wird gleich benutzt und muss dann schon stimmen
                }
                if (url.equals("")) {
                    Log.fehlerMeldungMReader(-163255009, "MediathekSwr.addToList__", "keine URL");
                } else {
                    //url = url.replace("&amp;", "&");
                    String[] add = new String[]{"http://swrmediathek.de/tvshow.htm?show=" + url, thema};
                    listeThemen.addUrl(add);
                }
            }
        }
    }

    private class SenderThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer strSeite1 = new StringBuffer();
        private StringBuffer strSeite2 = new StringBuffer();

        public SenderThemaLaden() {
        }

        @Override
        public void run() {
            try {
                int maxThemen = MAX_THEMEN_UPDATE;
                if (suchen.allesLaden) {
                    maxThemen = MAX_THEMEN_ALLES;
                }
                int themen = 0;
                meldungAddThread();
                String[] link = null;
                int nrListe = 0;
                if (listeThemen.size() > 0) {
                    try {
                        nrListe = startThema;
                    } catch (Exception ex) {
                        nrListe = 0;
                    }
                    //liste präparieren
                    if (listeThemen.size() <= nrListe) {
                        nrListe = 0;
                    } else {
                        int k = 0;
                        while (k < nrListe) {
                            if ((link = getListeThemen()) != null) {
                                ++k;
                            } else {
                                break;
                            }
                        }
                    }
                    if (listeThemen.size() > maxThemen) {
                        meldungAddMax(maxThemen);
                    } else {
                        meldungAddMax(listeThemen.size());
                    }
                    while (!Daten.filmeLaden.getStop() && themen < maxThemen && (link = getListeThemen()) != null) {
                        ++themen;
                        ++nrListe;
                        themenSeitenSuchen(link[0] /* url */, link[1] /* Thema */);
                        meldungProgress(link[0]);
                        meldungAddMax(1);
                    }
                    suchen.listeFilmeNeu.setInfo(ListeFilme.FILMLISTE_INFOS_SWR_NR_THEMA_NR, String.valueOf(nrListe));
                    meldungThreadUndFertig();
                }
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-739285690, "MediathekSwr.SenderThemaLaden.run", ex.getMessage());
            }
        }

        private void themenSeitenSuchen(String strUrlFeed, String thema) {
            final String MUSTER_URL = "<a class=\"plLink\" href=\"player.htm?show=";
            LinkedList<String> urls = new LinkedList<String>();
            strSeite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, strSeite1, thema);
            ++seiten;
            int pos = 0;
            int pos1;
            int pos2;
            String url;
            while (!Daten.filmeLaden.getStop() && (pos = strSeite1.indexOf(MUSTER_URL, pos)) != -1) {
                pos += MUSTER_URL.length();
                pos1 = pos;
                pos2 = strSeite1.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 != pos2) {
                    url = strSeite1.substring(pos1, pos2);
                    pos = pos2;
                    if (url.equals("")) {
                        Log.fehlerMeldungMReader(-648522300, "MediathekSwr.themenSeiteSuchen", "keine URL, Thema: " + thema);
                    } else {
                        url = "http://swrmediathek.de/player.htm?show=" + url;
                        if (!urls.contains(url)) {
                            //url sind mehrfach auf der Seite
                            urls.add(url);
                            addFilme2(strUrlFeed, thema, url);
                        }
                    }
                }
            }
        }

        private void addFilme2(String strUrlFeed, String thema, String urlFilm) {
            final String MUSTER_URL = "<span class=\"entry_media\">";
            final String MUSTER_TITEL = "entry_title=\"";
//            final String MUSTER_DATUM_1 = "<span class=\"mClipInfos\">";
//            final String MUSTER_DATUM_2 = "</span>";
            final String MUSTER_DATUM_1 = "<span class=\"entry_pdatehd\">";
            final String MUSTER_DATUM_2 = "</span>";
            final String MUSTER_ZEIT_1 = "<span class=\"entry_pdateht\">";
            final String MUSTER_ZEIT_2 = "</span>";

            //<span class="mClipInfos">		22.11.2010 | 23.00 Uhr | 2:07 min</span>
            //<p><span class="group_title">2+Leif</span> vom <span class="entry_pdatehd">22.11.2010</span> | <span class="entry_pdateht">23.00</span> Uhr</p>
//            urlFilm = "http://www.swrmediathek.de/player.htm?show=3791f560-fb1d-11df-8817-0026b975f2e6";
            meldung(urlFilm);
            strSeite2 = getUrl.getUri_Utf(nameSenderMReader, urlFilm, strSeite2, "");
            ++seiten;
            int pos;
            int pos1;
            int pos2;
            String url = "";
            String titel;
            String datum = "";
            String zeit = "";
            String tmp = "";
            // Datum suchen
            if ((pos = strSeite2.indexOf(MUSTER_DATUM_1)) != -1) {
                pos += MUSTER_DATUM_1.length();
                try {
                    if ((pos1 = strSeite2.indexOf(MUSTER_DATUM_2, pos)) != -1) {
                        datum = strSeite2.substring(pos, pos1).trim();
                        if (datum.length() < 10) {
                            if (datum.contains(".")) {
                                if ((tmp = datum.substring(0, datum.indexOf("."))).length() != 2) {
                                    datum = "0" + datum;
                                }
                            }
                            if (datum.indexOf(".") != datum.lastIndexOf(".")) {
                                if ((tmp = datum.substring(datum.indexOf(".") + 1, datum.lastIndexOf("."))).length() != 2) {
                                    datum = datum.substring(0, datum.indexOf(".") + 1) + "0" + datum.substring(datum.indexOf(".") + 1);
                                }
                            }
                        }
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldungMReader(-649820016, "MediathekSwr.addFilme2-1", new String[]{ex.getMessage(), thema + " " + urlFilm});
                }
            }
            // Zeit suchen
            if ((pos = strSeite2.indexOf(MUSTER_ZEIT_1)) != -1) {
                pos += MUSTER_ZEIT_1.length();
                try {
                    if ((pos1 = strSeite2.indexOf(MUSTER_ZEIT_2, pos)) != -1) {
                        zeit = strSeite2.substring(pos, pos1).trim();
                        if (!zeit.equals("")) {
                            if (zeit.contains("Uhr")) {
                                zeit = zeit.replace("Uhr", "");
                            }
                            if (zeit.length() <= 5) {
                                zeit = zeit.trim() + ":00";
                            }
                            zeit = zeit.replace(".", ":");
                            if (zeit.length() < 8) {
                                if (zeit.contains(":")) {
                                    if ((tmp = zeit.substring(0, zeit.indexOf(":"))).length() != 2) {
                                        zeit = "0" + zeit;
                                    }
                                }
                                if (zeit.indexOf(":") != zeit.lastIndexOf(":")) {
                                    if ((tmp = zeit.substring(zeit.indexOf(":") + 1, zeit.lastIndexOf(":"))).length() != 2) {
                                        zeit = zeit.substring(0, zeit.indexOf(":") + 1) + "0" + zeit + zeit.substring(zeit.lastIndexOf(":"));
                                    }
                                }
                            }
                        }
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldungMReader(-795623410, "MediathekSwr.addFilme2-2", new String[]{ex.getMessage(), thema + " " + urlFilm});
                }
            }
            pos = 0;
            //url suchen
            if ((pos = strSeite2.indexOf(MUSTER_URL, pos)) != -1) {
                pos += MUSTER_URL.length();
                pos1 = pos;
                pos2 = strSeite2.indexOf("<", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 < pos2) {
                    url = strSeite2.substring(pos1, pos2);
                }
            }
            pos = 0;
            //Titel
            if ((pos = strSeite2.indexOf(MUSTER_TITEL, pos)) != -1) {
                pos += MUSTER_TITEL.length();
                pos1 = pos;
                pos2 = strSeite2.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1 && pos1 < pos2) {
                    titel = strSeite2.substring(pos1, pos2);
                    if (!url.equals("")) {
                        // hohe Auflösung
                        if (url.startsWith("rtmp:")) {
                            // rtmp://fc-ondemand.swr.de/a4332/e6/swr-fernsehen/2plusleif/2012/05/14/538821.l.mp4
                            url = url.replace(".m.mp4", ".l.mp4");
                        }
                        // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                        DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit);
                        addFilm(film);
                    } else {
                        Log.fehlerMeldungMReader(-468200690, "MediathekSwr.addFilme2-4", thema + " " + urlFilm);
                    }
                }
            }
        }
    }
}
