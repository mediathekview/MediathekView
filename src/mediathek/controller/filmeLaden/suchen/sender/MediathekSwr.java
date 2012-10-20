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
import org.apache.commons.lang3.StringEscapeUtils;

/**
 *
 *           @author
 */
public class MediathekSwr extends MediathekReader implements Runnable {

    public static final String SENDER = "SWR";

    public MediathekSwr(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 1000, startPrio);
    }

    //===================================
    // public
    //===================================
    @Override
    public synchronized void addToList() {
        meldungStart();
        //Theman suchen
        listeThemen.clear();
        addToList__("http://www.swrmediathek.de/tvlist.htm");
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new SenderThemaLaden()).start();
            }
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
        strSeite = getUrlIo.getUri(nameSenderMReader, ADRESSE, Konstanten.KODIERUNG_UTF, 2, strSeite, "");
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
                meldungAddThread();
                String[] link = null;
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    themenSeitenSuchen(link[0] /* url */, link[1] /* Thema */);
                    meldungProgress(link[0]);
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-739285690, "MediathekSwr.SenderThemaLaden.run", ex.getMessage());
            }
        }

        private void themenSeitenSuchen(String strUrlFeed, String thema) {
            final String MUSTER_URL = "<li><a class=\"plLink\" href=\"player.htm?show=";
            strSeite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, strSeite1, thema);
            meldung(strUrlFeed);
            int pos1 = 0;
            int pos2;
            String url;
            int max = 0;
            while (!Daten.filmeLaden.getStop() && (pos1 = strSeite1.indexOf(MUSTER_URL, pos1)) != -1) {
                if (!suchen.allesLaden) {
                    ++max;
                    if (max > 5) {
                        break;
                    }
                }
                pos1 += MUSTER_URL.length();
                if ((pos2 = strSeite1.indexOf("\"", pos1)) != -1) {
                    url = strSeite1.substring(pos1, pos2);
                    if (url.equals("")) {
                        Log.fehlerMeldungMReader(-875012369, "MediathekSwr.addFilme2", "keine URL, Thema: " + thema);
                    } else {
                        url = "http://swrmediathek.de/AjaxEntry?callback=jsonp1347979401564&ekey=" + url;
                        jason(strUrlFeed, thema, url);
                    }

                }

            }
        }

        protected void jason(String strUrlFeed, String thema, String urlJson) {
            final String MUSTER_TITEL = "\"entry_title\":\"";
            final String MUSTER_DATUM = "\"entry_pdatehd\":\"";
            final String MUSTER_DAUER = "\"entry_durat\":\"";
            final String MUSTER_ZEIT = "\"entry_pdateht\":\"";
            final String MUSTER_URL = "\"entry_media\":\"";
            int pos1;
            int pos2;
            String url;
            String titel = "";
            String datum = "";
            String zeit = "";
            String dauer = "";
            String tmp;
            strSeite2 = getUrl.getUri_Utf(nameSenderMReader, urlJson, strSeite2, "");
            if ((pos1 = strSeite2.indexOf(MUSTER_TITEL)) != -1) {
                pos1 += MUSTER_TITEL.length();

                if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                    titel = strSeite2.substring(pos1, pos2);
                }
            }
            if ((pos1 = strSeite2.indexOf(MUSTER_DATUM)) != -1) {
                pos1 += MUSTER_DATUM.length();
                if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                    datum = strSeite2.substring(pos1, pos2);
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
            }
            if ((pos1 = strSeite2.indexOf(MUSTER_DAUER)) != -1) {
                pos1 += MUSTER_DAUER.length();

                if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                    dauer = strSeite2.substring(pos1, pos2);
                }
            }
            if ((pos1 = strSeite2.indexOf(MUSTER_ZEIT)) != -1) {
                pos1 += MUSTER_ZEIT.length();
                if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                    zeit = strSeite2.substring(pos1, pos2);
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
            if ((pos1 = strSeite2.indexOf(MUSTER_URL)) != -1) {
                pos1 += MUSTER_URL.length();
                if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                    url = strSeite2.substring(pos1, pos2);
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
                        Log.fehlerMeldungMReader(-468200690, "MediathekSwr.addFilme2-4", thema + " " + urlJson);
                    }
                }
            }
        }
    }
}
