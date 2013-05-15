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
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import org.apache.commons.lang3.StringEscapeUtils;

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
        StringBuffer strSeite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
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
                    Log.fehlerMeldung(-163255009, Log.FEHLER_ART_MREADER, "MediathekSwr.addToList__", "keine URL");
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
        private StringBuffer strSeite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        private StringBuffer strSeite2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);

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
            } catch (Exception ex) {
                Log.fehlerMeldung(-739285690, Log.FEHLER_ART_MREADER, "MediathekSwr.SenderThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
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
                if (!suchen.senderAllesLaden) {
                    ++max;
                    if (max > 5) {
                        break;
                    }
                }
                pos1 += MUSTER_URL.length();
                if ((pos2 = strSeite1.indexOf("\"", pos1)) != -1) {
                    url = strSeite1.substring(pos1, pos2);
                    if (url.equals("")) {
                        Log.fehlerMeldung(-875012369, Log.FEHLER_ART_MREADER, "MediathekSwr.addFilme2", "keine URL, Thema: " + thema);
                    } else {
                        url = "http://swrmediathek.de/AjaxEntry?callback=jsonp1347979401564&ekey=" + url;
                        json(strUrlFeed, thema, url);
                    }

                }

            }
        }

        private void json(String strUrlFeed, String thema, String urlJson) {
            //:"entry_media","attr":{"val0":"h264","val1":"3","val2":"rtmp://fc-ondemand.swr.de/a4332/e6/swr-fernsehen/landesschau-rp/aktuell/2012/11/582111.l.mp4",
            // oder
            // "entry_media":"http://mp4-download.swr.de/swr-fernsehen/zur-sache-baden-wuerttemberg/das-letzte-wort-podcast/20120913-2015.m.mp4"
            // oder
            // :"entry_media","attr":{"val0":"flashmedia","val1":"1","val2":"rtmp://fc-ondemand.swr.de/a4332/e6/swr-fernsehen/eisenbahn-romantik/381104.s.flv","val3":"rtmp://fc-ondemand.swr.de/a4332/e6/"},"sub":[]},{"name":"entry_media","attr":{"val0":"flashmedia","val1":"2","val2":"rtmp://fc-ondemand.swr.de/a4332/e6/swr-fernsehen/eisenbahn-romantik/381104.m.flv","val3":"rtmp://fc-ondemand.swr.de/a4332/e6/"},"sub":[]

            // "entry_title":"\"Troika-Tragödie - Verspielt die Regierung unser Steuergeld?\"

            final String MUSTER_TITEL_1 = "\"entry_title\":\"";
            final String MUSTER_TITEL_2 = "\"entry_title\":\"\\\"";
            final String MUSTER_DATUM = "\"entry_pdatehd\":\"";
            final String MUSTER_DAUER = "\"entry_durat\":\"";
            final String MUSTER_ZEIT = "\"entry_pdateht\":\"";
            final String MUSTER_URL_START = "rtmp://";
            final String MUSTER_URL_1 = "\"entry_media\":\"";
            final String MUSTER_URL_2 = "\"entry_media\",\"attr\":{\"val0\":\"h264\"";
            final String MUSTER_URL_3 = "\"entry_media\",\"attr\":{\"val0\":\"flashmedia\"";
            final String MUSTER_DESCRIPTION = "\"entry_descl\":\"";
            final String MUSTER_THUMBNAIL_URL = "\"entry_image_16_9\":\"";
            int pos1;
            int pos2;
            String url;
            String titel = "";
            String datum = "";
            String zeit = "";
            long dauer = 0;
            String description = "";
            String thumbnailUrl = "";
            String[] keywords = null;
            String tmp;
            try {
                strSeite2 = getUrl.getUri_Utf(nameSenderMReader, urlJson, strSeite2, "");
                if ((pos1 = strSeite2.indexOf(MUSTER_TITEL_1)) != -1) {
                    pos1 += MUSTER_TITEL_1.length();

                    if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                        titel = strSeite2.substring(pos1, pos2);
                    }
                }
                if (titel.startsWith("\\") && (pos1 = strSeite2.indexOf(MUSTER_TITEL_2)) != -1) {
                    pos1 += MUSTER_TITEL_2.length();
                    if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                        titel = strSeite2.substring(pos1, pos2);
                        if (titel.endsWith("\\")) {
                            titel = titel.substring(0, titel.length() - 2);
                        }
                    }
                }
                if ((pos1 = strSeite2.indexOf(MUSTER_DAUER)) != -1) {
                    pos1 += MUSTER_DAUER.length();
                    if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                        String d = strSeite2.substring(pos1, pos2);
                        String[] parts = d.split(":");
                        long power = 1;
                        for (int i = parts.length - 1; i >= 0; i--) {
                            dauer += Long.parseLong(parts[i]) * power;
                            power *= 60;
                        }
                    }
                }

                if ((pos1 = strSeite2.indexOf(MUSTER_DESCRIPTION)) != -1) {
                    pos1 += MUSTER_DESCRIPTION.length();
                    if ((pos2 = strSeite2.indexOf("\",", pos1)) != -1) {
                        description = strSeite2.substring(pos1, pos2);
                    }
                }

                if ((pos1 = strSeite2.indexOf(MUSTER_THUMBNAIL_URL)) != -1) {
                    pos1 += MUSTER_THUMBNAIL_URL.length();
                    if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                        thumbnailUrl = strSeite2.substring(pos1, pos2);
                    }
                }

                keywords = extractKeywords(strSeite2);
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
//                if ((pos1 = strSeite2.indexOf(MUSTER_DAUER)) != -1) {
//                    pos1 += MUSTER_DAUER.length();
//                    if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
//                        dauer = strSeite2.substring(pos1, pos2);
//                    }
//                }
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
                url = "";
                // entweder
                if ((pos1 = strSeite2.indexOf(MUSTER_URL_1)) != -1) {
                    pos1 += MUSTER_URL_1.length();
                    if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                        url = strSeite2.substring(pos1, pos2);
                        if (!url.equals("")) {
                            // hohe Auflösung
                            if (url.startsWith("rtmp:")) {
                                // rtmp://fc-ondemand.swr.de/a4332/e6/swr-fernsehen/2plusleif/2012/05/14/538821.l.mp4
                                url = url.replace(".m.mp4", ".l.mp4");
                            }
                            // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
//                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit);
                            DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit, dauer, description, thumbnailUrl, "", keywords);
                            addFilm(film);
                        } else {
                            Log.fehlerMeldung(-468200690, Log.FEHLER_ART_MREADER, "MediathekSwr.addFilme2-4", thema + " " + urlJson);
                        }
                    }
                }
                // oder
                if (url.equals("") && (pos1 = strSeite2.indexOf(MUSTER_URL_2)) != -1) {
                    pos1 += MUSTER_URL_2.length();
                    if ((pos1 = strSeite2.indexOf(MUSTER_URL_START, pos1)) != -1) {
                        if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                            url = strSeite2.substring(pos1, pos2);
                            if (!url.equals("")) {
                                // hohe Auflösung
                                // rtmp://fc-ondemand.swr.de/a4332/e6/swr-fernsehen/2plusleif/2012/05/14/538821.l.mp4
                                url = url.replace(".m.mp4", ".l.mp4");
                                // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
                                //DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit);
                                DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit, dauer, description, thumbnailUrl, "", keywords);
                                addFilm(film);
                            } else {
                                Log.fehlerMeldung(-468200690, Log.FEHLER_ART_MREADER, "MediathekSwr.json-1", thema + " " + urlJson);
                            }
                        }
                    }
                }
                // oder
                if (url.equals("") && (pos1 = strSeite2.indexOf(MUSTER_URL_3)) != -1) {
                    pos1 += MUSTER_URL_3.length();
                    if ((pos1 = strSeite2.indexOf(MUSTER_URL_START, pos1)) != -1) {
                        if ((pos2 = strSeite2.indexOf("\"", pos1)) != -1) {
                            url = strSeite2.substring(pos1, pos2);
                            if (!url.equals("")) {
                                // hohe Auflösung
                                // rtmp://fc-ondemand.swr.de/a4332/e6/swr-fernsehen/2plusleif/2012/05/14/538821.l.mp4
                                url = url.replace(".s.mp4", ".m.mp4");
                                // DatenFilm(Daten ddaten, String ssender, String tthema, String urlThema, String ttitel, String uurl, String uurlorg, String zziel) {
//                                DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit);
                                DatenFilm film = new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, datum, zeit, dauer, description, thumbnailUrl, "", keywords);
                                addFilm(film);
                            } else {
                                Log.fehlerMeldung(-468200690, Log.FEHLER_ART_MREADER, "MediathekSwr.json-1", thema + " " + urlJson);
                            }
                        }
                    }
                }
                if (url.equals("")) {
                    Log.fehlerMeldung(-203690478, Log.FEHLER_ART_MREADER, "MediathekSwr.jason-2", thema + " " + urlJson);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-939584720, Log.FEHLER_ART_MREADER, "MediathekSwr.json-3", thema + " " + urlJson);
            }
        }

        private String[] extractKeywords(StringBuffer strSeite2) {
            // {"name":"entry_keywd","attr":{"val":"Fernsehserie"},"sub":[]}
            final String MUSTER_KEYWORD_START = "{\"name\":\"entry_keywd\",\"attr\":{\"val\":\"";
            final String MUSTER_KEYWORD_END = "\"},\"sub\":[]}";

            LinkedList<String> keywords = new LinkedList<String>();
            int pos = 0;
            while ((pos = strSeite2.indexOf(MUSTER_KEYWORD_START, pos)) != -1) {
                pos += MUSTER_KEYWORD_START.length();
                int end = strSeite2.indexOf(MUSTER_KEYWORD_END, pos);
                if (end != -1) {
                    String keyword = strSeite2.substring(pos, end);
                    keywords.add(keyword);
                    pos = end;
                }
            }

            return keywords.toArray(new String[keywords.size()]);
        }
    }
}
