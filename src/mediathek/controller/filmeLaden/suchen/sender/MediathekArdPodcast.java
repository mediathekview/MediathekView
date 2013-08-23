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
import java.util.LinkedList;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.MVStringBuilder;
import org.apache.commons.lang3.StringEscapeUtils;

/**
 *
 * @author
 */
public class MediathekArdPodcast extends MediathekReader implements Runnable {

    public static final String SENDER = "ARD.Podcast";

    /**
     *
     * @param ddaten
     */
    public MediathekArdPodcast(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 4, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        final String ADRESSE = "http://www.ardmediathek.de/ard/servlet/ajax-cache/3551682/view=module/index.html";
        final String MUSTER_URL = "link\": \"";
        final String MUSTER_THEMA = "{ \"titel\": \"";
        final String MUSTER_SET = "http://www.ardmediathek.de";
        listeThemen.clear();
        MVStringBuilder seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        meldungStart();
        seite = getUrlIo.getUri_Utf(nameSenderMReader, ADRESSE, seite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        String thema = "";
        //Podcasts auslesen
        while ((pos = seite.indexOf(MUSTER_THEMA, pos)) != -1) {
            try {
                pos += MUSTER_THEMA.length();
                pos1 = pos;
                pos2 = seite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1) {
                    thema = seite.substring(pos1, pos2);
                }
                pos1 = seite.indexOf(MUSTER_URL, pos);
                pos1 = pos1 + MUSTER_URL.length();
                pos2 = seite.indexOf("\"", pos1);
                if (pos1 != -1 && pos2 != -1 && pos1 < pos2) {
                    url = seite.substring(pos1, pos2);
                }
                if (url.equals("")) {
                    continue;
                }
                if (!url.startsWith("/podcast/")) {
                    // nur dann ARD.Podcast
                    continue;
                }
                String[] add = new String[]{MUSTER_SET + url, thema};
                listeThemen.addUrl(add);
            } catch (Exception ex) {
                Log.fehlerMeldung(-764238903, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.addToList", ex, "kein Thema");
            }
        }
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            listeSort(listeThemen, 1);
            for (int t = 0; t < maxThreadLaufen; ++t) {
                //new Thread(new ThemaLaden()).start();
                Thread th = new Thread(new ThemaLaden());
                th.setName(nameSenderMReader + t);
                th.start();
            }
        }
    }

    private class ThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private MVStringBuilder seite = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);
        private MVStringBuilder seite2 = new MVStringBuilder(Konstanten.STRING_BUFFER_START_BUFFER);

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    meldungProgress(link[0]);
                    feedEinerSeiteSuchen(link[0] /* url */, link[1] /* Thema */);
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(-460287629, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.ArdThemaLaden.run", ex);
            }
            meldungThreadUndFertig();
        }

        private void feedEinerSeiteSuchen(String urlFeed, String thema) {
            //Feed eines Themas laden
            //<a class="mt-box_preload mt-box-overflow" href="/ard/servlet/ajax-cache/3516938/view=switch/documentId=427262/index.html">
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            String url = "";
            final String MUSTER = "<a class=\"mt-box_preload mt-box-overflow\" href=\"";
            final String MUSTER_SET = "http://www.ardmediathek.de";
            LinkedList<String> listeWeiter = new LinkedList<String>();
            boolean weiter = false;
            final String MUSTER_WEITER = "<option value=\"";
            seite = getUrl.getUri_Utf(nameSenderMReader, urlFeed, seite, "Thema: " + thema);
            //++++++++++++++++++++++++++++++++++ 1te Seite
            if ((pos = seite.indexOf(MUSTER, pos)) != -1) {
                pos += MUSTER.length();
                pos1 = pos;
                pos2 = seite.indexOf("\"", pos);
                if (pos1 != -1 && pos2 != -1) {
                    url = seite.substring(pos1, pos2);
                }
                if (url.equals("")) {
                    //-------------
                    Log.fehlerMeldung(-643188097, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeEinerSeiteSuchen-1", "keine URL für: " + urlFeed);
                } else {
                    url = MUSTER_SET + url;
                    //++++++++++++++++++++++++++++++++++ 2te Seite
                    //<h3 class="mt-title"><a href="/ard/servlet/content/3516968?documentId=1441144"
                    //final String MUSTER_2a = "<h3 class=\"mt-title\"><a href=\"";
                    final String MUSTER_2a = "<a href=\"";
                    final String MUSTER_2b = "\" class=\"mt-btt_rss\" onclick=\"";
                    String tmpUrl = url;
                    seite.setLength(0);
                    seite = getUrl.getUri_Utf(nameSenderMReader, url, seite, "Thema: " + thema);
                    pos = 0;
                    pos1 = 0;
                    pos2 = 0;
                    while ((pos = seite.indexOf(MUSTER_WEITER, pos)) != -1) {
                        pos += MUSTER_WEITER.length();
                        pos1 = pos;
                        pos2 = seite.indexOf("\"", pos);
                        if (pos1 != -1 && pos2 != -1) {
                            String tmpWeiter = MUSTER_SET + seite.substring(pos1, pos2);
                            listeWeiter.add(tmpWeiter);
                        }
                    }
                    do {
                        pos = 0;
                        pos1 = 0;
                        pos2 = 0;
                        url = "";
                        while (!Daten.filmeLaden.getStop() && (pos = seite.indexOf(MUSTER_2a, pos)) != -1) {
                            pos += MUSTER_2a.length();
                            pos1 = pos;
                            pos2 = seite.indexOf("\"", pos);
                            if (pos1 != -1 && pos2 != -1) {
                                url = seite.substring(pos1, pos2);
                            }
                            if (url.equals("")) {
                                //<a href="/ard/servlet/content/3517244?documentId=590570" class="mt-btt_rss" onclick="
                                pos = 0;
                                pos1 = 0;
                                pos2 = 0;
                                url = "";
                                if ((pos = seite.indexOf(MUSTER_2b, pos)) != -1) {
                                    pos1 = seite.lastIndexOf("\"");
                                    if (pos1 != -1) {
                                        url = seite.substring(pos1);
                                    }
                                }
                            }
                            if (url.equals("#")) {
                                Log.fehlerMeldung(-698025468, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeEinerSeiteSuchen-3", "keine URL für: " + tmpUrl);
                            } else if (url.equals("")) {
                                //-------------
                                Log.fehlerMeldung(-456903578, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeEinerSeiteSuchen-2", "keine URL für: " + urlFeed);
                            } else {
                                url = MUSTER_SET + url;
                                filmLaden(urlFeed, url, thema);
                            }
                        }
                        if (filmeSuchenSender.senderAllesLaden && listeWeiter.size() > 0) {
                            url = listeWeiter.pollFirst();
                            seite = getUrl.getUri_Utf(nameSenderMReader, url, seite, "Thema: " + thema);
                            weiter = true;
                        } else {
                            weiter = false;
                        }
                    } while (weiter);
                }
            }

        }

        private void filmLaden(String urlFeed, String filmWebsite, String thema) {
            int pos = 0;
            int pos1 = 0;
            int pos2 = 0;
            final String MUSTER_ = "http://www.ardmediathek.de/ard/servlet/content/3517244";
            if (!filmWebsite.contains("?")) {
                Log.fehlerMeldung(-969875421, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "keine URL für: " + filmWebsite);
                return;
            } else {
                //3517136 ersetzen mit 3517244
                //http://www.ardmediathek.de/ard/servlet/content/3516968?documentId=2584998
                //
                filmWebsite = MUSTER_ + filmWebsite.substring(filmWebsite.indexOf("?"));
                //++++++++++++++++++++++++++++++++++ 3te Seite
                //<input name="" type="text" value="http://www1.swr.de/podcast/xml/swr-fernsehen/60-jahre-rlp.xml" />
                //final String MUSTER_3 = "<input name=\"\" type=\"text\" value=\"";
                final String MUSTER_3 = "addMediaStream(0, 1, \"\", \"";
                seite2.setLength(0);
                seite2 = getUrl.getUri_Utf(nameSenderMReader, filmWebsite, seite2, "Thema: " + thema);
                if (seite2.length() == 0) {
                    Log.fehlerMeldung(-646569896, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "keine URL für: " + filmWebsite);
                    return;
                }
                long durationInSeconds = extractDuration(seite2);
                String description = extractDescription(seite2);
                String[] keywords = extractKeywords(seite2);
                String thumbnailUrl = extractThumbnailURL(seite2);
                String imageUrl = extractImageURL(seite2);

//                System.out.println(thema + ": " + durationInSeconds);
//                System.out.print("\tkeywords:");
//                for (String s : keywords) {
//                    System.out.print(" " + s);
//                }
//                System.out.println();
//                System.out.println(description);
                pos = 0;
                pos1 = 0;
                pos2 = 0;
                String url = "";
                String datum = "";
                String titel = "";
                if ((pos = seite2.indexOf(MUSTER_3, pos)) != -1) {
                    pos += MUSTER_3.length();
                    pos1 = pos;
                    pos2 = seite2.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite2.substring(pos1, pos2);
                    }
                    if (url.equals("")) {
                        //-------------
                        Log.fehlerMeldung(-363698701, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "keine URL für: " + filmWebsite);
                        return;
                    }
                }
                // Titel und Datum suchen
                //<title>ARD Mediathek: DiD-Folge 925: Die Dünnbrett-Bohrer - 16.05.2012 | Bayerisches Fernsehen</title>
                //<title>ARD Mediathek: 28 Stunden Ausnahmezustand in Freiburg | SWR Fernsehen BW</title>
                //<title>ARD Mediathek: Die Alpenüberquerung - Hoffentlich schwindelfrei | SWR Fernsehen</title>
                //<title>ARD Mediathek: Die Wahrheit über Deutschland: Leidenschaft | DW-TV</title>
                //<title>ARD Mediathek: Gipfeltreffen mit Ilse Neubauer - 17.05.2012 | Bayerisches Fernsehen</title>
                //<title>ARD Mediathek: Angeklickt: 18.05.2012, Es muss nicht immer Facebook sein | WDR Fernsehen</title>
                //<title>ARD Mediathek: angeklickt: 30.11.2012, Online Videos bearbeiten | WDR Fernsehen</title>
                // <title>Video-Clip &#034;Live aus Eging am See II - PS-Party in Pullmann City - 08.05.2013&#034; | Bayerisches Fernsehen | ARD Mediathek</title>
                final String MUSTER_TITEL = "<title>";
                if ((pos1 = seite2.indexOf(MUSTER_TITEL, 0)) == -1) {
                    Log.fehlerMeldung(-465698731, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "keine URL für: " + filmWebsite);
                    return;
                }
                pos1 += MUSTER_TITEL.length();
                if ((pos2 = seite2.indexOf("<", pos1)) == -1) {
                    Log.fehlerMeldung(-915487398, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "keine URL für: " + filmWebsite);
                    return;
                }
                titel = seite2.substring(pos1, pos2);
                if (titel.startsWith("Video-Clip")) {
                    titel = titel.replaceFirst("Video-Clip", "").trim();
                } else if (titel.startsWith("Audio-Clip")) {
                    titel = titel.replaceFirst("Audio-Clip", "");
                } else if (titel.startsWith("Video")) {
                    titel = titel.replaceFirst("Video", "").trim();
                } else if (titel.startsWith("Audio")) {
                    titel = titel.replaceFirst("Audio", "");
                } else {
                    Log.fehlerMeldung(-996500478, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "keine URL für: " + filmWebsite);
                    return;
                }
                titel = StringEscapeUtils.unescapeHtml4(titel);
                if (!titel.contains("|")) {
                    Log.fehlerMeldung(-102036977, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "keine URL für: " + filmWebsite);
                    return;
                }
                titel = titel.substring(0, titel.indexOf("|"));
                titel = titel.trim();
                if (titel.startsWith("\"")) {
                    titel = titel.substring(1);
                }
                if (titel.endsWith("\"")) {
                    titel = titel.substring(0, titel.length() - 1);
                }
                if (titel.contains(" - ") && titel.contains("20")) {
                    datum = titel.substring(titel.lastIndexOf(" - ") + 3).trim();
                    if (datum.length() != 10) {
                        //noch ein Versuch
                        if (titel.contains(".20")) {
                            int p = titel.indexOf(".20");
                            if (p > 6 && (p + 6) < titel.length()) {
                                datum = titel.substring(titel.indexOf(".20") - 5, titel.indexOf(".20") + 5);
                                titel = titel.replace(datum, "").trim();
                            }
                        }
                    } else {
                        titel = titel.substring(0, titel.lastIndexOf(" - ")).trim();
                    }
                } else {
                    if (titel.toLowerCase().contains("angeklickt:")) {
                        titel = titel.substring(titel.indexOf(":") + 1);
                        datum = titel.substring(0, titel.indexOf(",")).trim();
                    }
                }
                meldung(url);
                if (datum.equals("")) {
                    // xt_pageDate="201210211909";
                    final String MUSTER_DATUM = "xt_pageDate=\"";
                    if ((pos1 = seite2.indexOf(MUSTER_DATUM)) != -1) {
                        pos1 += MUSTER_DATUM.length();
                        if ((pos2 = seite2.indexOf("\"", pos1)) != -1) {
                            datum = seite2.substring(pos1, pos2);
                            if (datum.length() > 8) {
                                datum = datum.substring(0, 8);
                                datum = convertDatum(datum);
                            }
                        }
                    }
                }
                if (datum.equals("")) {
                    Log.fehlerMeldung(-102589463, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.filmeLaden", "kein Datum für: " + filmWebsite);
                }
                // public DatenFilm(String ssender, String tthema, String filmWebsite, String ttitel, String uurl, String datum, String zeit,
                //  long duration, String description, String thumbnailUrl, String imageUrl, String[] keywords) {
                addFilm(new DatenFilm(nameSenderMReader, thema, filmWebsite, titel, url, ""/*rtmpURL*/, datum, "", durationInSeconds, description, thumbnailUrl, imageUrl, keywords));
            }
        }

        private long extractDuration(MVStringBuilder page) {
            String duration = extractString(page, "<meta property=\"video:duration\" content=\"", "\"");
            if (duration == null) {
                return 0;
            }
            try {
                return Long.parseLong(duration);
            } catch (Exception ex) {
                return 0;
            }
        }

        private String extractDescription(MVStringBuilder page) {
            String desc = extractString(page, "<meta property=\"og:description\" content=\"", "\"");
            if (desc == null) {
                return "";
            }

            return desc;
        }

        private String[] extractKeywords(MVStringBuilder page) {
            String keywords = extractString(page, "<meta name=\"keywords\" content=\"", "\"");
            if (keywords == null) {
                return new String[]{""};
            }

            return keywords.split(", ");
        }

        private String extractThumbnailURL(MVStringBuilder page) {
            return extractString(page, "<meta itemprop=\"thumbnailURL\" content=\"", "\"");
        }

        private String extractImageURL(MVStringBuilder page) {
            return extractString(page, "<meta property=\"og:image\" content=\"", "\"");
        }

        protected String extractString(MVStringBuilder source, String startMarker, String endMarker) {
            int start = source.indexOf(startMarker);
            if (start == -1) {
                return null;
            }

            start = start + startMarker.length();

            int end = source.indexOf(endMarker, start);
            if (end == -1) {
                return null;
            }

            return source.substring(start, end);
        }
    }

    public static String convertDatum(String datum) {
        try {
            Date filmDate = new SimpleDateFormat("yyyyMMdd").parse(datum);
            datum = new SimpleDateFormat("dd.MM.yyyy").format(filmDate);
        } catch (Exception ex) {
            Log.fehlerMeldung(-979451236, Log.FEHLER_ART_MREADER, "MediathekArdPodcast.convertDatum", ex, "Datum: " + datum);
        }
        return datum;
    }
}
