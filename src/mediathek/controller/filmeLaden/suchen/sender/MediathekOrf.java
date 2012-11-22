/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 *
 * thausherr
 *
 *
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

import java.io.IOException;
import java.io.StringReader;
import java.net.URLDecoder;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import mediathek.controller.filmeLaden.suchen.FilmeSuchenSender;
import mediathek.controller.io.GetUrl;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 *
 * @author
 */
public class MediathekOrf extends MediathekReader implements Runnable {

    public static final String SENDER = "ORF";
    private final String ROOTURL = "http://tvthek.orf.at";
    private final String TOPICURL = "http://tvthek.orf.at/topics";

    /**
     *
     * @param ddaten
     */
    public MediathekOrf(FilmeSuchenSender ssearch, int startPrio) {
        super(ssearch, /* name */ SENDER, /* threads */ 2, /* urlWarten */ 500, startPrio);
    }

    @Override
    void addToList() {
        StringBuffer seite = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        listeThemen.clear();
        meldungStart();
        bearbeiteAdresse(TOPICURL, seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/archiv", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/monday", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/tuesday", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/wednesday", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/thursday", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/friday", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/saturday", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/sunday", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/monday_prev", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/tuesday_prev", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/wednesday_prev", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/thursday_prev", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/friday_prev", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/saturday_prev", seite);
        bearbeiteAdresse("http://tvthek.orf.at/schedule/last/sunday_prev", seite);
        if (Daten.filmeLaden.getStop()) {
            meldungThreadUndFertig();
        } else if (listeThemen.size() == 0) {
            meldungThreadUndFertig();
        } else {
            meldungAddMax(listeThemen.size());
            listeSort(listeThemen, 1);
            for (int t = 0; t < maxThreadLaufen; ++t) {
                new Thread(new OrfThemaLaden()).start();
            }
        }
    }

    /**
     * @param adresse Starter-URL von dem aus Sendungen gefunden werden
     */
    private void bearbeiteAdresse(String adresse, StringBuffer seite) {
        //System.out.println("bearbeiteAdresse: " + adresse);
        final String MUSTER_URL1 = "<a href=\""; //TH
        final String MUSTER_URL2 = "/programs/";
        final String MUSTER_URL2b = "/topics/"; //TH
        seite = getUrlIo.getUri(nameSenderMReader, adresse, Konstanten.KODIERUNG_UTF, 3, seite, "");
        int pos = 0;
        int pos1;
        int pos2;
        String url = "";
        String thema = "";
        //Podcasts auslesen
        while ((pos = seite.indexOf(MUSTER_URL1, pos)) != -1) {
            try {
                pos += MUSTER_URL1.length();
                //TH
                String m = MUSTER_URL2;
                int p = seite.indexOf(m, pos);
                if (p == -1) {
                    // Plan B
                    m = MUSTER_URL2b;
                    p = seite.indexOf(m, pos);
                }
                pos = p;
                //TH ende
                if (pos != -1) {
                    pos += m.length(); //TH
                    pos1 = pos;
                    pos2 = seite.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = seite.substring(pos1, pos2);
                    }
                    //TH neu: " title="ZIB 24: Spott aus Litauen">
                    pos1 = seite.indexOf("title=\"", pos) + 6; //TH
                    pos2 = seite.indexOf("\">", pos); //TH
                    if (pos1 != -1 && pos2 != -1 && pos1 < pos2) {
                        thema = seite.substring(pos1 + 1, pos2);
                        //TH
                        if (thema.endsWith(" aufrufen...")) {
                            thema = thema.replace(" aufrufen...", "");
                        }
                        //TH 31.7.2012
                        if (thema.endsWith(" ansehen...")) {
                            thema = thema.replace(" ansehen...", "");
                        }
                    }
                    if (url.equals("")) {
                        continue;
                    }
                    String[] add = new String[]{
                        ROOTURL + m + url, thema //TH
                    };
                    if (!istInListe(listeThemen, add[0], 0)) {
                        //System.out.println ("URL: " + add[0] + ", Thema: " + add[1]);
                        listeThemen.add(add);
                    }
                } else {
                    break; //TH muss sein da muster 2 manchmal nicht fündig - dann Endlosschleife
                }
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-896234580, "MediathekOrf.addToList", ex.getMessage());
            }
        }

        //TH 31.7.2012 Rekursive Sonderrunde für weitere topics ("alle Anzeigen")
        if (adresse.equals(TOPICURL)) {
            final String MUSTERURL_MORE = "<a class=\"more\" href=\"";
            pos = 0;
            StringBuffer s2 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER); // zum Reduzieren der StringBuffer
            while ((pos = seite.indexOf(MUSTERURL_MORE, pos)) != -1) {
                try {
                    pos += MUSTERURL_MORE.length();
                    pos1 = pos;
                    pos2 = seite.indexOf("\"", pos);
                    if (pos1 != -1 && pos2 != -1) {
                        url = ROOTURL + seite.substring(pos1, pos2);
                        if (!url.equals(adresse)) {
                            bearbeiteAdresse(url, s2);
                        }
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldungMReader(-468320478, "MediathekOrf.addToList", ex.getMessage());
                }
            }
        }
    }

    private class OrfThemaLaden implements Runnable {

        GetUrl getUrl = new GetUrl(wartenSeiteLaden);
        private StringBuffer seite1 = new StringBuffer(Konstanten.STRING_BUFFER_START_BUFFER);
        //private StringBuffer seiteAsx = new StringBuffer();

        @Override
        public synchronized void run() {
            try {
                meldungAddThread();
                String[] link;
                while (!Daten.filmeLaden.getStop() && (link = listeThemen.getListeThemen()) != null) {
                    try {
                        meldungProgress(link[0]);
                        feedEinerSeiteSuchen(link[0] /* url */, link[1] /* Thema */);
                    } catch (Exception ex) {
                        Log.fehlerMeldungMReader(-795633581, "MediathekOrf.OrfThemaLaden.run", ex.getMessage());
                    }
                }
                meldungThreadUndFertig();
            } catch (Exception ex) {
                Log.fehlerMeldungMReader(-554012398, "MediathekOrf.OrfThemaLaden.run", ex.getMessage());
            }
        }

        void feedEinerSeiteSuchen(String strUrlFeed, String thema) {
            //<title> ORF TVthek: a.viso - 28.11.2010 09:05 Uhr</title>
            final String MUSTER_DATUM_1 = "<span>"; //TH
            final String MUSTER_DATUM_2 = "Uhr</span>"; //TH
            seite1 = getUrl.getUri_Utf(nameSenderMReader, strUrlFeed, seite1, "Thema: " + thema);
            int pos = 0;
            int pos1;
            int pos2;
            String datum = "";
            String zeit = "";
            String tmp;

            if ((pos1 = seite1.indexOf(MUSTER_DATUM_1)) != -1) {
                pos1 += MUSTER_DATUM_1.length();
                if ((pos2 = seite1.indexOf(MUSTER_DATUM_2, pos1)) != -1) {
                    tmp = seite1.substring(pos1, pos2);
                    if (tmp.contains("-")) {
                        tmp = tmp.substring(tmp.lastIndexOf("-") + 1).trim();
                        if (tmp.contains(" ")) {
                            datum = tmp.substring(0, tmp.indexOf(" ")).trim();
                            zeit = tmp.substring(tmp.indexOf(" "));
                            zeit = zeit.replace("Uhr", "").trim() + ":00";
                        }
                    }
                }
            }
            //TH ggf. Trennen in Thema und Titel
            int dp = thema.indexOf(": ");
            if (dp != -1) {
                thema = thema.substring(0, dp);
            }//TH titel und thema getrennt

            //TH 27.8.2012 JS XML Variable auswerten
            final String MUSTER_FLASH = "ORF.flashXML = '";
            if ((pos = seite1.indexOf(MUSTER_FLASH)) != -1) {
                if ((pos2 = seite1.indexOf("'", pos + MUSTER_FLASH.length())) != -1) {
                    String xml = seite1.substring(pos + MUSTER_FLASH.length(), pos2);
                    try {
                        xml = URLDecoder.decode(xml, "UTF-8");
                        DocumentBuilder docBuilder = null;
                        docBuilder = DocumentBuilderFactory.newInstance().newDocumentBuilder();
                        Document doc = docBuilder.parse(new InputSource(new StringReader(xml)));
                        Node rootNode = doc.getDocumentElement();
                        NodeList nodeList = rootNode.getChildNodes();
                        for (int i = 0; i < nodeList.getLength(); ++i) {
                            Node Item = nodeList.item(i);
                            if ("Playlist".equals(Item.getNodeName())) {
                                NodeList childNodeList = Item.getChildNodes();
                                for (int j = 0; j < childNodeList.getLength(); ++j) {
                                    Node childItem = childNodeList.item(j);
                                    if ("Items".equals(childItem.getNodeName())) {
                                        NodeList childNodeList2 = childItem.getChildNodes();
                                        for (int k = 0; k < childNodeList2.getLength(); ++k) {
                                            Node childItem2 = childNodeList2.item(k);
                                            if ("Item".equals(childItem2.getNodeName())) {
                                                String url = "";
                                                String titel = "";
                                                NodeList childNodeList3 = childItem2.getChildNodes();
                                                for (int l = 0; l < childNodeList3.getLength(); ++l) {
                                                    Node childItem3 = childNodeList3.item(l);
                                                    if ("Title".equals(childItem3.getNodeName())) {
                                                        titel = childItem3.getTextContent();
                                                    }
                                                    if ("VideoUrl".equals(childItem3.getNodeName())) {
                                                        String quality = null;
                                                        NamedNodeMap namedNodeMap = childItem3.getAttributes();
                                                        if (namedNodeMap != null) {
                                                            Node node = namedNodeMap.getNamedItem("quality");
                                                            if (node != null) {
                                                                quality = node.getNodeValue();
                                                            }
                                                        }
                                                        // "SMIL"-Qualität nehmen, oder "keine Qualität"
                                                        if ((quality == null && titel.isEmpty()) || "SMIL".equals(quality)) {
                                                            url = childItem3.getTextContent();
                                                        }
                                                    }
                                                }
                                                if (!url.isEmpty() && !titel.isEmpty()) {
                                                    String urlRtmp = "";
                                                    int mpos = url.indexOf("mp4:");
                                                    if (mpos != -1) {
                                                        urlRtmp = "-r " + url + " -y " + url.substring(mpos) + " --flashVer WIN11,4,402,265 --swfUrl http://tvthek.orf.at/flash/player/TVThekPlayer_9_ver18_1.swf";
                                                    }
                                                    //rtmp://apasfw.apa.at/cms-worldwide/mp4:2012-09-09_1305_tl_23_UNGARISCHES-MAGAZIN_Beszelgetes-Szabo-Er__4582591__o__0000214447__s4588253___n__BHiRes_13241400P_13280400P_Q6A.mp4
                                                    //flvr=WIN11,4,402,265
                                                    //app=cms-worldwide/
                                                    //swfUrl=http://tvthek.orf.at/flash/player/TVThekPlayer_9_ver18_1.swf
                                                    //tcUrl=rtmp://apasfw.apa.at/%app%
                                                    //play=mp4:1950-01-01_1200_in_00_Ungarnkrise-1956_____3230831__o__0000936285__s3230833___Q6A.mp4
                                                    //flvstreamer --resume --rtmp %tcUrl% --flashVer %flvr% --app %app% --swfUrl %swfUrl% --playpath %play% --flv %Ziel%



                                                    //addFilm(new DatenFilm(senderName, thema, strUrlFeed, titel, url, datum, zeit));
                                                    addFilm(new DatenFilm(nameSenderMReader, thema, strUrlFeed, titel, url, urlRtmp, datum, zeit));
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } catch (SAXException ex) {
                        Log.fehlerMeldungMReader(-643206531, "MediathekOrf.feedEinerSeiteSuchen", ex.getMessage());
                    } catch (IOException ex) {
                        Log.fehlerMeldungMReader(-201456987, "MediathekOrf.feedEinerSeiteSuchen", ex.getMessage());
                    } catch (ParserConfigurationException ex) {
                        Log.fehlerMeldungMReader(-121036907, "MediathekOrf.feedEinerSeiteSuchen", ex.getMessage());
                    }
                }
            }
        }
    }
}
