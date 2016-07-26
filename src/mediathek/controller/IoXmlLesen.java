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
package mediathek.controller;

import java.io.FileInputStream;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.nio.file.Path;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import mSearch.Const;
import mSearch.filmlisten.DatenFilmlisteUrl;
import mSearch.tool.Duration;
import mSearch.tool.Listener;
import mSearch.tool.Log;
import mSearch.tool.ReplaceList;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.*;

public class IoXmlLesen {

    public static boolean datenLesen(Path xmlFilePath) {
        Duration.counterStart("Konfig lesen");
        boolean ret = false;
        if (Files.exists(xmlFilePath)) {
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            DatenPset datenPset = null;
            try (InputStreamReader in = new InputStreamReader(Files.newInputStream(xmlFilePath), Const.KODIERUNG_UTF)) {
                //XMLStreamReader parser = inFactory.createXMLStreamReader(new BufferedReader(in, 25_000));
                XMLStreamReader parser = inFactory.createXMLStreamReader(in);
                while (parser.hasNext()) {
                    event = parser.next();
                    if (event == XMLStreamConstants.START_ELEMENT) {
                        //String t = parser.getLocalName();
                        switch (parser.getLocalName()) {
                            case MVConfig.SYSTEM:
                                //System
                                getConfig(parser, MVConfig.SYSTEM);
                                break;
                            case DatenPset.TAG:
                                //Programmgruppen
                                datenPset = new DatenPset();
                                if (get(parser, DatenPset.TAG, DatenPset.XML_NAMES, datenPset.arr)) {
                                    Daten.listePset.add(datenPset);
                                }
                                break;
                            case DatenProg.TAG:
                                DatenProg datenProg = new DatenProg();
                                if (get(parser, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.arr)) {
                                    if (datenPset != null) {
                                        datenPset.addProg(datenProg);
                                    }
                                }
                                //ende Programgruppen
                                break;
                            case ReplaceList.REPLACELIST:
                                // Ersetzungstabelle
                                String[] sa = new String[ReplaceList.MAX_ELEM];
                                if (get(parser, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa)) {
                                    ReplaceList.list.add(sa);
                                }
                                break;
                            case DatenAbo.TAG:
                                //Abo
                                DatenAbo datenAbo = new DatenAbo();
                                if (get(parser, DatenAbo.TAG, DatenAbo.XML_NAMES, datenAbo.arr)) {
                                    Daten.listeAbo.addAbo(datenAbo);
                                }
                                break;
                            case DatenDownload.TAG:
                                //Downloads
                                DatenDownload d = new DatenDownload();
                                if (get(parser, DatenDownload.TAG, DatenDownload.XML_NAMES, d.arr)) {
                                    d.init();
                                    Daten.listeDownloads.add(d);
                                }
                                break;
                            case DatenBlacklist.TAG:
                                //Blacklist
                                DatenBlacklist datenBlacklist = new DatenBlacklist();
                                if (get(parser, DatenBlacklist.TAG, DatenBlacklist.XML_NAMES, datenBlacklist.arr)) {
                                    Daten.listeBlacklist.addProgStart(datenBlacklist);
                                }
                                break;
                            case DatenFilmlisteUrl.FILM_UPDATE_SERVER:
                                //Urls Filmlisten
                                DatenFilmlisteUrl datenFilmlisteUrl = new DatenFilmlisteUrl();
                                if (get(parser, DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenFilmlisteUrl.arr)) {
                                    switch (datenFilmlisteUrl.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR]) {
                                        case DatenFilmlisteUrl.SERVER_ART_AKT:
                                            Daten.filmeLaden.getDownloadUrlsFilmlisten_akt().addWithCheck(datenFilmlisteUrl);
                                            break;
                                        case DatenFilmlisteUrl.SERVER_ART_DIFF:
                                            Daten.filmeLaden.getDownloadUrlsFilmlisten_diff().addWithCheck(datenFilmlisteUrl);
                                            break;
                                    }
                                }
                                break;
                        }
                    }
                }
                parser.close();
                ret = true;
            } catch (Exception ex) {
                ret = false;
                Log.errorLog(392840096, ex);
            }
            Daten.listeDownloads.listeNummerieren();
            Daten.listeAbo.sort();
            //ListeFilmUpdateServer aufbauen
            Daten.filmeLaden.getDownloadUrlsFilmlisten_akt().sort();
            Daten.filmeLaden.getDownloadUrlsFilmlisten_diff().sort();
        }

        Duration.counterStop("Konfig lesen");
        return ret;
    }

    public static boolean einstellungenExistieren() {
        Path xmlFilePath = Daten.getMediathekXmlFilePath();
        return Files.exists(xmlFilePath);
    }

    public static int[] importAboBlacklist(String datei, boolean abo, boolean black, boolean replace) {
        int[] found = new int[]{0, 0, 0};
        try {
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            XMLStreamReader parser;
            InputStreamReader in;
            in = new InputStreamReader(new FileInputStream(datei), Const.KODIERUNG_UTF);
            parser = inFactory.createXMLStreamReader(in);
            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    //String t = parser.getLocalName();
                    if (abo && parser.getLocalName().equals(DatenAbo.TAG)) {
                        //Abo
                        DatenAbo datenAbo = new DatenAbo();
                        if (get(parser, DatenAbo.TAG, DatenAbo.XML_NAMES, datenAbo.arr)) {
                            ++found[0];
                            Daten.listeAbo.addAbo(datenAbo);
                        }
                    } else if (black && parser.getLocalName().equals(DatenBlacklist.TAG)) {
                        //Blacklist
                        ListeBlacklist blacklist = Daten.listeBlacklist;
                        DatenBlacklist datenBlacklist = new DatenBlacklist();
                        if (get(parser, DatenBlacklist.TAG, DatenBlacklist.XML_NAMES, datenBlacklist.arr)) {
                            ++found[1];
                            blacklist.addProgStart(datenBlacklist);
                        }
                    } else if (replace && parser.getLocalName().equals(ReplaceList.REPLACELIST)) {
                        //Ersetzungstabelle
                        String[] sa = new String[ReplaceList.MAX_ELEM];
                        if (get(parser, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa)) {
                            ++found[2];
                            ReplaceList.list.add(sa);
                        }
                    }
                }
            }
            in.close();
        } catch (Exception ex) {
            Log.errorLog(302045698, ex);
        }
        if (found[0] > 0) {
            Daten.listeAbo.aenderungMelden();
        }
        if (found[1] > 0) {
            Daten.listeBlacklist.notifyBlack();
        }
        if (found[2] > 0) {
            Listener.notify(Listener.EREIGNIS_REPLACELIST_CHANGED, IoXmlLesen.class.getSimpleName());
        }
        return found;
    }

    // ##############################
    // private
    // ##############################
    private static boolean get(XMLStreamReader parser, String xmlElem, String[] xmlNames, String[] strRet) {
        return get(parser, xmlElem, xmlNames, strRet, true);
    }

    private static boolean get(XMLStreamReader parser, String xmlElem, String[] xmlNames, String[] strRet, boolean log) {
        boolean ret = true;
        int maxElem = strRet.length;
        for (int i = 0; i < maxElem; ++i) {
            if (strRet[i] == null) {
                // damit Vorgaben nicht verschwinden!
                strRet[i] = "";
            }
        }
        try {
            while (parser.hasNext()) {
                int event = parser.next();
                if (event == XMLStreamConstants.END_ELEMENT) {
                    if (parser.getLocalName().equals(xmlElem)) {
                        break;
                    }
                }
                if (event == XMLStreamConstants.START_ELEMENT) {
                    for (int i = 0; i < maxElem; ++i) {
                        if (parser.getLocalName().equals(xmlNames[i])) {
                            strRet[i] = parser.getElementText();
                            break;
                        }
                    }
                }
            }
        } catch (Exception ex) {
            ret = false;
            if (log) {
                Log.errorLog(739530149, ex);
            }
        }
        return ret;
    }

    private static boolean getConfig(XMLStreamReader parser, String xmlElem) {
        boolean ret = true;
        try {
            while (parser.hasNext()) {
                int event = parser.next();
                if (event == XMLStreamConstants.END_ELEMENT) {
                    if (parser.getLocalName().equals(xmlElem)) {
                        break;
                    }
                }
                if (event == XMLStreamConstants.START_ELEMENT) {
                    String s = parser.getLocalName();
                    String n = parser.getElementText();
                    MVConfig.add(s, n);
                }
            }
        } catch (Exception ex) {
            ret = false;
            Log.errorLog(945120369, ex);
        }
        return ret;
    }
//
//    private static void readNode(Node sub, String[] arr, String[] XML_NAMES) {
//        NodeList subList = sub.getChildNodes();
//        if (subList == null) {
//            return;
//        }
//        for (int ii = 0; ii < subList.getLength(); ++ii) {
//            final Node subnode = subList.item(ii);
//            for (int iii = 0; iii < XML_NAMES.length; ++iii) {
//                if (subnode.getNodeName().equals(XML_NAMES[iii])) {
//                    arr[iii] = subnode.getTextContent();
//                    break;
//                }
//            }
//        }
//    }
//
//    private static void readNodeConfig(Node sub) {
//        NodeList subList = sub.getChildNodes();
//        if (subList == null) {
//            return;
//        }
//        for (int ii = 0; ii < subList.getLength(); ++ii) {
//            final Node subnode = subList.item(ii);
//            if (!subnode.getTextContent().isEmpty()) {
//                MVConfig.add(subnode.getNodeName(), subnode.getTextContent());
//            }
//        }
//    }
//
//    public static boolean datenLesen_(Path xmlFilePath) {
//        Duration.counterStart("Konfig lesen - neu");
//        Document doc = null;
//        boolean ret = false;
//        try {
//            if (Files.exists(xmlFilePath)) {
//
//                final DocumentBuilderFactory dbf = DocumentBuilderFactory.newInstance();
//                dbf.setNamespaceAware(true);
//                final DocumentBuilder db = dbf.newDocumentBuilder();
//                doc = db.parse(xmlFilePath.toFile());
//
//                Element root = doc.getDocumentElement();
//                NodeList nlist = root.getChildNodes();
//                Node sub;
//                for (int i = 0; i < nlist.getLength(); i++) {
//                    sub = nlist.item(i);
//
//                    DatenPset datenPset = null;
////                    System.out.println(sub.getNodeName());
//                    switch (sub.getNodeName()) {
//                        case MVConfig.SYSTEM:
//                            //System
//                            readNodeConfig(sub);
//
//                            break;
//                        case DatenPset.TAG:
//                            //Programmgruppen
//                            datenPset = new DatenPset();
//                            readNode(sub, datenPset.arr, DatenPset.XML_NAMES);
//
//                            Daten.listePset.add(datenPset);
//                            break;
//                        case DatenProg.TAG:
//                            DatenProg datenProg = new DatenProg();
//                            readNode(sub, datenProg.arr, DatenProg.XML_NAMES);
//                            if (datenPset != null) {
//                                datenPset.addProg(datenProg);
//                            }
//                            //ende Programgruppen
//                            break;
//                        case ReplaceList.REPLACELIST:
//                            // Ersetzungstabelle
//                            String[] sa = new String[ReplaceList.MAX_ELEM];
//                            readNode(sub, sa, ReplaceList.COLUMN_NAMES);
//                            ReplaceList.list.add(sa);
//                            break;
//                        case DatenAbo.TAG:
//                            //Abo
//                            DatenAbo datenAbo = new DatenAbo();
//                            readNode(sub, datenAbo.arr, DatenAbo.XML_NAMES);
//                            Daten.listeAbo.addAbo(datenAbo);
//                            break;
//                        case DatenDownload.TAG:
//                            //Downloads
//                            DatenDownload d = new DatenDownload();
//                            readNode(sub, d.arr, DatenDownload.XML_NAMES);
//                            d.init();
//                            Daten.listeDownloads.add(d);
//                            break;
//                        case DatenBlacklist.TAG:
//                            //Blacklist
//                            DatenBlacklist datenBlacklist = new DatenBlacklist();
//                            readNode(sub, datenBlacklist.arr, DatenBlacklist.XML_NAMES);
//                            Daten.listeBlacklist.addProgStart(datenBlacklist);
//                            break;
//                        case DatenFilmlisteUrl.FILM_UPDATE_SERVER:
//                            //Urls Filmlisten
//                            DatenFilmlisteUrl datenFilmlisteUrl = new DatenFilmlisteUrl();
//                            readNode(sub, datenFilmlisteUrl.arr, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES);
//
//                            switch (datenFilmlisteUrl.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR]) {
//                                case DatenFilmlisteUrl.SERVER_ART_AKT:
//                                    Daten.filmeLaden.getDownloadUrlsFilmlisten_akt().addWithCheck(datenFilmlisteUrl);
//                                    break;
//                                case DatenFilmlisteUrl.SERVER_ART_DIFF:
//                                    Daten.filmeLaden.getDownloadUrlsFilmlisten_diff().addWithCheck(datenFilmlisteUrl);
//                                    break;
//                            }
//                    }
//
//                }
//
//                Daten.listeDownloads.listeNummerieren();
//                Daten.listeAbo.sort();
//                //ListeFilmUpdateServer aufbauen
//                Daten.filmeLaden.getDownloadUrlsFilmlisten_akt().sort();
//                Daten.filmeLaden.getDownloadUrlsFilmlisten_diff().sort();
//            }
//            ret = true;
//        } catch (Exception ignore) {
//            ret = false;
//        }
//        Duration.counterStop("Konfig lesen - neu");
//        return ret;
//    }

}
