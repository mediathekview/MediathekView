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
import mSearch.filmlisten.DatenFilmlisteUrl;
import mSearch.tool.MSConst;
import mSearch.tool.MSLog;
import mediathek.daten.*;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVReplaceList;

public class IoXmlLesen {

    public static boolean datenLesen(Path xmlFilePath) {
        boolean ret = false;
        if (Files.exists(xmlFilePath)) {
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            DatenPset datenPset = null;
            try (InputStreamReader in = new InputStreamReader(Files.newInputStream(xmlFilePath), MSConst.KODIERUNG_UTF)) {
                XMLStreamReader parser = inFactory.createXMLStreamReader(in);
                while (parser.hasNext()) {
                    event = parser.next();
                    if (event == XMLStreamConstants.START_ELEMENT) {
                        //String t = parser.getLocalName();
                        switch (parser.getLocalName()) {
                            case MVConfig.SYSTEM:
                                //System
                                getConfig(parser, MVConfig.SYSTEM, Daten.mVConfig, true);
                                break;
                            case DatenPset.PROGRAMMSET:
                                //Programmgruppen
                                datenPset = new DatenPset();
                                if (get(parser, DatenPset.PROGRAMMSET, DatenPset.COLUMN_NAMES_, datenPset.arr)) {
                                    Daten.listePset.add(datenPset);
                                }
                                break;
                            case DatenProg.PROGRAMM:
                                DatenProg datenProg = new DatenProg();
                                if (get(parser, DatenProg.PROGRAMM, DatenProg.COLUMN_NAMES_, datenProg.arr)) {
                                    if (datenPset != null) {
                                        datenPset.addProg(datenProg);
                                    }
                                }
                                //ende Programgruppen
                                break;
                            case MVReplaceList.REPLACELIST:
                                // Ersetzungstabelle
                                String[] sa = new String[MVReplaceList.MAX_ELEM];
                                if (get(parser, MVReplaceList.REPLACELIST, MVReplaceList.COLUMN_NAMES, sa)) {
                                    Daten.mVReplaceList.list.add(sa);
                                }
                                break;
                            case DatenAbo.ABO:
                                //Abo
                                DatenAbo datenAbo = new DatenAbo();
                                if (get(parser, DatenAbo.ABO, DatenAbo.COLUMN_NAMES, datenAbo.arr)) {
                                    Daten.listeAbo.addAbo(datenAbo);
                                }
                                break;
                            case DatenDownload.DOWNLOAD:
                                //Downloads
                                DatenDownload d = new DatenDownload();
                                if (get(parser, DatenDownload.DOWNLOAD, DatenDownload.COLUMN_NAMES_, d.arr)) {
                                    d.init();
                                    Daten.listeDownloads.add(d);
                                }
                                break;
                            case DatenBlacklist.BLACKLIST:
                                //Blacklist
                                ListeBlacklist blacklist = Daten.listeBlacklist;
                                DatenBlacklist datenBlacklist = new DatenBlacklist();
                                if (get(parser, DatenBlacklist.BLACKLIST, DatenBlacklist.BLACKLIST_COLUMN_NAMES, datenBlacklist.arr)) {
                                    blacklist.add(datenBlacklist);
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
                MSLog.fehlerMeldung(392840096, ex);
            }
            Daten.listeDownloads.listeNummerieren();
            //ListeFilmUpdateServer aufbauen
            Daten.filmeLaden.getDownloadUrlsFilmlisten_akt().sort();
            Daten.filmeLaden.getDownloadUrlsFilmlisten_diff().sort();
        }
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
            in = new InputStreamReader(new FileInputStream(datei), MSConst.KODIERUNG_UTF);
            parser = inFactory.createXMLStreamReader(in);
            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    //String t = parser.getLocalName();
                    if (abo && parser.getLocalName().equals(DatenAbo.ABO)) {
                        //Abo
                        DatenAbo datenAbo = new DatenAbo();
                        if (get(parser, DatenAbo.ABO, DatenAbo.COLUMN_NAMES, datenAbo.arr)) {
                            ++found[0];
                            Daten.listeAbo.addAbo(datenAbo);
                        }
                    } else if (black && parser.getLocalName().equals(DatenBlacklist.BLACKLIST)) {
                        //Blacklist
                        ListeBlacklist blacklist = Daten.listeBlacklist;
                        DatenBlacklist datenBlacklist = new DatenBlacklist();
                        if (get(parser, DatenBlacklist.BLACKLIST, DatenBlacklist.BLACKLIST_COLUMN_NAMES, datenBlacklist.arr)) {
                            ++found[1];
                            blacklist.add(datenBlacklist);
                        }
                    } else if (replace && parser.getLocalName().equals(MVReplaceList.REPLACELIST)) {
                        //Ersetzungstabelle
                        String[] sa = new String[MVReplaceList.MAX_ELEM];
                        if (get(parser, MVReplaceList.REPLACELIST, MVReplaceList.COLUMN_NAMES, sa)) {
                            ++found[2];
                            Daten.mVReplaceList.list.add(sa);
                        }
                    }
                }
            }
            in.close();
        } catch (Exception ex) {
            MSLog.fehlerMeldung(302045698, ex);
        }
        if (found[0] > 0) {
            Daten.listeAbo.aenderungMelden();
        }
        if (found[2] > 0) {
            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_REPLACELIST_CHANGED, IoXmlLesen.class.getSimpleName());
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
                MSLog.fehlerMeldung(739530149, ex);
            }
        }
        return ret;
    }

    private static boolean getConfig(XMLStreamReader parser, String xmlElem, MVConfig mVConfig, boolean log) {
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
                    mVConfig.add(s, n);
                }
            }
        } catch (Exception ex) {
            ret = false;
            if (log) {
                MSLog.fehlerMeldung(945120369, ex);
            }
        }
        return ret;
    }
}
