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
package mediathek.controller.io;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.zip.ZipOutputStream;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import mediathek.controller.filmeLaden.importieren.DatenFilmlistenServer;
import mediathek.controller.filmeLaden.importieren.DatenUrlFilmliste;
import mediathek.controller.filmeLaden.importieren.FilmlistenSuchen;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.daten.ListeBlacklist;
import mediathek.daten.ListePset;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream;

public class IoXmlLesen {

    ZipOutputStream zipOutputStream = null;
    BZip2CompressorOutputStream bZip2CompressorOutputStream = null;

    public void datenLesen(DDaten daten) {
        xmlDatenLesen(daten);
    }

    public static boolean einstellungenExistieren() {
        try {
            String datei;
            datei = Daten.getBasisVerzeichnis(false) + Konstanten.XML_DATEI;
            if (new File(datei).exists()) {
                return true;
            }
        } catch (Exception ex) {
        }
        return false;
    }

    public static ListePset importPset(DDaten dd, String dateiUrl, boolean log) {
        int timeout = 10000; //10 Sekunden
        try {
            if (GuiFunktionen.istUrl(dateiUrl)) {
                URLConnection conn;
                conn = new URL(dateiUrl).openConnection();
                conn.setConnectTimeout(timeout);
                conn.setReadTimeout(timeout);
                conn.setRequestProperty("User-Agent", Daten.getUserAgent());
                return importPset(dd, conn.getInputStream(), log);
            } else {
                return importPset(dd, new FileInputStream(dateiUrl), log);
            }
        } catch (Exception ex) {
            if (log) {
                Log.fehlerMeldung(630048926, Log.FEHLER_ART_PROG, "IoXml.importPset", ex);
            }
            return null;
        }
    }

    public static ListePset importPset(DDaten dd, InputStream datei, boolean log) {
        DatenPset datenPset = null;
        ListePset liste = new ListePset();
        try {
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            XMLStreamReader parser;
            InputStreamReader in;
            in = new InputStreamReader(datei, Konstanten.KODIERUNG_UTF);
            parser = inFactory.createXMLStreamReader(in);
            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    //String t = parser.getLocalName();
                    if (parser.getLocalName().equals(DatenPset.PROGRAMMSET)) {
                        datenPset = new DatenPset();
                        if (!get(parser, event, DatenPset.PROGRAMMSET, DatenPset.PROGRAMMSET_COLUMN_NAMES, datenPset.arr, false)) {
                            datenPset = null;
                        } else {
                            liste.add(datenPset);
                        }
//                    } else if (parser.getLocalName().equals(Konstanten__old.PROGRAMMGRUPPE_BUTTON)) {
//                        DatenPgruppe__old datenPgruppe__old = new DatenPgruppe__old();
//                        if (!get(parser, event, Konstanten__old.PROGRAMMGRUPPE_BUTTON, Konstanten__old.PROGRAMMGRUPPE_COLUMN_NAMES, datenPgruppe__old.arr, false)) {
//                            datenPset = null;
//                        } else {
//                            datenPset = datenPgruppe__old.getNewVersion();
//                            liste.add(datenPset);
//                        }
                    } else if (parser.getLocalName().equals(DatenProg.PROGRAMM)) {
                        if (datenPset != null) {
                            DatenProg datenProg = new DatenProg();
                            if (get(parser, event, DatenProg.PROGRAMM, DatenProg.PROGRAMM_COLUMN_NAMES, datenProg.arr, false)) {
                                datenPset.addProg(datenProg);
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            if (log) {
                Log.fehlerMeldung(467810360, Log.FEHLER_ART_PROG, "IoXml.importPset", ex);
            }
            return null;
        }
        if (liste.size() == 0) {
            return null;
        } else {
            ListePset ll = new ListePset();
            ll.addVorlage(dd, liste);
            return ll;
        }
    }

    public static ListePset importPsetText(DDaten dd, String text, boolean log) {
        DatenPset datenPset = null;
        ListePset liste = new ListePset();
        try {
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            XMLStreamReader parser;
            InputStreamReader in = new InputStreamReader(new ByteArrayInputStream(text.getBytes()));
            parser = inFactory.createXMLStreamReader(in);
            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    //String t = parser.getLocalName();
                    if (parser.getLocalName().equals(DatenPset.PROGRAMMSET)) {
                        datenPset = new DatenPset();
                        if (!get(parser, event, DatenPset.PROGRAMMSET, DatenPset.PROGRAMMSET_COLUMN_NAMES, datenPset.arr, false)) {
                            datenPset = null;
                        } else {
                            liste.add(datenPset);
                        }
//                    } else if (parser.getLocalName().equals(Konstanten__old.PROGRAMMGRUPPE_BUTTON)) {
//                        DatenPgruppe__old datenPgruppe__old = new DatenPgruppe__old();
//                        if (!get(parser, event, Konstanten__old.PROGRAMMGRUPPE_BUTTON, Konstanten__old.PROGRAMMGRUPPE_COLUMN_NAMES, datenPgruppe__old.arr, false)) {
//                            datenPset = null;
//                        } else {
//                            datenPset = datenPgruppe__old.getNewVersion();
//                            liste.add(datenPset);
//                        }
//                    } else if (parser.getLocalName().equals(DatenProg.PROGRAMM)) {
                        if (datenPset != null) {
                            DatenProg datenProg = new DatenProg();
                            if (get(parser, event, DatenProg.PROGRAMM, DatenProg.PROGRAMM_COLUMN_NAMES, datenProg.arr, false)) {
                                datenPset.addProg(datenProg);
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            if (log) {
                Log.fehlerMeldung(100298325, Log.FEHLER_ART_PROG, "IoXml.importPset", ex);
            }
            return null;
        }
        if (liste.size() == 0) {
            return null;
        } else {
            ListePset ll = new ListePset();
            ll.addVorlage(dd, liste);
            return ll;
        }
    }

    // ##############################
    // private
    // ##############################
    private void xmlDatenLesen(DDaten ddaten) {
        try {
            String datei;
            datei = Daten.getBasisVerzeichnis(false) + Konstanten.XML_DATEI;
            if (new File(datei).exists()) {
                //nur wenn die Datei schon existiert
                int event;
                XMLInputFactory inFactory = XMLInputFactory.newInstance();
                inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
                XMLStreamReader parser;
                InputStreamReader in;
                DatenPset datenPset = null;
                in = new InputStreamReader(new FileInputStream(datei), Konstanten.KODIERUNG_UTF);
                parser = inFactory.createXMLStreamReader(in);
                while (parser.hasNext()) {
                    event = parser.next();
                    if (event == XMLStreamConstants.START_ELEMENT) {
                        //String t = parser.getLocalName();
                        if (parser.getLocalName().equals(Konstanten.SYSTEM)) {
                            //System
                            get(parser, event, Konstanten.SYSTEM, Konstanten.SYSTEM_COLUMN_NAMES, DDaten.system);
                        } else if (parser.getLocalName().equals(DatenPset.PROGRAMMSET)) {
                            //Programmgruppen
                            datenPset = new DatenPset();
                            if (get(parser, event, DatenPset.PROGRAMMSET, DatenPset.PROGRAMMSET_COLUMN_NAMES, datenPset.arr)) {
                                ddaten.listePset.add(datenPset);
                            }
                        } else if (parser.getLocalName().equals(DatenProg.PROGRAMM)) {
                            DatenProg datenProg = new DatenProg();
                            if (get(parser, event, DatenProg.PROGRAMM, DatenProg.PROGRAMM_COLUMN_NAMES, datenProg.arr)) {
                                if (datenPset != null) {
                                    datenPset.addProg(datenProg);
                                }
                            }
                            //ende Programgruppen
                        } else if (parser.getLocalName().equals(DatenAbo.ABO)) {
                            //Abo
                            DatenAbo datenAbo = new DatenAbo();
                            if (get(parser, event, DatenAbo.ABO, DatenAbo.ABO_COLUMN_NAMES, datenAbo.arr)) {
                                ddaten.listeAbo.addAbo(datenAbo);
                            }
                        } else if (parser.getLocalName().equals(DatenDownload.DOWNLOAD)) {
                            //Downloads
                            DatenDownload d = new DatenDownload();
                            if (get(parser, event, DatenDownload.DOWNLOAD, DatenDownload.DOWNLOAD_COLUMN_NAMES, d.arr)) {
                                ddaten.listeDownloads.add(d);
                            }
                        } else if (parser.getLocalName().equals(DatenBlacklist.BLACKLIST)) {
                            //Blacklist
                            ListeBlacklist blacklist = ddaten.listeBlacklist;
                            DatenBlacklist datenBlacklist = new DatenBlacklist();
                            if (get(parser, event, DatenBlacklist.BLACKLIST, DatenBlacklist.BLACKLIST_COLUMN_NAMES, datenBlacklist.arr)) {
                                blacklist.add(datenBlacklist);
                            }
                        } else if (parser.getLocalName().equals(FilmlistenSuchen.FILM_UPDATE_SERVER)) {
                            //Urls Filmlisten
                            DatenUrlFilmliste datenUrlFilmliste = new DatenUrlFilmliste();
                            if (get(parser, event, FilmlistenSuchen.FILM_UPDATE_SERVER, FilmlistenSuchen.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr)) {
                                DDaten.filmeLaden.getDownloadUrlsFilmlisten(false).addWithCheck(datenUrlFilmliste);
                            }
                        } else if (parser.getLocalName().equals(DatenFilmlistenServer.FILM_LISTEN_SERVER)) {
                            //Filmlisteserver
                            DatenFilmlistenServer datenFilmlistenServer = new DatenFilmlistenServer();
                            if (get(parser, event, DatenFilmlistenServer.FILM_LISTEN_SERVER, DatenFilmlistenServer.FILM_LISTEN_SERVER_COLUMN_NAMES, datenFilmlistenServer.arr)) {
                                DDaten.filmeLaden.getListeFilmlistnServer().add(datenFilmlistenServer);
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(392840096, Log.FEHLER_ART_PROG, "IoXml.xmlDatenLesen", ex);
        } finally {
            ddaten.listeDownloads.listeNummerieren();
            //ListeFilmUpdateServer aufbauen
            DDaten.filmeLaden.getDownloadUrlsFilmlisten(false).sort();
        }
    }

    private static boolean get(XMLStreamReader parser, int event, String xmlElem, String[] xmlNames, String[] strRet) {
        return get(parser, event, xmlElem, xmlNames, strRet, true);
    }

    private static boolean get(XMLStreamReader parser, int event, String xmlElem, String[] xmlNames, String[] strRet, boolean log) {
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
                event = parser.next();
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
                Log.fehlerMeldung(739530149, Log.FEHLER_ART_PROG, "IoXmlLesen.get", ex);
            }
        }
        return ret;
    }
}
