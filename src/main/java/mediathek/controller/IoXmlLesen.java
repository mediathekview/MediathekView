/*
 * MediathekView Copyright (C) 2008 W. Xaver W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or any later version.
 *
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License along with this program. If
 * not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.*;
import mediathek.gui.messages.ReplaceListChangedEvent;
import mediathek.tool.ReplaceList;
import org.apache.commons.lang3.tuple.ImmutableTriple;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;


public class IoXmlLesen {
    private static final Logger logger = LogManager.getLogger(IoXmlLesen.class);
    private final XMLInputFactory inFactory;
    private final Daten daten;

    public IoXmlLesen() {
        inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);

        daten = Daten.getInstance();
    }

    public ImmutableTriple<Integer, Integer, Integer> importAboBlacklist(String datei, boolean abo, boolean black,
                                                                         boolean replace) throws IOException, XMLStreamException {
        int foundAbos = 0;
        int foundBlacklistEntries = 0;
        int foundReplaceListEntries = 0;
        XMLStreamReader parser = null;

        try (FileInputStream fis = new FileInputStream(datei);
             InputStreamReader in = new InputStreamReader(fis, StandardCharsets.UTF_8)) {

            parser = inFactory.createXMLStreamReader(in);
            while (parser.hasNext()) {
                final int event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    if (abo && parser.getLocalName().equals(DatenAbo.TAG)) {
                        // Abo
                        DatenAbo datenAbo = new DatenAbo();
                        if (get(parser, DatenAbo.TAG, DatenAbo.XML_NAMES, datenAbo.arr)) {
                            foundAbos++;
                            daten.getListeAbo().addAbo(datenAbo);
                        }
                    } else if (black && parser.getLocalName().equals(DatenBlacklist.TAG)) {
                        // Blacklist
                        ListeBlacklist blacklist = daten.getListeBlacklist();
                        DatenBlacklist datenBlacklist = new DatenBlacklist();
                        if (get(parser, DatenBlacklist.TAG, DatenBlacklist.XML_NAMES, datenBlacklist.arr)) {
                            foundBlacklistEntries++;
                            blacklist.addWithoutNotification(datenBlacklist);
                        }
                    } else if (replace && parser.getLocalName().equals(ReplaceList.REPLACELIST)) {
                        // Ersetzungstabelle
                        String[] sa = new String[ReplaceList.MAX_ELEM];
                        if (get(parser, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa)) {
                            foundReplaceListEntries++;
                            ReplaceList.list.add(sa);
                        }
                    }
                }
            }
        } finally {
            if (parser != null) {
                try {
                    parser.close();
                } catch (XMLStreamException ignored) {
                }
            }
        }
        if (foundAbos > 0) {
            daten.getListeAbo().aenderungMelden(true); 
        }

        if (foundBlacklistEntries > 0)
            daten.getListeBlacklist().filterListAndNotifyListeners();

        if (foundReplaceListEntries > 0)
            daten.getMessageBus().publishAsync(new ReplaceListChangedEvent());

        return new ImmutableTriple<>(foundAbos, foundBlacklistEntries, foundReplaceListEntries);
    }

    private boolean get(XMLStreamReader parser, String xmlElem, String[] xmlNames,
                        String[] strRet) {
        boolean ret = true;
        final int maxElem = strRet.length;
        for (int i = 0; i < maxElem; ++i) {
            if (strRet[i] == null) {
                // damit Vorgaben nicht verschwinden!
                strRet[i] = "";
            }
        }
        try {
            while (parser.hasNext()) {
                final int event = parser.next();
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
            logger.error("get", ex);
        }
        return ret;
    }

    private void readSystemConfiguration(XMLStreamReader parser) {
        try {
            while (parser.hasNext()) {
                int event = parser.next();
                if (event == XMLStreamConstants.END_ELEMENT) {
                    if (parser.getLocalName().equals(MVConfig.SYSTEM)) {
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
            logger.error("readSystemConfiguration", ex);
        }
    }

    private void readReplacementList(XMLStreamReader parser) {
        String[] sa = new String[ReplaceList.MAX_ELEM];
        if (get(parser, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa)) {
            ReplaceList.list.add(sa);
        }
    }

    private void readAbos(XMLStreamReader parser) {
        DatenAbo datenAbo = new DatenAbo();
        if (get(parser, DatenAbo.TAG, DatenAbo.XML_NAMES, datenAbo.arr)) {
            daten.getListeAbo().addAbo(datenAbo);
        }
    }

    private void readDownloads(XMLStreamReader parser) {
        // Downloads
        DatenDownload d = new DatenDownload();
        if (get(parser, DatenDownload.TAG, DatenDownload.XML_NAMES, d.arr)) {
            d.init();
            daten.getListeDownloads().add(d);
        }
    }

    private void readBlacklist(XMLStreamReader parser) {
        // Blacklist
        DatenBlacklist datenBlacklist = new DatenBlacklist();
        if (get(parser, DatenBlacklist.TAG, DatenBlacklist.XML_NAMES, datenBlacklist.arr)) {
            daten.getListeBlacklist().addWithoutNotification(datenBlacklist);
        }
    }

    private void readMediaPath(XMLStreamReader parser) {
        DatenMediaPath mp = new DatenMediaPath();
        if (get(parser, DatenMediaPath.TAG, DatenMediaPath.XML_NAMES, mp.arr)) {
            daten.getListeMediaPath().add(mp);
        }
    }

    public boolean datenLesen(Path xmlFilePath) {
        boolean ret = false;
        if (Files.exists(xmlFilePath)) {
            DatenPset datenPset = null;

            XMLStreamReader parser = null;
            try (InputStream is = Files.newInputStream(xmlFilePath);
                 InputStreamReader in = new InputStreamReader(is, StandardCharsets.UTF_8)) {
                parser = inFactory.createXMLStreamReader(in);
                while (parser.hasNext()) {
                    final int event = parser.next();
                    if (event == XMLStreamConstants.START_ELEMENT) {
                        switch (parser.getLocalName()) {
                            case MVConfig.SYSTEM:
                                // System
                                readSystemConfiguration(parser);
                                break;

                            case DatenPset.TAG:
                                // Programmgruppen
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
                                // ende Programgruppen
                                break;

                            case ReplaceList.REPLACELIST:
                                readReplacementList(parser);
                                break;

                            case DatenAbo.TAG:
                                readAbos(parser);
                                break;

                            case DatenDownload.TAG:
                                readDownloads(parser);
                                break;

                            case DatenBlacklist.TAG:
                                readBlacklist(parser);
                                break;

                            case DatenMediaPath.TAG:
                                readMediaPath(parser);
                                break;
                        }
                    }
                }
                ret = true;
            } catch (Exception ex) {
                ret = false;
                logger.error("datenLesen", ex);
            } finally {
                if (parser != null) {
                    try {
                        parser.close();
                    } catch (XMLStreamException ignored) {
                    }
                }
            }

            sortLists();

            MVConfig.loadSystemParameter();
        }

        return ret;
    }

    private void sortLists() {
        daten.getListeDownloads().listeNummerieren();
        daten.getListeAbo().sort();
    }

}
