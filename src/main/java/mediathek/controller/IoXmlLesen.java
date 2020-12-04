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
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.tool.ReplaceList;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
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

    private void readAboEntry(XMLStreamReader parser) {
        try {
            DatenAbo datenAbo = new DatenAbo();
            datenAbo.readFromConfig(parser);
            daten.getListeAbo().addAbo(datenAbo);
        } catch (XMLStreamException e) {
            logger.error("Failed to read abo entry", e);
        }
    }

    private void readDownloadEntry(XMLStreamReader parser) {
        try {
            var dl = DatenDownload.getFromConfig(parser);
            // abo entries will be generated...but we need this for CLI so far
            if (!dl.isFromAbo())
                daten.getListeDownloads().add(dl);
        } catch (Exception e) {
            logger.error("readDownloadEntry", e);
        }
    }

    private void readBlacklist(XMLStreamReader parser) {
        // Blacklist
        BlacklistRule blacklistRule = new BlacklistRule();
        if (get(parser, BlacklistRule.TAG, BlacklistRule.XML_NAMES, blacklistRule.arr)) {
            daten.getListeBlacklist().addWithoutNotification(blacklistRule);
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
                            case MVConfig.SYSTEM -> readSystemConfiguration(parser);
                            case DatenPset.TAG -> {
                                datenPset = new DatenPset();
                                if (get(parser, DatenPset.TAG, DatenPset.XML_NAMES, datenPset.arr)) {
                                    Daten.listePset.add(datenPset);
                                }
                            }
                            case DatenProg.TAG -> {
                                DatenProg datenProg = new DatenProg();
                                if (get(parser, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.arr)) {
                                    if (datenPset != null) {
                                        datenPset.addProg(datenProg);
                                    }
                                }
                            }
                            case ReplaceList.REPLACELIST -> readReplacementList(parser);
                            case DatenAbo.TAG -> readAboEntry(parser);
                            case DatenDownload.TAG -> readDownloadEntry(parser);
                            case BlacklistRule.TAG -> readBlacklist(parser);
                            case DatenMediaPath.TAG -> readMediaPath(parser);
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
