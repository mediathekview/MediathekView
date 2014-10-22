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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.tool.Konstanten;
import mediathek.tool.MVConfig;
import mediathek.tool.MVReplaceList;
import msearch.filmeLaden.DatenFilmlisteUrl;
import msearch.tool.MSConst;

public class IoXmlSchreiben {

    private static XMLStreamWriter writer;
    private static OutputStreamWriter out = null;
    private static Path xmlFilePath = Daten.getMediathekXmlFilePath();

    public static synchronized void datenSchreiben() {
        xmlDatenSchreiben();
    }

    public static synchronized void exportPset(DatenPset[] pSet, String datei) {
        try {
            Log.systemMeldung("Pset exportieren");
            xmlFilePath = Paths.get(datei);
            xmlSchreibenStart();
            xmlSchreibenPset(pSet);
            xmlSchreibenEnde();
        } catch (Exception ex) {
            Log.fehlerMeldung(392846204, "IoXmlSchreiben.exportPset", ex, "nach: " + datei);
        }
    }

    private static void xmlDatenSchreiben() {
        try {
            Log.systemMeldung("Daten Schreiben");
            xmlSchreibenStart();

            writer.writeCharacters("\n\n");
            writer.writeComment("Abos");
            writer.writeCharacters("\n");
            xmlSchreibenAbo();

            writer.writeCharacters("\n\n");
            writer.writeComment("Blacklist");
            writer.writeCharacters("\n");
            xmlSchreibenBlackList();

            writer.writeCharacters("\n\n");
            writer.writeComment("Programmeinstellungen");
            writer.writeCharacters("\n");
            xmlSchreibenConfig(MVConfig.SYSTEM, Daten.mVConfig.getAll(), true);
            writer.writeCharacters("\n");

            writer.writeCharacters("\n\n");
            writer.writeComment("Programmsets");
            writer.writeCharacters("\n");
            xmlSchreibenProg();

            writer.writeCharacters("\n\n");
            writer.writeComment("Ersetzungstabelle");
            writer.writeCharacters("\n");
            xmlSchreibenErsetzungstabelle();

            writer.writeCharacters("\n\n");
            writer.writeComment("Downloads");
            writer.writeCharacters("\n");
            xmlSchreibenDownloads();

            writer.writeCharacters("\n\n");
            writer.writeComment("Update Filmliste");
            writer.writeCharacters("\n");
            xmlSchreibenFilmUpdateServer();

            writer.writeCharacters("\n\n");
            xmlSchreibenEnde();
        } catch (Exception ex) {
            Log.fehlerMeldung(656328109, "IoXml.xmlDatenSchreiben", ex);
        }
    }

    private static void xmlSchreibenStart() throws IOException, XMLStreamException {
        Log.systemMeldung("Start Schreiben nach: " + xmlFilePath.toAbsolutePath());
        out = new OutputStreamWriter(Files.newOutputStream(xmlFilePath), MSConst.KODIERUNG_UTF);

        XMLOutputFactory outFactory = XMLOutputFactory.newInstance();
        writer = outFactory.createXMLStreamWriter(out);
        writer.writeStartDocument(MSConst.KODIERUNG_UTF, "1.0");
        writer.writeCharacters("\n");//neue Zeile
        writer.writeStartElement(Konstanten.XML_START);
        writer.writeCharacters("\n");//neue Zeile
    }

    private static void xmlSchreibenErsetzungstabelle() {
        for (String[] sa : Daten.mVReplaceList.liste) {
            xmlSchreibenDaten(MVReplaceList.REPLACELIST, MVReplaceList.COLUMN_NAMES, sa, false);
        }
    }

    private static void xmlSchreibenProg() {
        //Proggruppen schreiben
        for (DatenPset datenPset : Daten.listePset) {
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.COLUMN_NAMES_, datenPset.arr, false);
            for (DatenProg datenProg : datenPset.getListeProg()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.COLUMN_NAMES_, datenProg.arr, false);
            }
        }
    }

    private static void xmlSchreibenPset(DatenPset[] psetArray) {
        for (DatenPset pset : psetArray) {
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.COLUMN_NAMES_, pset.arr, false);
            for (DatenProg datenProg : pset.getListeProg()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.COLUMN_NAMES_, datenProg.arr, false);
            }
        }
    }

    private static void xmlSchreibenDownloads() {
        //Abo schreiben
        for (DatenDownload download : Daten.listeDownloads) {
            if (download.isInterrupted()) {
                // unterbrochene werden gespeichert, dass die Info "Interrupt" erhalten bleibt
                xmlSchreibenDaten(DatenDownload.DOWNLOAD, DatenDownload.COLUMN_NAMES_, download.arr, false);
            } else if (!download.istAbo() && !download.isFinished()) {
                //Download, (Abo müssen neu angelegt werden)
                xmlSchreibenDaten(DatenDownload.DOWNLOAD, DatenDownload.COLUMN_NAMES_, download.arr, false);
            }
        }
    }

    private static void xmlSchreibenAbo() {
        //Abo schreiben
        for (DatenAbo datenAbo : Daten.listeAbo) {
            xmlSchreibenDaten(DatenAbo.ABO, DatenAbo.COLUMN_NAMES, datenAbo.arr, false);
        }
    }

    private static void xmlSchreibenBlackList() {
        //Blacklist schreiben
        for (DatenBlacklist blacklist : Daten.listeBlacklist) {
            xmlSchreibenDaten(DatenBlacklist.BLACKLIST, DatenBlacklist.BLACKLIST_COLUMN_NAMES, blacklist.arr, false);
        }
    }

    private static void xmlSchreibenFilmUpdateServer() throws XMLStreamException {
        //FilmUpdate schreiben
        writer.writeCharacters("\n");
        writer.writeComment("Akt-Filmliste");
        writer.writeCharacters("\n");
        for (DatenFilmlisteUrl datenUrlFilmliste : Daten.filmeLaden.getDownloadUrlsFilmlisten_akt()) {
            datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR] = DatenFilmlisteUrl.SERVER_ART_AKT;
            xmlSchreibenDaten(DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }

        writer.writeCharacters("\n");
        writer.writeComment("Diff-Filmliste");
        writer.writeCharacters("\n");
        for (DatenFilmlisteUrl datenUrlFilmliste : Daten.filmeLaden.getDownloadUrlsFilmlisten_diff()) {
            datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR] = DatenFilmlisteUrl.SERVER_ART_DIFF;
            xmlSchreibenDaten(DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }
    }

    private static void xmlSchreibenDaten(String xmlName, String[] xmlSpalten, String[] datenArray, boolean newLine) {
        final int xmlMax = datenArray.length;
        try {
            writer.writeStartElement(xmlName);
            if (newLine) {
                writer.writeCharacters("\n"); //neue Zeile
            }
            for (int i = 0; i < xmlMax; ++i) {
                if (!datenArray[i].equals("")) {
                    if (newLine) {
                        writer.writeCharacters("\t"); //Tab
                    }
                    writer.writeStartElement(xmlSpalten[i]);
                    writer.writeCharacters(datenArray[i]);
                    writer.writeEndElement();
                    if (newLine) {
                        writer.writeCharacters("\n"); //neue Zeile
                    }
                }
            }
            writer.writeEndElement();
            writer.writeCharacters("\n"); //neue Zeile
        } catch (Exception ex) {
            Log.fehlerMeldung(198325017, "IoXmlSchreiben.xmlSchreibenDaten", ex);
        }
    }

    private static void xmlSchreibenConfig(String xmlName, String[][] xmlSpalten, boolean newLine) {
        try {
            writer.writeStartElement(xmlName);
            if (newLine) {
                writer.writeCharacters("\n"); //neue Zeile
            }
            for (String[] xmlSpalte : xmlSpalten) {
                if (!xmlSpalte[1].equals("")) {
                    if (newLine) {
                        writer.writeCharacters("\t"); //Tab
                    }
                    writer.writeStartElement(xmlSpalte[0]);
                    writer.writeCharacters(xmlSpalte[1]);
                    writer.writeEndElement();
                    if (newLine) {
                        writer.writeCharacters("\n"); //neue Zeile
                    }
                }
            }
            writer.writeEndElement();
            writer.writeCharacters("\n"); //neue Zeile
        } catch (Exception ex) {
            Log.fehlerMeldung(951230478, "IoXmlSchreiben.xmlSchreibenConfig", ex);
        }
    }

    private static void xmlSchreibenEnde() throws Exception {
        writer.writeEndElement();
        writer.writeEndDocument();
        writer.flush();

        writer.close();
        out.close();

        Log.systemMeldung("geschrieben!");
    }
}
