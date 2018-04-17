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

import mSearch.filmlisten.DatenFilmlisteUrl;
import mSearch.tool.Duration;
import mSearch.tool.Log;
import mSearch.tool.ReplaceList;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.*;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

public class IoXmlSchreiben {
    private final XMLOutputFactory outFactory;

    public IoXmlSchreiben() {
        outFactory = XMLOutputFactory.newInstance();
    }

    private void writeFileHeader(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeStartDocument(StandardCharsets.UTF_8.name(), "1.0");
        writeNewLine(writer);
        writer.writeStartElement(Konstanten.XML_START);
        writeNewLine(writer);
    }

    private void writeFileEnd(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeEndElement();
        writer.writeEndDocument();
        writer.flush();
        writer.close();
    }

    private void writeAbos(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment("Abos");
        writeNewLine(writer);

        for (DatenAbo datenAbo : Daten.getInstance().getListeAbo()) {
            xmlSchreibenDaten(writer, DatenAbo.TAG, DatenAbo.XML_NAMES, datenAbo.arr, false);
        }
    }

    private void writeBlacklist(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment("Blacklist");
        writeNewLine(writer);
        //Blacklist schreiben
        for (DatenBlacklist blacklist : Daten.getInstance().getListeBlacklist()) {
            xmlSchreibenDaten(writer, DatenBlacklist.TAG, DatenBlacklist.XML_NAMES, blacklist.arr, false);
        }
    }

    private void writeProgramSettings(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment(MVConfig.PARAMETER_INFO);
        writer.writeCharacters("\n\n");
        //writer.writeComment("Programmeinstellungen");
        writeNewLine(writer);
        xmlSchreibenConfig(writer, MVConfig.getAll());
        writeNewLine(writer);
    }

    private void writeProgramSets(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment("Programmsets");
        writeNewLine(writer);
        //Proggruppen schreiben, bei Konfig-Datei
        for (DatenPset datenPset : Daten.listePset) {
            xmlSchreibenDaten(writer, DatenPset.TAG, DatenPset.XML_NAMES, datenPset.arr, false);
            for (DatenProg datenProg : datenPset.getListeProg()) {
                xmlSchreibenDaten(writer, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.arr, false);
            }
        }
    }

    private void writeReplacementTable(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment("Ersetzungstabelle");
        writeNewLine(writer);

        for (String[] sa : ReplaceList.list) {
            xmlSchreibenDaten(writer, ReplaceList.REPLACELIST, ReplaceList.COLUMN_NAMES, sa, false);
        }
    }

    private void writeDownloads(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment("Downloads");
        writeNewLine(writer);
        //Abo schreiben
        for (DatenDownload download : Daten.getInstance().getListeDownloads()) {
            if (download.isInterrupted()) {
                // unterbrochene werden gespeichert, dass die Info "Interrupt" erhalten bleibt
                xmlSchreibenDaten(writer, DatenDownload.TAG, DatenDownload.XML_NAMES, download.arr, false);
            } else if (!download.istAbo() && !download.isFinished()) {
                //Download, (Abo müssen neu angelegt werden)
                xmlSchreibenDaten(writer, DatenDownload.TAG, DatenDownload.XML_NAMES, download.arr, false);
            }
        }
    }

    private void writeMediaDatabase(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment("Pfade MedienDB");
        writeNewLine(writer);
        //Pfade der MedienDB schreiben
        for (DatenMediaPath mp : Daten.getInstance().getListeMediaPath()) {
            xmlSchreibenDaten(writer, DatenMediaPath.TAG, DatenMediaPath.XML_NAMES, mp.arr, false);
        }
    }

    private void writeUpdateServerData(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        writeNewLine(writer);
        //FilmUpdate schreiben
        writeNewLine(writer);
        for (DatenFilmlisteUrl datenUrlFilmliste : Daten.getInstance().getFilmeLaden().getDownloadUrlsFilmlisten_akt()) {
            datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR] = DatenFilmlisteUrl.SERVER_ART_AKT;
            xmlSchreibenDaten(writer, DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }

        writeNewLine(writer);
        for (DatenFilmlisteUrl datenUrlFilmliste : Daten.getInstance().getFilmeLaden().getDownloadUrlsFilmlisten_diff()) {
            datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR] = DatenFilmlisteUrl.SERVER_ART_DIFF;
            xmlSchreibenDaten(writer, DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }
    }

    private void xmlSchreibenPset(XMLStreamWriter writer, DatenPset[] psetArray) throws XMLStreamException {
        // wird beim Export Sets verwendet
        writer.writeCharacters("\n\n");
        for (DatenPset pset : psetArray) {
            xmlSchreibenDaten(writer, DatenPset.TAG, DatenPset.XML_NAMES, pset.arr, true);
            for (DatenProg datenProg : pset.getListeProg()) {
                xmlSchreibenDaten(writer, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.arr, true);
            }
            writer.writeCharacters("\n\n");
        }
    }

    private void xmlSchreibenDaten(XMLStreamWriter writer, String xmlName, String[] xmlSpalten, String[] datenArray, boolean newLine) {
        final int xmlMax = datenArray.length;
        try {
            writer.writeStartElement(xmlName);
            if (newLine) {
                writeNewLine(writer);
            }
            for (int i = 0; i < xmlMax; ++i) {
                if (!datenArray[i].isEmpty()) {
                    if (newLine) {
                        writer.writeCharacters("\t"); //Tab
                    }
                    writer.writeStartElement(xmlSpalten[i]);
                    writer.writeCharacters(datenArray[i]);
                    writer.writeEndElement();
                    if (newLine) {
                        writeNewLine(writer);
                    }
                }
            }
            writer.writeEndElement();
            writeNewLine(writer);
        } catch (Exception ex) {
            Log.errorLog(198325017, ex);
        }
    }

    private void writeNewLine(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n"); //neue Zeile
    }

    private void xmlSchreibenConfig(XMLStreamWriter writer, String[][] xmlSpalten) {
        try {
            writer.writeStartElement(MVConfig.SYSTEM);
            writeNewLine(writer);
            for (String[] xmlSpalte : xmlSpalten) {
                if (!MVConfig.Configs.find(xmlSpalte[0])) {
                    continue; //nur Configs schreiben die es noch gibt
                }
                writer.writeCharacters("\t"); //Tab
                writer.writeStartElement(xmlSpalte[0]);
                writer.writeCharacters(xmlSpalte[1]);
                writer.writeEndElement();
                writeNewLine(writer);
            }
            writer.writeEndElement();
            writeNewLine(writer);
        } catch (Exception ex) {
            Log.errorLog(951230478, ex);
        }
    }

    public synchronized void writeConfigurationFile(Path xmlFilePath) {
        SysMsg.sysMsg("Daten Schreiben nach: " + xmlFilePath.toString());
        xmlDatenSchreiben(xmlFilePath);
    }

    public synchronized void exportPset(DatenPset[] pSet, String datei) {
        final Path xmlFilePath = Paths.get(datei);
        try (OutputStream os = Files.newOutputStream(xmlFilePath);
             OutputStreamWriter out = new OutputStreamWriter(os, StandardCharsets.UTF_8)
        ) {
            XMLStreamWriter writer = outFactory.createXMLStreamWriter(out);
            SysMsg.sysMsg("Pset exportieren nach: " + xmlFilePath.toString());
            SysMsg.sysMsg("Start Schreiben nach: " + xmlFilePath.toAbsolutePath());

            writeFileHeader(writer);

            xmlSchreibenPset(writer, pSet);

            writeFileEnd(writer);
            SysMsg.sysMsg("geschrieben!");
        } catch (Exception ex) {
            Log.errorLog(392846204, ex, "nach: " + datei);
        }
    }

    private void xmlDatenSchreiben(Path xmlFilePath) {
        SysMsg.sysMsg("Start Schreiben nach: " + xmlFilePath.toAbsolutePath());
        Duration.counterStart("Config schreiben");

        try (OutputStream os = Files.newOutputStream(xmlFilePath);
             OutputStreamWriter out = new OutputStreamWriter(os, StandardCharsets.UTF_8)) {
            XMLStreamWriter writer = outFactory.createXMLStreamWriter(out);

            writeFileHeader(writer);

            writeAbos(writer);

            writeBlacklist(writer);

            writeProgramSettings(writer);

            writeProgramSets(writer);

            writeReplacementTable(writer);

            writeDownloads(writer);

            writeMediaDatabase(writer);

            writeUpdateServerData(writer);

            writer.writeCharacters("\n\n");

            writeFileEnd(writer);

            SysMsg.sysMsg("geschrieben!");
        } catch (Exception ex) {
            Log.errorLog(656328109, ex);
        }
        Duration.counterStop("Config schreiben");
    }
}
