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

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.daten.*;
import mediathek.daten.blacklist.BlacklistRule;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.ReplaceList;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.concurrent.RejectedExecutionException;

public class IoXmlSchreiben {
    private static final Logger logger = LogManager.getLogger(IoXmlSchreiben.class);
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
        writeNewLine(writer);

        for (DatenAbo datenAbo : Daten.getInstance().getListeAbo()) {
            datenAbo.writeToConfig(writer);
        }
    }

    private void writeBlacklist(XMLStreamWriter writer) throws XMLStreamException {
        writer.writeCharacters("\n\n");
        //writer.writeComment("Blacklist");
        writeNewLine(writer);
        //Blacklist schreiben
        for (BlacklistRule blacklist : Daten.getInstance().getListeBlacklist()) {
            xmlSchreibenDaten(writer, BlacklistRule.TAG, BlacklistRule.XML_NAMES, blacklist.arr, false);
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
            xmlSchreibenDaten(writer, DatenPset.TAG, DatenPset.XML_NAMES, datenPset.arr, true);
            for (DatenProg datenProg : datenPset.getListeProg()) {
                xmlSchreibenDaten(writer, DatenProg.TAG, DatenProg.XML_NAMES, datenProg.arr, true);
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

    /**
     * Write all abo entries into XML config file.
     * @param writer the writer for the config file
     * @throws XMLStreamException caller must handle errors.
     */
    private void writeDownloads(XMLStreamWriter writer) throws XMLStreamException {
        /*
            CLI client must rely on specific format as this is some strange dialect.
            Here we set what version we save.
         */
        final int dl_list_version = 1;
        try {
            ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.CLI_CLIENT_DOWNLOAD_LIST_FORMAT, dl_list_version);
        }
        catch (RejectedExecutionException ignore) {
            //this may occur during shutdown
        }
        catch (Exception e) {
            logger.error("writeDownloads error!", e);
        }

        writer.writeCharacters("\n\n");
        writeNewLine(writer);

        for (DatenDownload download : Daten.getInstance().getListeDownloads()) {
            if (download.isInterrupted()) {
                // unterbrochene werden gespeichert, dass die Info "Interrupt" erhalten bleibt
                download.writeConfigEntry(writer);
            } else if (!download.isFinished() && !download.isFromAbo()) {
                //Download, (Abo müssen neu angelegt werden)
                download.writeConfigEntry(writer);
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
            logger.error("xmlSchreibenDaten", ex);
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
            logger.error(ex);
        }
    }

    public synchronized void writeConfigurationFile(Path xmlFilePath) {
        logger.info("Daten Schreiben nach: {}", xmlFilePath.toString());
        xmlDatenSchreiben(xmlFilePath);
    }

    public synchronized void exportPset(DatenPset[] pSet, String datei) {
        final Path xmlFilePath = Paths.get(datei);
        try (OutputStream os = Files.newOutputStream(xmlFilePath);
             OutputStreamWriter out = new OutputStreamWriter(os, StandardCharsets.UTF_8)
        ) {
            XMLStreamWriter writer = outFactory.createXMLStreamWriter(out);
            logger.info("Pset exportieren nach: {}", xmlFilePath.toString());
            logger.info("Start Schreiben nach: {}", xmlFilePath.toAbsolutePath());

            writeFileHeader(writer);

            xmlSchreibenPset(writer, pSet);

            writeFileEnd(writer);
            logger.info("geschrieben!");
        } catch (Exception ex) {
            logger.error("nach {}", datei, ex);
        }
    }

    private void xmlDatenSchreiben(Path xmlFilePath) {
        logger.info("Config Schreiben nach: {} startet", xmlFilePath.toAbsolutePath());

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

            writer.writeCharacters("\n\n");

            writeFileEnd(writer);

            logger.info("Config Schreiben beendet");
        } catch (Exception ex) {
            logger.error("xmlDatenSchreiben", ex);
        }
    }
}
