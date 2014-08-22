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
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
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
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream;

public class IoXmlSchreiben {
    private class FileDecompressor {
        private OutputStream decompressedOutputStream = null;
        private Path xmlFilePath = null;

        public FileDecompressor(Path xmlFilePath)
        {
            this.xmlFilePath = xmlFilePath;
        }

        public OutputStream decompress() throws IOException
        {
            OutputStream outputStream = Files.newOutputStream(xmlFilePath);
            if (xmlFilePath.endsWith(MSConst.FORMAT_BZ2)) {
                decompressedOutputStream = new BZip2CompressorOutputStream(outputStream, 2);
            } else if (xmlFilePath.endsWith(MSConst.FORMAT_ZIP)) {
                ZipOutputStream zipOutputStream = new ZipOutputStream(outputStream);
                ZipEntry entry = new ZipEntry(Konstanten.PROGRAMMNAME);
                zipOutputStream.putNextEntry(entry);
                decompressedOutputStream = zipOutputStream;
            } else {
                decompressedOutputStream = outputStream;
            }

            return decompressedOutputStream;
        }

        public void close() throws IOException
        {
            decompressedOutputStream.close();
        }
    }

    private XMLStreamWriter writer;
    private OutputStreamWriter out = null;
    private Path xmlFilePath = null;
    private FileDecompressor decompressor = null;

    public IoXmlSchreiben() {
        xmlFilePath = Daten.getMediathekXmlFilePath();
    }

    public synchronized void datenSchreiben(Daten daten) {
        xmlDatenSchreiben(daten);
    }

    public synchronized void exportPset(DatenPset[] pSet, String datei) {
        try {
            Log.systemMeldung("Pset exportieren");
            xmlFilePath = Paths.get(datei);
            xmlSchreibenStart();
            xmlSchreibenPset(pSet);
            xmlSchreibenEnde();
        } catch (Exception ex) {
            Log.fehlerMeldung(392846204,  "IoXmlSchreiben.exportPset", ex, "nach: " + datei);
        }
    }

    private void xmlDatenSchreiben(Daten daten) {
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
            xmlSchreibenProg(daten);

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

    private void xmlSchreibenStart() throws IOException, XMLStreamException {
        Log.systemMeldung("Start Schreiben nach: " + xmlFilePath.toAbsolutePath());
        decompressor = new FileDecompressor(xmlFilePath);
        out = new OutputStreamWriter(decompressor.decompress(), MSConst.KODIERUNG_UTF);

        XMLOutputFactory outFactory = XMLOutputFactory.newInstance();
        writer = outFactory.createXMLStreamWriter(out);
        writer.writeStartDocument(MSConst.KODIERUNG_UTF, "1.0");
        writer.writeCharacters("\n");//neue Zeile
        writer.writeStartElement(Konstanten.XML_START);
        writer.writeCharacters("\n");//neue Zeile
    }

    private void xmlSchreibenErsetzungstabelle() {
        for (String[] sa : Daten.mVReplaceList.liste) {
            xmlSchreibenDaten(MVReplaceList.REPLACELIST, MVReplaceList.COLUMN_NAMES, sa, false);
        }
    }

    private void xmlSchreibenProg(Daten daten) {
        //Proggruppen schreiben
        for (DatenPset datenPset : daten.listePset) {
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.COLUMN_NAMES_, datenPset.arr, false);
            for (DatenProg datenProg : datenPset.getListeProg()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.COLUMN_NAMES_, datenProg.arr, false);
            }
        }
    }

    /**
     * ProgrammSet in die Konfigurationsdatei schreiben.
     * @param psetArray Die PSet-Informationen.
     */
    private void xmlSchreibenPset(DatenPset[] psetArray) {
        for (DatenPset pset : psetArray) {
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.COLUMN_NAMES_, pset.arr, false);
            for (DatenProg datenProg : pset.getListeProg()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.COLUMN_NAMES_, datenProg.arr, false);
            }
        }
    }

    private void xmlSchreibenDownloads() {
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

    private void xmlSchreibenAbo() {
        //Abo schreiben
        for (DatenAbo datenAbo : Daten.listeAbo) {
            xmlSchreibenDaten(DatenAbo.ABO, DatenAbo.COLUMN_NAMES, datenAbo.arr, false);
        }
    }

    private void xmlSchreibenBlackList() {
        //Blacklist schreiben
        for (DatenBlacklist blacklist : Daten.listeBlacklist) {
            xmlSchreibenDaten(DatenBlacklist.BLACKLIST, DatenBlacklist.BLACKLIST_COLUMN_NAMES, blacklist.arr, false);
        }
    }

    private void xmlSchreibenFilmUpdateServer() throws XMLStreamException {
        //FilmUpdate schreiben
        writer.writeCharacters("\n");
        writer.writeComment("Akt-Filmliste");
        writer.writeCharacters("\n");
        for (DatenFilmlisteUrl datenUrlFilmliste : Daten.filmeLaden.getDownloadUrlsFilmlisten_akt()) {
            datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR] = DatenFilmlisteUrl.SERVER_ART_AKT;
            xmlSchreibenDaten(DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }
        writer.writeCharacters("\n");
        writer.writeComment("Old-Filmliste");
        writer.writeCharacters("\n");

        Iterator<DatenFilmlisteUrl> iterator = Daten.filmeLaden.getDownloadUrlsFilmlisten_old().iterator();
        while (iterator.hasNext()) {
            DatenFilmlisteUrl datenUrlFilmliste = iterator.next();
            datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR] = DatenFilmlisteUrl.SERVER_ART_OLD;
            xmlSchreibenDaten(DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }
        writer.writeCharacters("\n");
        writer.writeComment("Diff-Filmliste");
        writer.writeCharacters("\n");

        iterator = Daten.filmeLaden.getDownloadUrlsFilmlisten_diff().iterator();
        while (iterator.hasNext()) {
            DatenFilmlisteUrl datenUrlFilmliste = iterator.next();
            datenUrlFilmliste.arr[DatenFilmlisteUrl.FILM_UPDATE_SERVER_ART_NR] = DatenFilmlisteUrl.SERVER_ART_DIFF;
            xmlSchreibenDaten(DatenFilmlisteUrl.FILM_UPDATE_SERVER, DatenFilmlisteUrl.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }
    }

    private void xmlSchreibenDaten(String xmlName, String[] xmlSpalten, String[] datenArray, boolean newLine) {
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
            Log.fehlerMeldung(198325017,  "IoXmlSchreiben.xmlSchreibenDaten", ex);
        }
    }

    private void xmlSchreibenConfig(String xmlName, String[][] xmlSpalten, boolean newLine) {
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
            Log.fehlerMeldung(951230478,  "IoXmlSchreiben.xmlSchreibenConfig", ex);
        }
    }

    private void xmlSchreibenEnde() throws Exception {
        writer.writeEndElement();
        writer.writeEndDocument();
        writer.flush();

        decompressor.close();
        writer.close();
        out.close();

        Log.systemMeldung("geschrieben!");
    }
}
