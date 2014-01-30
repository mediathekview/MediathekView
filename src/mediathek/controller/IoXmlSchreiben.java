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
import java.util.ListIterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamWriter;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.MVConfig;
import msearch.filmeLaden.DatenFilmlistenServer;
import msearch.filmeLaden.DatenUrlFilmliste;
import msearch.filmeLaden.MSearchFilmlistenSuchen;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream;

public class IoXmlSchreiben {

    private XMLStreamWriter writer;
    private OutputStreamWriter out = null;
    ZipOutputStream zipOutputStream = null;
    BZip2CompressorOutputStream bZip2CompressorOutputStream = null;
    private Path xmlFilePath = null;

    public IoXmlSchreiben() {
        xmlFilePath = Daten.getMediathekXmlFilePath();
    }

    public synchronized void datenSchreiben(Daten daten) {
        xmlDatenSchreiben(daten);
        daten.history.speichern();
    }

    public synchronized void exportPset(DatenPset[] pSet, String datei) {
        try {
            Log.systemMeldung("Pset exportieren");
            xmlFilePath = Paths.get(datei);
            xmlSchreibenStart();
            xmlSchreibenPset(pSet);
            xmlSchreibenEnde();
        } catch (Exception ex) {
            Log.fehlerMeldung(392846204, Log.FEHLER_ART_PROG, "IoXmlSchreiben.exportPset", ex, "nach: " + datei);
        }
    }

    // ##############################
    // private
    // ##############################
    private void xmlDatenSchreiben(Daten daten) {
        try {
            Log.systemMeldung("Daten Schreiben");
            xmlSchreibenStart();
            //System schreibem
//            xmlSchreibenDaten(Konstanten.SYSTEM, Konstanten.SYSTEM_COLUMN_NAMES, Daten.system, true);
            xmlSchreibenConfig(MVConfig.SYSTEM, Daten.mVConfig.getAll(), true);
            //Senderliste
            xmlSchreibenProg(daten);
            xmlSchreibenDownloads(daten);
            xmlSchreibenAbo(daten);
            xmlSchreibenBlackList(daten);
            xmlSchreibenFilmUpdateServer();
            xmlSchreibenEnde();
        } catch (Exception ex) {
            Log.fehlerMeldung(656328109, Log.FEHLER_ART_PROG, "IoXml.xmlDatenSchreiben", ex);
        }
    }

    private void xmlSchreibenStart() throws IOException, XMLStreamException {
        Log.systemMeldung("Start Schreiben nach: " + xmlFilePath.toAbsolutePath());
        final OutputStream outputStream = Files.newOutputStream(xmlFilePath);
        if (xmlFilePath.endsWith(GuiKonstanten.FORMAT_BZ2)) {
            bZip2CompressorOutputStream = new BZip2CompressorOutputStream(outputStream, 2);
            out = new OutputStreamWriter(bZip2CompressorOutputStream, Konstanten.KODIERUNG_UTF);
        } else if (xmlFilePath.endsWith(GuiKonstanten.FORMAT_ZIP)) {
            zipOutputStream = new ZipOutputStream(outputStream);
            ZipEntry entry = new ZipEntry(Konstanten.PROGRAMMNAME);
            zipOutputStream.putNextEntry(entry);
            out = new OutputStreamWriter(zipOutputStream, Konstanten.KODIERUNG_UTF);
        } else {
            out = new OutputStreamWriter(outputStream, Konstanten.KODIERUNG_UTF);
        }

        XMLOutputFactory outFactory = XMLOutputFactory.newInstance();
        writer = outFactory.createXMLStreamWriter(out);
        writer.writeStartDocument(Konstanten.KODIERUNG_UTF, "1.0");
        writer.writeCharacters("\n");//neue Zeile
        writer.writeStartElement(Konstanten.XML_START);
        writer.writeCharacters("\n");//neue Zeile
    }

    private void xmlSchreibenProg(Daten daten) {
        ListIterator<DatenPset> iterator;
        //Proggruppen schreiben
        DatenPset datenPset;
        ListIterator<DatenProg> it;
        iterator = daten.listePset.listIterator();
        while (iterator.hasNext()) {
            datenPset = iterator.next();
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.COLUMN_NAMES_, datenPset.arr, false);
            it = datenPset.getListeProg().listIterator();
            while (it.hasNext()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.COLUMN_NAMES_, it.next().arr, false);
            }
        }
    }

    private void xmlSchreibenPset(DatenPset[] psetArray) {
        ListIterator<DatenProg> it;
        for (DatenPset pset : psetArray) {
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.COLUMN_NAMES_, pset.arr, false);
            it = pset.getListeProg().listIterator();
            while (it.hasNext()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.COLUMN_NAMES_, it.next().arr, false);
            }
        }
    }

    private void xmlSchreibenDownloads(Daten daten) {
        Iterator<DatenDownload> iterator;
        //Abo schreiben
        DatenDownload d;
        iterator = Daten.listeDownloads.iterator();
        while (iterator.hasNext()) {
            d = iterator.next();
            if (!d.istAbo()) {
                // Abos müssen neu angelegt werden
                if (d.start != null) {
                    if (d.start.status >= Start.STATUS_FERTIG) {
                        // keine fertigen Downloads
                        continue;
                    }
                }
                xmlSchreibenDaten(DatenDownload.DOWNLOAD, DatenDownload.COLUMN_NAMES_, d.arr, false);
            }
        }
    }

    private void xmlSchreibenAbo(Daten daten) {
        ListIterator<DatenAbo> iterator;
        //Abo schreibem
        DatenAbo datenAbo;
        iterator = Daten.listeAbo.listIterator();
        while (iterator.hasNext()) {
            datenAbo = iterator.next();
            xmlSchreibenDaten(DatenAbo.ABO, DatenAbo.COLUMN_NAMES, datenAbo.arr, false);
        }
    }

    private void xmlSchreibenBlackList(Daten daten) {
        Iterator<DatenBlacklist> it = Daten.listeBlacklist.iterator();
        //Blacklist schreibem
        DatenBlacklist blacklist;
        while (it.hasNext()) {
            blacklist = it.next();
            xmlSchreibenDaten(DatenBlacklist.BLACKLIST, DatenBlacklist.BLACKLIST_COLUMN_NAMES, blacklist.arr, false);
        }
    }

    private void xmlSchreibenFilmUpdateServer() {
        Iterator<DatenUrlFilmliste> iterator;
        //FilmUpdate schreibem
        DatenUrlFilmliste datenUrlFilmliste;
        iterator = Daten.filmeLaden.getDownloadUrlsFilmlisten(false).iterator();
        while (iterator.hasNext()) {
            datenUrlFilmliste = iterator.next();
            xmlSchreibenDaten(MSearchFilmlistenSuchen.FILM_UPDATE_SERVER, MSearchFilmlistenSuchen.FILM_UPDATE_SERVER_COLUMN_NAMES, datenUrlFilmliste.arr, false);
        }
        Iterator<DatenFilmlistenServer> it;
        it = Daten.filmeLaden.getListeFilmlistnServer().iterator();
        while (it.hasNext()) {
            DatenFilmlistenServer f = it.next();
            xmlSchreibenDaten(DatenFilmlistenServer.FILM_LISTEN_SERVER, DatenFilmlistenServer.FILM_LISTEN_SERVER_COLUMN_NAMES, f.arr, false);
        }
    }

    private void xmlSchreibenDaten(String xmlName, String[] xmlSpalten, String[] datenArray, boolean newLine) {
        int xmlMax = datenArray.length;
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
            Log.fehlerMeldung(198325017, Log.FEHLER_ART_PROG, "IoXmlSchreiben.xmlSchreibenDaten", ex);
        }
    }

    private void xmlSchreibenConfig(String xmlName, String[][] xmlSpalten, boolean newLine) {
        int xmlMax = xmlSpalten.length;
        try {
            writer.writeStartElement(xmlName);
            if (newLine) {
                writer.writeCharacters("\n"); //neue Zeile
            }
            for (int i = 0; i < xmlMax; ++i) {
                if (!xmlSpalten[i][1].equals("")) {
                    if (newLine) {
                        writer.writeCharacters("\t"); //Tab
                    }
                    writer.writeStartElement(xmlSpalten[i][0]);
                    writer.writeCharacters(xmlSpalten[i][1]);
                    writer.writeEndElement();
                    if (newLine) {
                        writer.writeCharacters("\n"); //neue Zeile
                    }
                }
            }
            writer.writeEndElement();
            writer.writeCharacters("\n"); //neue Zeile
        } catch (Exception ex) {
            Log.fehlerMeldung(951230478, Log.FEHLER_ART_PROG, "IoXmlSchreiben.xmlSchreibenConfig", ex);
        }
    }

    private void xmlSchreibenEnde() throws Exception {
        writer.writeEndElement();
        writer.writeEndDocument();
        writer.flush();

        if (xmlFilePath.endsWith(GuiKonstanten.FORMAT_BZ2)) {
            writer.close();
            out.close();
            bZip2CompressorOutputStream.close();
        } else if (xmlFilePath.endsWith(GuiKonstanten.FORMAT_ZIP)) {
            zipOutputStream.closeEntry();
            writer.close();
            out.close();
            zipOutputStream.close();
        } else {
            writer.close();
            out.close();
        }
        Log.systemMeldung("geschrieben!");
    }
}
