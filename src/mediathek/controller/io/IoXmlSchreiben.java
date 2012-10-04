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

import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.Iterator;
import java.util.ListIterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import mediathek.controller.filmeLaden.importieren.DatenFilmUpdateServer;
import mediathek.controller.filmeLaden.importieren.FilmUpdateServer;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenBlacklist;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenProg;
import mediathek.daten.DatenPset;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream;

public class IoXmlSchreiben {

    private XMLOutputFactory outFactory;
    private XMLStreamWriter writer;
    private OutputStreamWriter out = null;
    ZipOutputStream zipOutputStream = null;
    BZip2CompressorOutputStream bZip2CompressorOutputStream = null;

    public IoXmlSchreiben() {
    }

    public synchronized void datenSchreiben(DDaten daten) {
        xmlDatenSchreiben(daten);
        daten.history.speichern();
    }

    public synchronized void exportPset(DatenPset[] pSet, String datei) {
        try {
            Log.systemMeldung("Pset exportieren");
            xmlSchreibenStart(datei);
            xmlSchreibenPset(pSet);
            xmlSchreibenEnde(datei);
        } catch (Exception ex) {
            Log.fehlerMeldung(392846204,"IoXmlSchreiben.exportPset", ex, "nach: " + datei);
        }
    }

    // ##############################
    // private
    // ##############################
    private void xmlDatenSchreiben(DDaten daten) {
        try {
            Log.systemMeldung("Daten Schreiben");
            xmlSchreibenStart(Daten.getBasisVerzeichnis(true) + Konstanten.XML_DATEI);
            //System schreibem
            xmlSchreibenDaten(Konstanten.SYSTEM, Konstanten.SYSTEM_COLUMN_NAMES, DDaten.system);
            //Senderliste
            xmlSchreibenProg(daten);
            xmlSchreibenDownloads(daten);
            xmlSchreibenAbo(daten);
            xmlSchreibenBlackList(daten);
            xmlSchreibenFilmUpdateServer(daten);
            xmlSchreibenEnde();
        } catch (Exception ex) {
            Log.fehlerMeldung(656328109,"IoXml.xmlDatenSchreiben", ex);
        }
    }

    private void xmlSchreibenStart(String datei) throws Exception {
        File file = new File(datei);
        Log.systemMeldung("Start Schreiben nach: " + datei);
        outFactory = XMLOutputFactory.newInstance();
        if (datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
            bZip2CompressorOutputStream = new BZip2CompressorOutputStream(new FileOutputStream(file), 2);
            out = new OutputStreamWriter(bZip2CompressorOutputStream, Konstanten.KODIERUNG_UTF);
        } else if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
            zipOutputStream = new ZipOutputStream(new FileOutputStream(file));
            ZipEntry entry = new ZipEntry(Konstanten.XML_DATEI_FILME);
            zipOutputStream.putNextEntry(entry);
            out = new OutputStreamWriter(zipOutputStream, Konstanten.KODIERUNG_UTF);
        } else {
            out = new OutputStreamWriter(new FileOutputStream(file), Konstanten.KODIERUNG_UTF);
        }
        writer = outFactory.createXMLStreamWriter(out);
        writer.writeStartDocument("UTF-8", "1.0");
        writer.writeCharacters("\n");//neue Zeile
        writer.writeStartElement(Konstanten.XML_START);
        writer.writeCharacters("\n");//neue Zeile
    }

    private void xmlSchreibenProg(DDaten daten) {
        ListIterator<DatenPset> iterator;
        //Proggruppen schreiben
        DatenPset datenPset;
        ListIterator<DatenProg> it;
        iterator = daten.listePset.listIterator();
        while (iterator.hasNext()) {
            datenPset = iterator.next();
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.PROGRAMMSET_COLUMN_NAMES, datenPset.arr);
            it = datenPset.getListeProg().listIterator();
            while (it.hasNext()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.PROGRAMM_COLUMN_NAMES, it.next().arr);
            }
        }
    }

    private void xmlSchreibenPset(DatenPset[] datenPset) {
        ListIterator<DatenProg> it;
        for (int i = 0; i < datenPset.length; ++i) {
            xmlSchreibenDaten(DatenPset.PROGRAMMSET, DatenPset.PROGRAMMSET_COLUMN_NAMES, datenPset[i].arr);
            it = datenPset[i].getListeProg().listIterator();
            while (it.hasNext()) {
                xmlSchreibenDaten(DatenProg.PROGRAMM, DatenProg.PROGRAMM_COLUMN_NAMES, it.next().arr);
            }
        }
    }

    private void xmlSchreibenDownloads(DDaten daten) {
        Iterator<DatenDownload> iterator;
        //Abo schreibem
        DatenDownload d;
        iterator = daten.listeDownloads.iterator();
        while (iterator.hasNext()) {
            d = iterator.next();
            if (!d.istAbo()) {
                // Abos müssen neu angelegt werden
                xmlSchreibenDaten(DatenDownload.DOWNLOAD, DatenDownload.DOWNLOAD_COLUMN_NAMES, d.arr);
            }
        }
    }

    private void xmlSchreibenAbo(DDaten daten) {
        ListIterator<DatenAbo> iterator;
        //Abo schreibem
        DatenAbo datenAbo;
        iterator = daten.listeAbo.listIterator();
        while (iterator.hasNext()) {
            datenAbo = iterator.next();
            xmlSchreibenDaten(DatenAbo.ABO, DatenAbo.ABO_COLUMN_NAMES, datenAbo.arr);
        }
    }

    private void xmlSchreibenBlackList(DDaten daten) {
        Iterator<DatenBlacklist> it = daten.listeBlacklist.iterator();
        //Blacklist schreibem
        DatenBlacklist blacklist;
        while (it.hasNext()) {
            blacklist = it.next();
            xmlSchreibenDaten(DatenBlacklist.BLACKLIST, DatenBlacklist.BLACKLIST_COLUMN_NAMES, blacklist.arr);
        }
    }

    private void xmlSchreibenFilmUpdateServer(DDaten daten) {
        Iterator<DatenFilmUpdateServer> iterator;
        //FilmUpdate schreibem
        DatenFilmUpdateServer datenFilmUpdate;
        iterator = DDaten.filmeLaden.getListeFilmUpdateServer(false).iterator();
        while (iterator.hasNext()) {
            datenFilmUpdate = iterator.next();
            xmlSchreibenDaten(FilmUpdateServer.FILM_UPDATE_SERVER, FilmUpdateServer.FILM_UPDATE_SERVER_COLUMN_NAMES, datenFilmUpdate.arr);
        }
    }

    private void xmlSchreibenDaten(String xmlName, String[] xmlSpalten, String[] datenArray) {
        int xmlMax = datenArray.length;
        try {
            writer.writeStartElement(xmlName);
            for (int i = 0; i < xmlMax; ++i) {
                if (!datenArray[i].equals("")) {
                    writer.writeStartElement(xmlSpalten[i]);
                    writer.writeCharacters(datenArray[i]);
                    writer.writeEndElement();
                }
            }
            writer.writeEndElement();
            writer.writeCharacters("\n");//neue Zeile
        } catch (Exception ex) {
            Log.fehlerMeldung(198325017,"IoXmlSchreiben.xmlSchreibenDaten", ex);
        }
    }

    private void xmlSchreibenEnde() throws Exception {
        xmlSchreibenEnde("");
    }

    private void xmlSchreibenEnde(String datei) throws Exception {
        writer.writeEndElement();
        writer.writeEndDocument();
        writer.flush();
        if (datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
            writer.close();
            bZip2CompressorOutputStream.close();
        } else if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
            zipOutputStream.closeEntry();
            writer.close();
            zipOutputStream.close();
        } else {
            writer.close();
        }
        Log.systemMeldung("geschrieben!");
    }
}
