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

import mediathek.Log;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.ListIterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;
import javax.xml.stream.XMLOutputFactory;
import javax.xml.stream.XMLStreamWriter;
import mediathek.Konstanten;
import mediathek.tool.GuiKonstanten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorOutputStream;

public class IoXmlFilmlisteSchreiben {

    private XMLOutputFactory outFactory;
    private XMLStreamWriter writer;
    private OutputStreamWriter out = null;
    ZipOutputStream zipOutputStream = null;
    BZip2CompressorOutputStream bZip2CompressorOutputStream = null;

    public IoXmlFilmlisteSchreiben() {
    }

    public void filmeSchreiben(String datei, ListeFilme listeFilme) {
        try {
            Log.systemMeldung("Filme Schreiben");
            xmlSchreibenStart(datei);
            xmlSchreibenFilmliste(listeFilme);
            xmlSchreibenEnde(datei);
        } catch (Exception ex) {
            Log.fehlerMeldung("IoXmlSchreiben.FilmeSchreiben", ex, "nach: " + datei);
        }
    }

    // ##############################
    // private
    // ##############################
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

    private void xmlSchreibenFilmliste(ListeFilme listeFilme) {
        //Filmliste Metadaten schreiben
        listeFilme.metaDaten[ListeFilme.FILMLISTE_VERSION_NR] = Konstanten.VERSION;
        xmlSchreibenDaten(ListeFilme.FILMLISTE, ListeFilme.FILMLISTE_COLUMN_NAMES, listeFilme.metaDaten);
        xmlSchreibenDaten(ListeFilme.FILMLISTE_INFOS, ListeFilme.FILMLISTE_INFOS_COLUMN_NAMES, listeFilme.infos);
        xmlSchreibenFeldInfo();
        //Filme schreiben
        ListIterator<DatenFilm> iterator;
        DatenFilm datenFilm;
        iterator = listeFilme.listIterator();
        while (iterator.hasNext()) {
            datenFilm = iterator.next();
            xmlSchreibenDaten(DatenFilm.FILME_, DatenFilm.FILME_COLUMN_NAMES_, datenFilm.getClean().arr);
        }
    }

    private void xmlSchreibenFeldInfo() {
        int xmlMax = DatenFilm.FILME_COLUMN_NAMES.length;
        try {
            writer.writeStartElement(DatenFilm.FELD_INFO);
            writer.writeCharacters("\n");//neue Zeile
            for (int i = 0; i < xmlMax; ++i) {
                writer.writeStartElement(DatenFilm.FILME_COLUMN_NAMES_[i]);
                writer.writeCharacters(DatenFilm.FILME_COLUMN_NAMES[i]);
                writer.writeEndElement();
                writer.writeCharacters("\n");//neue Zeile
            }
            writer.writeEndElement();
            writer.writeCharacters("\n");//neue Zeile
        } catch (Exception ex) {
            Log.fehlerMeldung("IoXmlSchreiben.xmlSchreibenFeldInfo", ex);
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
            Log.fehlerMeldung("IoXmlSchreiben.xmlSchreibenDaten", ex);
        }
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
