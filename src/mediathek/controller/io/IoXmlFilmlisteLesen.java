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

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.net.URL;
import java.net.URLConnection;
import java.util.zip.ZipInputStream;
import javax.swing.event.EventListenerList;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import mediathek.controller.filmeLaden.ListenerFilmeLaden;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.daten.Daten;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.tool.DatumZeit;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;

public class IoXmlFilmlisteLesen {

    private EventListenerList listeners = new EventListenerList();
    private int max = 0;
    private int progress = 0;
    private int event;
    private int ii, i;

    public void addAdListener(ListenerFilmeLaden listener) {
        listeners.add(ListenerFilmeLaden.class, listener);
    }

    /**
     *
     * @param datei
     * @param istUrl
     * @return
     */
    public boolean filmlisteLesen(String datei, boolean istUrl) {
        // Filmliste an bel. Stelle lesen und als eigenen Liste speichern
        // die Datei erst mal an Ort und Stelle kopieren und evtl. entpacken
        if (istUrl && datei.endsWith(GuiKonstanten.FORMAT_BZ2) || istUrl && datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
            // da wird eine temp-Datei benutzt
            this.notifyStart(300);
            this.notifyProgress(datei);
        } else {
            this.notifyStart(200);
            this.notifyProgress(datei);
        }
        if (!datei.equals(Daten.getDateiFilmliste())) {
            // macht nur Sinn, wenn nicht die eigene Liste gelesen werden soll!
            if (!filmlisteInStandardDateiSchreiben(datei, istUrl)) {
                // dann wars das
                this.notifyFertig(Daten.listeFilme);
                return false;
            }
        }
        // die Filmdatei ist jetzt entpackt und im Konfigordner gespeichert, dort jetzt lesen
        boolean ret = standardFilmlisteLesen();
        this.notifyFertig(Daten.listeFilme);
        return ret;
    }

    public boolean standardFilmlisteLesen() {
        // beim Programmstart, da wird die Filmlise Daten.listeFilme nicht vorher gelöscht
        boolean ret = true;
        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        XMLStreamReader parser;
        InputStreamReader inReader = null;
        // die Filmdatei ist jetzt entpackt und im Konfigordner gespeichert, dort jetzt lesen
        try {
            inReader = new InputStreamReader(new FileInputStream(Daten.getDateiFilmliste()), Konstanten.KODIERUNG_UTF);
            parser = inFactory.createXMLStreamReader(inReader);
            ret = filmlisteXmlLesen(parser, Daten.getDateiFilmliste(), Daten.listeFilme);
        } catch (Exception ex) {
            ret = false;
            Log.fehlerMeldung(901739831, Log.FEHLER_ART_PROG, "IoXmlLesen.standardFilmlisteLesen", ex);
        } finally {
            try {
                if (inReader != null) {
                    inReader.close();
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(301029761, Log.FEHLER_ART_PROG, "IoXmlLesen.standardFilmlisteLesen", ex);
            }
        }
        return ret;
    }

    private boolean filmlisteInStandardDateiSchreiben(String datei, boolean istUrl) {
        if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_FILMLISTE_UMBENENNEN_NR])) {
            // wenn gewünscht, erst mal die alte Filmliste umbenenen
            filmlisteUmbenennen();
        }
        boolean ret = true;
        InputStreamReader inReader = null;
        OutputStreamWriter fOutWriter = null;
        BZip2CompressorInputStream bZip2CompressorInputStream;
        int timeout = 10000; //10 Sekunden
        URLConnection conn;
        File tmpFile = null;
        try {
            if (!istUrl) {
                // lokale Datei
                if (!new File(datei).exists()) {
                    return false;
                }
                if (datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
                    bZip2CompressorInputStream = new BZip2CompressorInputStream(new FileInputStream(datei));
                    inReader = new InputStreamReader(bZip2CompressorInputStream, Konstanten.KODIERUNG_UTF);
                } else if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
                    ZipInputStream zipInputStream = new ZipInputStream(new FileInputStream(datei));
                    zipInputStream.getNextEntry();
                    inReader = new InputStreamReader(zipInputStream, Konstanten.KODIERUNG_UTF);
                } else {
                    inReader = new InputStreamReader(new FileInputStream(datei), Konstanten.KODIERUNG_UTF);
                }
            } else {
                // aus dem Netzt
                conn = new URL(datei).openConnection();
                conn.setConnectTimeout(timeout);
                conn.setReadTimeout(timeout);
                conn.setRequestProperty("User-Agent", Daten.getUserAgent());
                if (datei.endsWith(GuiKonstanten.FORMAT_BZ2) || datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
                    // bei gezipten brauchen wir ein lokales Tempfile
                    tmpFile = File.createTempFile("mediathek", null);
                    //tmpFile.deleteOnExit();
                    BufferedInputStream in = new BufferedInputStream(conn.getInputStream());
                    FileOutputStream fOut = new FileOutputStream(tmpFile);
                    byte[] buffer = new byte[1024];
                    int n = 0;
                    int count = 0;
                    this.notifyProgress(datei);
                    while (!Daten.filmeLaden.getStop() && (n = in.read(buffer)) != -1) {
                        fOut.write(buffer, 0, n);
                        ++count;
                        if (count > 88) {
                            this.notifyProgress(datei);
                            count = 0;
                        }
                    }
                    try {
                        fOut.close();
                        in.close();
                    } catch (Exception e) {
                    }
                    if (datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
                        inReader = new InputStreamReader(new BZip2CompressorInputStream(new FileInputStream(tmpFile)), Konstanten.KODIERUNG_UTF);
                    } else if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
                        ZipInputStream zipInputStream = new ZipInputStream(new FileInputStream(tmpFile));
                        zipInputStream.getNextEntry();
                        inReader = new InputStreamReader(zipInputStream, Konstanten.KODIERUNG_UTF);
                    }
                } else {
                    inReader = new InputStreamReader(new FileInputStream(tmpFile), Konstanten.KODIERUNG_UTF);
                }
            }
            // jetzt die Datei speichern
            fOutWriter = new OutputStreamWriter(new FileOutputStream(Daten.getDateiFilmliste()), Konstanten.KODIERUNG_UTF);
            char[] buffer = new char[1024];
            int n = 0;
            int count = 0;
            this.notifyProgress(datei);
            while (!Daten.filmeLaden.getStop() && (n = inReader.read(buffer)) != -1) {
                fOutWriter.write(buffer, 0, n);
                ++count;
                if (count > 88) {
                    this.notifyProgress(datei);
                    count = 0;
                }
            }
        } catch (Exception ex) {
            ret = false;
            Log.fehlerMeldung(649830178, Log.FEHLER_ART_PROG, "IoXmlLesen.importDatenFilm", ex, "von: " + datei);
        } finally {
            try {
                if (fOutWriter != null) {
                    fOutWriter.close();
                }
                if (inReader != null) {
                    inReader.close();
                }
                if (tmpFile != null) {
                    tmpFile.delete();
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(728396587, Log.FEHLER_ART_PROG, "IoXmlLesen.importDatenFilm", ex);
            }
        }
        return ret;
    }

    public boolean dateiInListeEinlesen(String datei, boolean istUrl, ListeFilme listeFilme) {
        // die Filmliste "datei" wird in die List "listeFilme" eingelesen
        boolean ret = true;
        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        XMLStreamReader parser;
        InputStreamReader inReader = null;
        BZip2CompressorInputStream bZip2CompressorInputStream;
        int timeout = 10000; //10 Sekunden
        URLConnection conn;
        File tmpFile = null;
        try {
            if (!istUrl) {
                if (!new File(datei).exists()) {
                    return false;
                }
            }
            if (!istUrl) {
                if (datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
                    bZip2CompressorInputStream = new BZip2CompressorInputStream(new FileInputStream(datei));
                    inReader = new InputStreamReader(bZip2CompressorInputStream, Konstanten.KODIERUNG_UTF);
                } else if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
                    ZipInputStream zipInputStream = new ZipInputStream(new FileInputStream(datei));
                    zipInputStream.getNextEntry();
                    inReader = new InputStreamReader(zipInputStream, Konstanten.KODIERUNG_UTF);
                } else {
                    inReader = new InputStreamReader(new FileInputStream(datei), Konstanten.KODIERUNG_UTF);
                }
            } else {
                conn = new URL(datei).openConnection();
                conn.setConnectTimeout(timeout);
                conn.setReadTimeout(timeout);
                conn.setRequestProperty("User-Agent", Daten.getUserAgent());
                tmpFile = File.createTempFile("mediathek", null);
                //tmpFile.deleteOnExit();
                BufferedInputStream in = new BufferedInputStream(conn.getInputStream());
                FileOutputStream fOut = new FileOutputStream(tmpFile);
                byte[] buffer = new byte[1024];
                int n = 0;
                this.notifyProgress(datei);
                while (!Daten.filmeLaden.getStop() && (n = in.read(buffer)) != -1) {
                    fOut.write(buffer, 0, n);
                }
                fOut.close();
                in.close();
                if (datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
                    inReader = new InputStreamReader(new BZip2CompressorInputStream(new FileInputStream(tmpFile)), Konstanten.KODIERUNG_UTF);
                } else if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
                    ZipInputStream zipInputStream = new ZipInputStream(new FileInputStream(tmpFile));
                    zipInputStream.getNextEntry();
                    inReader = new InputStreamReader(zipInputStream, Konstanten.KODIERUNG_UTF);
                } else {
                    inReader = new InputStreamReader(new FileInputStream(tmpFile), Konstanten.KODIERUNG_UTF);
                }
            }
            parser = inFactory.createXMLStreamReader(inReader);
            ret = filmlisteXmlLesen(parser, datei, listeFilme);
        } catch (Exception ex) {
            ret = false;
            Log.fehlerMeldung(468956200, Log.FEHLER_ART_PROG, "IoXmlLesen.importDatenFilm", ex, "von: " + datei);
        } finally {
            try {
                if (inReader != null) {
                    inReader.close();
                }
                if (tmpFile != null) {
                    tmpFile.delete();
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(468983014, Log.FEHLER_ART_PROG, "IoXmlLesen.importDatenFilm", ex);
            }
        }
        return ret;
    }

    // ##############################
    // private
    // ##############################
    private String filmlisteUmbenennen() {
        String dest = "";
        try {
            if (Daten.listeFilme.isEmpty()) {
                Log.fehlerMeldung(312126987, Log.FEHLER_ART_PROG, "IoXmlLesen.filmlisteUmbenennen", "Die Filmliste ist leer.");
                return "";
            }
            String src = Daten.getDateiFilmliste();
            dest = Daten.getBasisVerzeichnis(false) + Daten.listeFilme.genDateRev() + "__" + Konstanten.XML_DATEI_FILME;
            if (src.equals(dest)) {
                return "";
            }
            File fileDest = new File(dest);
            if (fileDest.exists()) {
                Log.systemMeldung(new String[]{"Filmliste umbenennen: ", "Es gibt schon eine Liste mit dem Datum."});
                return "";
            }
            File fileSrc = new File(src);
            fileSrc.renameTo(fileDest);
            fileSrc = null;
            fileDest = null;
        } catch (Exception ex) {
            Log.fehlerMeldung(978451206, Log.FEHLER_ART_PROG, "IoXmlLesen.filmlisteUmbenennen", ex);
        }
        return dest;
    }

    private boolean filmlisteXmlLesen(XMLStreamReader parser, String text, ListeFilme listeFilme) throws XMLStreamException {
        boolean ret = true;
        int count = 0;
        DatenFilm datenFilm;
        String sender = "", thema = "";
        int event_;
        String filmTag = DatenFilm.FILME_;
        String[] namen = DatenFilm.COLUMN_NAMES_;
        while (!Daten.filmeLaden.getStop() && parser.hasNext()) {
            event_ = parser.next();
            //Filmeliste
            if (event_ == XMLStreamConstants.START_ELEMENT) {
                if (parser.getLocalName().equals(ListeFilme.FILMLISTE)) {
                    get(parser, ListeFilme.FILMLISTE, ListeFilme.COLUMN_NAMES, listeFilme.metaDaten, ListeFilme.MAX_ELEM);
                }
            }
            //Filme
            if (event_ == XMLStreamConstants.START_ELEMENT) {
                if (parser.getLocalName().equals(filmTag)) {
                    datenFilm = new DatenFilm();
                    get(parser, filmTag, namen, datenFilm.arr, DatenFilm.MAX_ELEM);
                    if (datenFilm.arr[DatenFilm.FILM_SENDER_NR].equals("")) {
                        datenFilm.arr[DatenFilm.FILM_SENDER_NR] = sender;
                    } else {
                        sender = datenFilm.arr[DatenFilm.FILM_SENDER_NR];
                    }
                    if (datenFilm.arr[DatenFilm.FILM_THEMA_NR].equals("")) {
                        datenFilm.arr[DatenFilm.FILM_THEMA_NR] = thema;
                    } else {
                        thema = datenFilm.arr[DatenFilm.FILM_THEMA_NR];
                    }
                    ++count;
                    if (count > 790) {
                        count = 0;
                        this.notifyProgress(text);
                    }
                    listeFilme.addWithNr(datenFilm);
                }
            }
        }
        return ret;
    }

    private void get(XMLStreamReader parser, String xmlElem, String[] xmlNames, String[] strRet, int maxElem) throws XMLStreamException {
        ii = 0;
        outer:
        while (parser.hasNext()) {
            event = parser.next();
            if (event == XMLStreamConstants.END_ELEMENT) {
                if (parser.getLocalName().equals(xmlElem)) {
                    break;
                }
            }
            if (event == XMLStreamConstants.START_ELEMENT) {
                for (i = ii; i < maxElem; ++i) {
                    // String s = parser.getLocalName();
                    if (parser.getLocalName().equals(xmlNames[i])) {
                        strRet[i] = parser.getElementText();
                        ii = ++i;
                        continue outer;
                    }
                }
                for (i = 0; i < maxElem; ++i) {
                    // String s = parser.getLocalName();
                    if (parser.getLocalName().equals(xmlNames[i])) {
                        strRet[i] = parser.getElementText();
                        continue outer;
                    }
                }
            }
        }
    }

    private void notifyStart(int mmax) {
        max = mmax;
        progress = 0;
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.start(new ListenerFilmeLadenEvent("", "", max, 0));
        }
    }

    private void notifyProgress(String text) {
        if (progress < max) {
            progress += 1;
        }
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.progress(new ListenerFilmeLadenEvent("", text, max, progress));
        }
    }

    private void notifyFertig(ListeFilme liste) {
        Log.systemMeldung("Liste Filme gelesen: " + DatumZeit.getHeute_dd_MM_yyyy() + " " + DatumZeit.getJetzt_HH_MM_SS());
        Log.systemMeldung("Anzahl Filme: " + liste.size());
        for (ListenerFilmeLaden l : listeners.getListeners(ListenerFilmeLaden.class)) {
            l.fertig(new ListenerFilmeLadenEvent("", "", max, progress));
        }
    }
}
