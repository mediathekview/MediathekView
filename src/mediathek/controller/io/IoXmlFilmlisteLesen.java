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

import java.io.*;
import java.net.URL;
import java.net.URLConnection;
import java.util.zip.ZipInputStream;
import javax.swing.event.EventListenerList;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.Log;
import mediathek.controller.filme.FilmListener;
import mediathek.controller.filme.FilmListenerElement;
import mediathek.daten.DatenFilm;
import mediathek.daten.ListeFilme;
import mediathek.tool.DatumZeit;
import mediathek.tool.GuiKonstanten;
import org.apache.commons.compress.compressors.bzip2.BZip2CompressorInputStream;

public class IoXmlFilmlisteLesen {

    private EventListenerList listeners = new EventListenerList();
    private int max = 0;
    private int progress = 0;

    public void addAdListener(FilmListener listener) {
        listeners.add(FilmListener.class, listener);
    }

    /**
     *
     * @param datei
     * @param zip
     * @param istDatei
     * @return
     */
    public boolean filmlisteLesen(String datei, boolean istUrl, ListeFilme listeFilme) {
        boolean ret = true;
        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        XMLStreamReader parser;
        InputStreamReader inReader = null;
        BZip2CompressorInputStream bZip2CompressorInputStream;
        int timeout = 10000; //10 Sekunden
        URLConnection conn;
        try {
            if (!istUrl) {
                if (!new File(datei).exists()) {
                    return false;
                }
            }
            if (istUrl && datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
                // da wird eine temp-Datei benutzt
                this.notifyStart(1000);
                this.notifyProgress(datei);
            } else {
                this.notifyStart(500);
                this.notifyProgress(datei);
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
                if (datei.endsWith(GuiKonstanten.FORMAT_BZ2)) {
                    File tmpFile = File.createTempFile("mediathek", null);
                    tmpFile.deleteOnExit();
                    BufferedInputStream in = new BufferedInputStream(conn.getInputStream());
                    FileOutputStream fOut = new FileOutputStream(tmpFile);
                    final byte[] buffer = new byte[100];
                    int n = 0;
                    int count = 0;
                    this.notifyProgress(datei);
                    while (!Daten.filmeLaden.getStop() && -1 != (n = in.read(buffer))) {
                        fOut.write(buffer, 0, n);
                        ++count;
                        if (count > 30) {
                            this.notifyProgress("Download: " + datei);
                            count = 0;
                        }
                    }
                    fOut.close();
                    in.close();
                    inReader = new InputStreamReader(new BZip2CompressorInputStream(new FileInputStream(tmpFile)), Konstanten.KODIERUNG_UTF);
                } else if (datei.endsWith(GuiKonstanten.FORMAT_ZIP)) {
                    ZipInputStream zipInputStream = new ZipInputStream(conn.getInputStream());
                    zipInputStream.getNextEntry();
                    inReader = new InputStreamReader(zipInputStream, Konstanten.KODIERUNG_UTF);
                } else {
                    inReader = new InputStreamReader(conn.getInputStream(), Konstanten.KODIERUNG_UTF);
                }
            }
            parser = inFactory.createXMLStreamReader(inReader);
            ret = datenFilmlisteLesen(parser, datei, listeFilme);
        } catch (Exception ex) {
            ret = false;
            Log.fehlerMeldung("IoXmlLesen.importDatenFilm", ex, "von: " + datei);
        } finally {
            try {
                if (inReader != null) {
                    inReader.close();
                }
            } catch (Exception ex) {
                Log.fehlerMeldung("IoXmlLesen.importDatenFilm", ex);
            }
        }
        this.notifyFertig(listeFilme);
        return ret;
    }

    // ##############################
    // private
    // ##############################
    private boolean datenFilmlisteLesen(XMLStreamReader parser, String text, ListeFilme listeFilme) {
        boolean ret = true;
        int count = 0;
        DatenFilm datenFilm;
        DatenFilm datenFilmAlt = new DatenFilm();
        try {
            int event;
            String filmTag = DatenFilm.FILME_;
            String[] namen = DatenFilm.FILME_COLUMN_NAMES_;
            while (!Daten.filmeLaden.getStop() && parser.hasNext()) {
                event = parser.next();
                //Filmeliste
                if (event == XMLStreamConstants.START_ELEMENT) {
                    if (parser.getLocalName().equals(ListeFilme.FILMLISTE)) {
                        get(parser, event, ListeFilme.FILMLISTE, ListeFilme.FILMLISTE_COLUMN_NAMES, listeFilme.metaDaten);
                        if (listeFilme.metaDaten[ListeFilme.FILMLISTE_VERSION_NR].startsWith("3")) {
                            filmTag = DatenFilm.FILME_;
                            namen = DatenFilm.FILME_COLUMN_NAMES_;
                        } else {
                            filmTag = DatenFilm.FILME;
                            namen = DatenFilm.FILME_COLUMN_NAMES;
                        }
                        int anz = 1;
                        try {
                            anz = Integer.parseInt(listeFilme.metaDaten[ListeFilme.FILMLISTE_ANZAHL_NR]);
                        } catch (Exception ex) {
                        }
                    }
                }
                //FilmeInfos
                if (event == XMLStreamConstants.START_ELEMENT) {
                    if (parser.getLocalName().equals(ListeFilme.FILMLISTE_INFOS)) {
                        get(parser, event, ListeFilme.FILMLISTE_INFOS, ListeFilme.FILMLISTE_INFOS_COLUMN_NAMES, listeFilme.infos);
                    }
                }
                //Filme
                if (event == XMLStreamConstants.START_ELEMENT) {
                    if (parser.getLocalName().equals(filmTag)) {
                        datenFilm = new DatenFilm();
                        if (get(parser, event, filmTag, namen, datenFilm.arr)) {
                            if (datenFilm.arr[DatenFilm.FILM_SENDER_NR].equals("")) {
                                datenFilm.arr[DatenFilm.FILM_SENDER_NR] = datenFilmAlt.arr[DatenFilm.FILM_SENDER_NR];
                            }
                            if (datenFilm.arr[DatenFilm.FILM_THEMA_NR].equals("")) {
                                datenFilm.arr[DatenFilm.FILM_THEMA_NR] = datenFilmAlt.arr[DatenFilm.FILM_THEMA_NR];
                            }
                            ++count;
                            if (count > 100) {
                                count = 0;
                                this.notifyProgress(text);
                            }
                            listeFilme.addWithNr(datenFilm);
                            datenFilmAlt = datenFilm;
                        }
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung("IoXml.datenLesenFilme", ex);
            ret = false;
        }
        return ret;
    }

    private boolean get(XMLStreamReader parser, int event, String xmlElem, String[] xmlNames, String[] strRet) {
        boolean ret = true;
        int maxElem = strRet.length;
        for (int i = 0; i < maxElem; ++i) {
            strRet[i] = "";
        }
        try {
            while (parser.hasNext()) {
                event = parser.next();
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
            Log.fehlerMeldung("IoXmlLesen.get", ex);
        }
        return ret;
    }

    private void notifyStart(int mmax) {
        max = mmax;
        progress = 0;
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.start(new FilmListenerElement("", "", max, 0));
        }
    }

    private void notifyProgress(String text) {
        if (progress < max) {
            progress += 1;
        }
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.progress(new FilmListenerElement("", text, max, progress));
        }
    }

    private void notifyFertig(ListeFilme liste) {
        Log.systemMeldung("Liste Filme gelesen: " + DatumZeit.getHeute_dd_MM_yyyy() + " " + DatumZeit.getJetzt_HH_MM_SS());
        Log.systemMeldung("Anzahl Filme: " + liste.size());
        for (FilmListener l : listeners.getListeners(FilmListener.class)) {
            l.fertig(new FilmListenerElement("", "", max, progress));
        }
    }
}
