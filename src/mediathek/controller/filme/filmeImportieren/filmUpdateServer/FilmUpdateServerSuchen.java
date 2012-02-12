/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.filme.filmeImportieren.filmUpdateServer;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamException;
import javax.xml.stream.XMLStreamReader;
import mediathek.Daten;
import mediathek.Konstanten;

public class FilmUpdateServerSuchen {

    public static String[] getListe(ListeFilmUpdateServer sListe) throws MalformedURLException, IOException, XMLStreamException {
        String[] ret = new String[]{""/*version*/, ""/*release*/, ""/*updateUrl*/};
        //String parsername = "";
        sListe.clear();
        int event;
        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        XMLStreamReader parser = null;
        InputStreamReader inReader = null;
        int timeout = 10000;
        URLConnection conn = null;
        conn = new URL(Konstanten.ADRESSE_UPDATE).openConnection();
        conn.setRequestProperty("User-Agent",  Daten.getUserAgent());
        conn.setReadTimeout(timeout);
        conn.setConnectTimeout(timeout);
        inReader = new InputStreamReader(conn.getInputStream(), Konstanten.KODIERUNG_UTF);
        parser = inFactory.createXMLStreamReader(inReader);
        while (parser.hasNext()) {
            event = parser.next();
            if (event == XMLStreamConstants.START_ELEMENT) {
                //parsername = parser.getLocalName();
                if (parser.getLocalName().equals("Program_Version")) {
                    ret[0] = parser.getElementText();
                } else if (parser.getLocalName().equals("Program_Release_Info")) {
                    ret[1] = parser.getElementText();
                } else if (parser.getLocalName().equals("Download_Programm")) {
                    ret[2] = parser.getElementText();
                } else if (parser.getLocalName().equals("Server")) {
                    //wieder ein neuer Server, toll
                    getServer(parser, sListe);
                }
            }
        }
        return ret;
    }

    private static void getServer(XMLStreamReader parser, ListeFilmUpdateServer sListe) {
        String anzahl = "";
        String zeit = "";
        String datum = "";
        String filmUrl = "";
        //String parsername = "";
        String prio = "";
        int event;
        try {
            while (parser.hasNext()) {
                prio = FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_1;
                event = parser.next();
                if (event == XMLStreamConstants.END_ELEMENT) {
                    //parsername = parser.getLocalName();
                    if (parser.getLocalName().equals("Server")) {
                        if (!filmUrl.equals("")) {
                            //public DatenFilmUpdate(String url, String prio, String zeit, String datum, String anzahl) {
                            sListe.addWithCheck(new DatenFilmUpdateServer(filmUrl, prio, zeit, datum, anzahl));
                        }
                        break;
                    }
                }
                if (event == XMLStreamConstants.START_ELEMENT) {
                    //parsername = parser.getLocalName();
                    if (parser.getLocalName().equals("Download_Filme_1")) {
                        filmUrl = parser.getElementText();
                        prio = FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_1;
                    } else if (parser.getLocalName().equals("Download_Filme_2")) {
                        filmUrl = parser.getElementText();
                        prio = FilmUpdateServer.FILM_UPDATE_SERVER_PRIO_2;
                    } else if (parser.getLocalName().equals("Datum")) {
                        datum = parser.getElementText();
                    } else if (parser.getLocalName().equals("Zeit")) {
                        zeit = parser.getElementText();
                    } else if (parser.getLocalName().equals("Anzahl")) {
                        anzahl = parser.getElementText();
                    }
                }
            }
        } catch (XMLStreamException ex) {
        }

    }
}
