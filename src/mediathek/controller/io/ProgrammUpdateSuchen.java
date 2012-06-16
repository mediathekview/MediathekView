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
package mediathek.controller.io;

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
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.gui.dialog.DialogHinweisUpdate;
import mediathek.tool.DatumZeit;

public class ProgrammUpdateSuchen {

    public boolean checkVersion(DDaten ddaten, boolean anzeigen) {
        // prüft auf neue Version, aneigen: wenn true, dann AUCH wenn es keine neue Version gibt ein Fenster
        String version;
        String release;
        String downloadUrlProgramm;
        String[] ret;
        boolean neueVersion = false;
        try {
            ret = suchen();
            version = ret[0];
            release = ret[1];
            downloadUrlProgramm = ret[2];
            if (!version.equals("")) {
                Daten.system[Konstanten.SYSTEM_UPDATE_DATUM_NR] = DatumZeit.getHeute_yyyyMMdd();
                if (checkObNeueVersion(version, Konstanten.VERSION)) {
                    neueVersion = true;
                    // DialogHinweisUpdate(java.awt.Frame parent, boolean modal, String ttext, String dialogTitel, DDaten ddaten) {
                    new DialogHinweisUpdate(null, true,
                            "   ==================================================\n"
                            + "   Neue Version:\n" + "   " + version + "\n\n"
                            + "   ==================================================\n"
                            + "   Änderungen:\n" + "   " + release + "\n\n"
                            + "   ==================================================\n"
                            + "   URL:\n"
                            + "   " + downloadUrlProgramm + "\n\n",
                            "Eine neue Version liegt vor").setVisible(true);
                } else {
                    DialogHinweisUpdate dialog = new DialogHinweisUpdate(null, true, "Alles aktuell!", "Update suchen");
                    if (anzeigen) {
                        dialog.setVisible(true);
                    }
                }
            } else {
                new DialogHinweisUpdate(null, true, "Es ist ein Fehler aufgetreten!" + "\n\n" + "", "Fehler bei der Versionsprüfung!").setVisible(true);
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(159002583,"FilmUpdateServer.checkVersion", ex);
        }
        return neueVersion;
    }

    private boolean checkObNeueVersion(String infoVersion, String ichVersion) {
        // liefert true, wenn es eine neue Version gibt
        try {
            // erste stelle
            int info = Integer.parseInt(infoVersion.substring(0, 1) + infoVersion.substring(2, 3) + infoVersion.substring(4, 5));
            int ich = Integer.parseInt(ichVersion.substring(0, 1) + ichVersion.substring(2, 3) + ichVersion.substring(4, 5));
            if (info > ich) {
                return true;
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(683021193,"FilmUpdateServer.checkObNeueVersion", ex);
        }
        return false;
    }

    private String[] suchen() throws MalformedURLException, IOException, XMLStreamException {
        String[] ret = new String[]{""/* version */, ""/* release */, ""/* updateUrl */};
        //String parsername = "";
        int event;
        XMLInputFactory inFactory = XMLInputFactory.newInstance();
        inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
        XMLStreamReader parser;
        InputStreamReader inReader;
        int timeout = 10000;
        URLConnection conn;
        conn = new URL(Konstanten.ADRESSE_PROGRAMM_VERSION).openConnection();
        conn.setRequestProperty("User-Agent", Daten.getUserAgent());
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
                }
            }
        }
        return ret;
    }
}
