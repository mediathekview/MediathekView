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

import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.URL;
import java.net.URLConnection;
import java.util.LinkedList;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.ListePset;
import mediathek.file.GetFile;
import mediathek.tool.Funktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;
import mediathek.tool.TModel;

public class ListePsetVorlagen extends LinkedList<String[]> {

    public static final String BS_WIN_32 = "Windows-32Bit";
    public static final String BS_WIN_64 = "Windows-64Bit";
    public static final String BS_LINUX = "Linux";
    public static final String BS_MAC = "Mac";
    public static final String[] BS = {"", BS_WIN_32, BS_WIN_64, BS_LINUX, BS_MAC};
    //
    public static final String PGR = "Vorlage";
    public static final int PGR_MAX_ELEM = 4;
    public static final String PGR_NAME = "Name";
    public static final int PGR_NAME_NR = 0;
    public static final String PGR_BESCHREIBUNG = "Beschreibung";
    public static final int PGR_BESCHREIBUNG_NR = 1;
    public static final String PGR_BS = "Bs";
    public static final int PGR_BS_NR = 2;
    public static final String PGR_URL = "URL";
    public static final int PGR_URL_NR = 3;
    public static final String[] PGR_COLUMN_NAMES = {PGR_NAME, PGR_BESCHREIBUNG, PGR_BS, PGR_URL};
    // private
    private final int timeout = 10000;

    public TModel getTModel(String bs) {
        LinkedList<String[]> tmp = new LinkedList<String[]>();
        String[][] object;
        if (this.size() > 0) {
            if (!bs.equals("")) {
                for (int i = 0; i < this.size(); i++) {
                    if (this.get(i)[PGR_BS_NR].contains(bs)) {
                        tmp.add(this.get(i));
                    }
                }
                object = new String[tmp.size()][PGR_MAX_ELEM];
                for (int i = 0; i < tmp.size(); i++) {
                    object[i] = tmp.get(i);
                }
            } else {
                object = new String[this.size()][PGR_MAX_ELEM];
                for (int i = 0; i < this.size(); i++) {
                    object[i] = this.get(i);
                }
            }
            TModel model = new TModel(object, PGR_COLUMN_NAMES);
            return model;
        } else {
            return new TModel(new Object[][]{}, PGR_COLUMN_NAMES);
        }
    }

    public boolean getListe() {
        try {
            this.clear();
            int event;
            XMLInputFactory inFactory = XMLInputFactory.newInstance();
            inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
            XMLStreamReader parser;
            InputStreamReader inReader;
            URLConnection conn;
            conn = new URL(Konstanten.ADRESSE_VORLAGE_PROGRAMMGRUPPEN).openConnection();
            conn.setRequestProperty("User-Agent", Daten.getUserAgent());
            conn.setReadTimeout(timeout);
            conn.setConnectTimeout(timeout);
            inReader = new InputStreamReader(conn.getInputStream(), Konstanten.KODIERUNG_UTF);
            parser = inFactory.createXMLStreamReader(inReader);
            while (parser.hasNext()) {
                event = parser.next();
                if (event == XMLStreamConstants.START_ELEMENT) {
                    if (parser.getLocalName().equals(PGR)) {
                        //wieder ein neuer Server, toll
                        String[] p = new String[PGR_MAX_ELEM];
                        get(parser, event, PGR, PGR_COLUMN_NAMES, p);
                        if (!p[PGR_URL_NR].equals("")) {
                            this.add(p);
                        }
                    }
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(398001963, Log.FEHLER_ART_PROG, "VorlageProgrammgruppen.getListe", ex);
            return false;
        }
        return true;
    }

    public static ListePset getStandarset(DDaten ddaten, String bs) {
        ListePset pSet = null;
        String[] vorlage = null;
        ListePsetVorlagen lv = new ListePsetVorlagen();
        if (lv.getListe()) {
            for (String[] ar : lv) {
                if (ar[PGR_NAME_NR].equalsIgnoreCase("Standardset " + bs)) {
                    vorlage = ar;
                    break;
                }
            }
            if (vorlage != null) {
                if (!vorlage[PGR_URL_NR].equals("")) {
                    pSet = IoXmlLesen.importPset(ddaten, vorlage[ListePsetVorlagen.PGR_URL_NR], true);
                }
            }
        }
        if (pSet == null) {
            // dann nehmen wir halt die im jar-File
            pSet = getStandardprogramme(ddaten);
        }
        return pSet;
    }

    public static ListePset getStandardprogramme(DDaten ddaten) {
        // liefert das Standard Programmset für das entsprechende BS
        ListePset pSet;
        InputStream datei;
        switch (Funktionen.getOs()) {
            case Funktionen.OS_LINUX:
                datei = new GetFile().getPsetVorlageLinux();
                break;
            case Funktionen.OS_MAC:
                datei = new GetFile().getPsetVorlageMac();
                break;
            case Funktionen.OS_WIN_32BIT:
            case Funktionen.OS_WIN_64BIT:
            case Funktionen.OS_UNKNOWN:
            default:
                datei = new GetFile().getPsetVorlageWindows();
        }
        // Standardgruppen laden
        pSet = IoXmlLesen.importPset(ddaten, datei, true);
        return pSet;
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
            Log.fehlerMeldung(467256394, Log.FEHLER_ART_PROG, "VorlageProgrammgruppen.get", ex);
        }
        return ret;
    }
}
