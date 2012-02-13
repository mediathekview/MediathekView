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
package mediathek.importOld;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStreamReader;
import javax.swing.JOptionPane;
import javax.xml.stream.XMLInputFactory;
import javax.xml.stream.XMLStreamConstants;
import javax.xml.stream.XMLStreamReader;
import mediathek.daten.*;

public class IoXmlLesen__old {
    
    public ListePgruppe__old listePgruppeButton = new ListePgruppe__old();
    public ListePgruppe__old listePgruppeAbo = new ListePgruppe__old();
    public ListeAbo__old listeAbo = new ListeAbo__old();
    public ListeBlacklist listeBlacklist = new ListeBlacklist();
    public String[] system = new String[Konstanten__old.SYSTEM_MAX_ELEM];

    // ##############################
    // private
    // ##############################
    public void importOld(DDaten ddaten) {
        String datei = getBasisVerzeichnis() + Konstanten__old.XML_DATEI;
        DialogImportOld dialogImportOld = new DialogImportOld(null, true, datei);
        dialogImportOld.setVisible(true);
        if (!dialogImportOld.ok) {
            // Satz mit X, war wohl nix
            return;
        }
        xmlDatenLesen(dialogImportOld.ziel);
        // Liste Buttons importieren
        for (int i = 0; i < listePgruppeButton.size(); ++i) {
            DatenPgruppe__old gruppe = listePgruppeButton.get(i);
            DatenPgruppe gruppeNeu = gruppe.getNewVersion();
            gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.TRUE.toString();
            gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "Button" + String.valueOf(i) + "-" + gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR];
            ListeProg__old listeProg = gruppe.getListeProg();
            for (int l = 0; l < listeProg.size(); ++l) {
                DatenProg progNeu = listeProg.get(l).getNewVersion();
                gruppeNeu.addProg(progNeu);
            }
            ddaten.listePgruppe.addPgruppe(gruppeNeu);
        }
        // erster Button=abspielen
        ddaten.listePgruppe.getListeButton().getFirst().setAbspielen(ddaten);
        // Liste Abos importieren
        for (int i = 0; i < listePgruppeAbo.size(); ++i) {
            DatenPgruppe__old gruppe = listePgruppeAbo.get(i);
            DatenPgruppe gruppeNeu = gruppe.getNewVersion();
            gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABO_NR] = Boolean.TRUE.toString();
            gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = "PGruppeAbo" + String.valueOf(i) + "-" + gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR];
            ListeProg__old listeProg = gruppe.getListeProg();
            for (int l = 0; l < listeProg.size(); ++l) {
                DatenProg progNeu = listeProg.get(l).getNewVersion();
                gruppeNeu.addProg(progNeu);
            }
            ddaten.listePgruppe.addPgruppe(gruppeNeu);
        }
        // ersters Abo=speichern
        ddaten.listePgruppe.getListeAbo().getFirst().setSpeichern(true);
        // Liste Abos importieren
        for (int i = 0; i < listeAbo.size(); ++i) {
            DatenAbo__old abo = listeAbo.get(i);
            DatenAbo aboNeu = abo.getNewVersion();
            ddaten.listeAbo.addAbo(aboNeu);
        }
        // Liste Blacklist
        for (int i = 0; i < listeBlacklist.size(); ++i) {
            ddaten.listeBlacklist.add(listeBlacklist.get(i));
        }
    }
    
    public static boolean altExistiert() {
        try {
            String datei = getBasisVerzeichnis() + Konstanten__old.XML_DATEI;
            if (new File(datei).exists()) {
                return true;
            }
        } catch (Exception ex) {
        }
        return false;
    }
    
    private void xmlDatenLesen(String datei) {
        try {
            if (new File(datei).exists()) {
                //nur wenn die Datei schon existiert
                int event;
                XMLInputFactory inFactory = XMLInputFactory.newInstance();
                inFactory.setProperty(XMLInputFactory.IS_COALESCING, Boolean.FALSE);
                XMLStreamReader parser;
                InputStreamReader in;
                DatenPgruppe__old datenPgruppe = null;
                in = new InputStreamReader(new FileInputStream(datei), Konstanten__old.KODIERUNG_UTF);
                parser = inFactory.createXMLStreamReader(in);
                while (parser.hasNext()) {
                    event = parser.next();
                    if (event == XMLStreamConstants.START_ELEMENT) {
                        //String t = parser.getLocalName();
                        if (parser.getLocalName().equals(Konstanten__old.SYSTEM)) {
                            //System
                            get(parser, event, Konstanten__old.SYSTEM, Konstanten__old.SYSTEM_COLUMN_NAMES, system);
                        } else if (parser.getLocalName().equals(Konstanten__old.PROGRAMMGRUPPE_ABO)) {
                            //Programmgruppen
                            datenPgruppe = new DatenPgruppe__old();
                            if (get(parser, event, Konstanten__old.PROGRAMMGRUPPE_ABO, Konstanten__old.PROGRAMMGRUPPE_COLUMN_NAMES, datenPgruppe.arr)) {
                                listePgruppeAbo.add(datenPgruppe);
                            }
                        } else if (parser.getLocalName().equals(Konstanten__old.PROGRAMMGRUPPE_BUTTON)) {
                            datenPgruppe = new DatenPgruppe__old();
                            if (get(parser, event, Konstanten__old.PROGRAMMGRUPPE_BUTTON, Konstanten__old.PROGRAMMGRUPPE_COLUMN_NAMES, datenPgruppe.arr)) {
                                listePgruppeButton.add(datenPgruppe);
                            }
                        } else if (parser.getLocalName().equals(Konstanten__old.PROGRAMM)) {
                            DatenProg__old datenProg = new DatenProg__old();
                            if (get(parser, event, Konstanten__old.PROGRAMM, Konstanten__old.PROGRAMM_COLUMN_NAMES, datenProg.arr)) {
                                datenPgruppe.addProg(datenProg);
                            }
                            //ende Programgruppen
                        } else if (parser.getLocalName().equals(Konstanten__old.ABO)) {
                            //Abo
                            DatenAbo__old datenAbo = new DatenAbo__old();
                            if (get(parser, event, Konstanten__old.ABO, Konstanten__old.ABO_COLUMN_NAMES, datenAbo.arr)) {
                                if (!Boolean.parseBoolean(datenAbo.arr[Konstanten__old.ABO_EINMAL_ERLEDIGT_NR])) {
                                    // keine fertigen Einmalabos laden
                                    listeAbo.addAbo(datenAbo);
                                }
                            }
                        } else if (parser.getLocalName().equals(Konstanten__old.BLACKLIST)) {
                            //Blacklist
                            DatenBlacklist blacklist = new DatenBlacklist();
                            if (get(parser, event, Konstanten__old.BLACKLIST, Konstanten__old.BLACKLIST_COLUMN_NAMES, blacklist.arr)) {
                                listeBlacklist.add(blacklist);
                            }
                        }
                    }
                }
            }
        } catch (Exception ex) {
            JOptionPane.showMessageDialog(null, "Die alten Einstellungen konnten nicht alle importiert werden!",
                    "Fehler", JOptionPane.ERROR_MESSAGE);
        }
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
        }
        return ret;
    }
    
    private static String getBasisVerzeichnis() {
        return System.getProperty("user.home") + File.separator + ".mediathek" + File.separator;
    }
}
