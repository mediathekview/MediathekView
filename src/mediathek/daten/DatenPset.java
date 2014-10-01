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
 // *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import com.jidesoft.utils.SystemInfo;
import java.awt.Color;
import java.util.Iterator;
import java.util.ListIterator;
import javax.swing.JOptionPane;
import mediathek.controller.Log;
import mediathek.tool.GuiFunktionenProgramme;
import mediathek.tool.MVMessageDialog;

public class DatenPset {

    public static final String PROGRAMMSET = "Programmset";
    //Tags Programmgruppen
    public static final String PROGRAMMSET_NAME = "Name";
    public static final int PROGRAMMSET_NAME_NR = 0;
    public static final String PROGRAMMSET_PRAEFIX_DIREKT = "Praefix";
    public static final int PROGRAMMSET_PRAEFIX_DIREKT_NR = 1;
    public static final String PROGRAMMSET_SUFFIX_DIREKT = "Suffix";
    public static final int PROGRAMMSET_SUFFIX_DIREKT_NR = 2;
    public static final String PROGRAMMSET_FARBE = "Farbe";
    public static final int PROGRAMMSET_FARBE_NR = 3;
    public static final String PROGRAMMSET_ZIEL_PFAD = "Zielpfad";
    public static final int PROGRAMMSET_ZIEL_PFAD_NR = 4;
    public static final String PROGRAMMSET_ZIEL_DATEINAME = "Zieldateiname";
    public static final int PROGRAMMSET_ZIEL_DATEINAME_NR = 5;
    public static final String PROGRAMMSET_THEMA_ANLEGEN = "Thema-anlegen";
    public static final int PROGRAMMSET_THEMA_ANLEGEN_NR = 6;
    public static final String PROGRAMMSET_IST_ABSPIELEN = "Abspielen";
    public static final int PROGRAMMSET_IST_ABSPIELEN_NR = 7;
    public static final String PROGRAMMSET_IST_SPEICHERN = "Speichern";
    public static final int PROGRAMMSET_IST_SPEICHERN_NR = 8;
    public static final String PROGRAMMSET_IST_BUTTON = "Button";
    public static final int PROGRAMMSET_IST_BUTTON_NR = 9;
    public static final String PROGRAMMSET_IST_ABO = "Abo";
    public static final int PROGRAMMSET_IST_ABO_NR = 10;
    public static final String PROGRAMMSET_LAENGE_BESCHRAENKEN = "Laenge";
    public static final int PROGRAMMSET_LAENGE_BESCHRAENKEN_NR = 11;
    public static final String PROGRAMMSET_MAX_LAENGE = "max-Laenge";
    public static final int PROGRAMMSET_MAX_LAENGE_NR = 12;
    public static final String PROGRAMMSET_AUFLOESUNG = "Aufloesung";
    public static final int PROGRAMMSET_AUFLOESUNG_NR = 13;
    public static final String PROGRAMMSET_ADD_ON = "AddOn";
    public static final int PROGRAMMSET_ADD_ON_NR = 14;
    public static final String PROGRAMMSET_BESCHREIBUNG = "Beschreibung";
    public static final int PROGRAMMSET_BESCHREIBUNG_NR = 15;
    public static final String PROGRAMMSET_INFODATEI = "Infodatei";
    public static final int PROGRAMMSET_INFODATEI_NR = 16;
    public static final String PROGRAMMSET_SPOTLIGHT = "Spotlight";
    public static final int PROGRAMMSET_SPOTLIGHT_NR = 17;
    public static final int MAX_ELEM = 18;
    public static final String[] COLUMN_NAMES = {"Setname", "Präfix", PROGRAMMSET_SUFFIX_DIREKT,
        PROGRAMMSET_FARBE, PROGRAMMSET_ZIEL_PFAD, PROGRAMMSET_ZIEL_DATEINAME, PROGRAMMSET_THEMA_ANLEGEN,
        PROGRAMMSET_IST_ABSPIELEN, PROGRAMMSET_IST_SPEICHERN, PROGRAMMSET_IST_BUTTON, PROGRAMMSET_IST_ABO,
        PROGRAMMSET_LAENGE_BESCHRAENKEN, PROGRAMMSET_MAX_LAENGE, "Auflösung", PROGRAMMSET_ADD_ON,
        PROGRAMMSET_BESCHREIBUNG, PROGRAMMSET_INFODATEI, PROGRAMMSET_SPOTLIGHT};
    public static final String[] COLUMN_NAMES_ = {PROGRAMMSET_NAME, PROGRAMMSET_PRAEFIX_DIREKT, PROGRAMMSET_SUFFIX_DIREKT,
        PROGRAMMSET_FARBE, PROGRAMMSET_ZIEL_PFAD, PROGRAMMSET_ZIEL_DATEINAME, PROGRAMMSET_THEMA_ANLEGEN,
        PROGRAMMSET_IST_ABSPIELEN, PROGRAMMSET_IST_SPEICHERN, PROGRAMMSET_IST_BUTTON, PROGRAMMSET_IST_ABO,
        PROGRAMMSET_LAENGE_BESCHRAENKEN, PROGRAMMSET_MAX_LAENGE, PROGRAMMSET_AUFLOESUNG, PROGRAMMSET_ADD_ON,
        PROGRAMMSET_BESCHREIBUNG, PROGRAMMSET_INFODATEI, PROGRAMMSET_SPOTLIGHT};
    private ListeProg listeProg = new ListeProg();
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public String[] arr;

    public DatenPset() {
        makeArray();
    }

    public DatenPset(String name) {
        // neue Pset sind immer gleich Button
        makeArray();
        arr[PROGRAMMSET_NAME_NR] = name;
        arr[PROGRAMMSET_IST_BUTTON_NR] = Boolean.TRUE.toString();
    }

    // public
    public boolean addProg(DatenProg prog) {
        return listeProg.add(prog);
    }

    public ListeProg getListeProg() {
        return listeProg;
    }

    public DatenProg getProg(int i) {
        return listeProg.get(i);
    }

    public boolean progsContainPath() {
        // ein Programmschalter mit "**"
        boolean ret = false;
        Iterator<DatenProg> it = listeProg.iterator();
        DatenProg prog;
        while (it.hasNext()) {
            prog = it.next();
            if (prog.arr[DatenProg.PROGRAMM_SCHALTER_NR].contains("**")) {
                ret = true;
                break;
            }
        }
        return ret;
    }

    public boolean istAbspielen() {
        return Boolean.parseBoolean(arr[PROGRAMMSET_IST_ABSPIELEN_NR]);
    }

    public boolean istSpeichern() {
        return Boolean.parseBoolean(arr[PROGRAMMSET_IST_SPEICHERN_NR]);
    }

    public boolean istButton() {
        return Boolean.parseBoolean(arr[PROGRAMMSET_IST_BUTTON_NR]);
    }

    public boolean istAbo() {
        return Boolean.parseBoolean(arr[PROGRAMMSET_IST_ABO_NR]);
    }

    public boolean isLable() {
        // wenn die Programmliste leer ist und einen Namen hat, ist es ein Lable
        if (this.listeProg.isEmpty()) {
            if (!this.arr[PROGRAMMSET_NAME_NR].equals("")) {
                return true;
            }
        }
        return false;
    }

    public boolean isFreeLine() {
        //Wenn die Programmgruppe keinen Namen hat, leere Zeile
        return this.arr[PROGRAMMSET_NAME_NR].equals("");
    }

    public void setAbspielen(Daten daten) {
        for (DatenPset datenPset : Daten.listePset) {
            datenPset.arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR] = Boolean.FALSE.toString();
        }
        arr[DatenPset.PROGRAMMSET_IST_ABSPIELEN_NR] = Boolean.TRUE.toString();
    }

    public void setSpeichern(boolean set) {
        arr[DatenPset.PROGRAMMSET_IST_SPEICHERN_NR] = Boolean.toString(set);
    }

    public void setButton(boolean set) {
        arr[DatenPset.PROGRAMMSET_IST_BUTTON_NR] = Boolean.toString(set);
    }

    public void setAbo(boolean set) {
        arr[DatenPset.PROGRAMMSET_IST_ABO_NR] = Boolean.toString(set);
    }

    public DatenProg getProgUrl(String url) {
        //mit einer Url das Passende Programm finden
        //passt nichts, wird das letzte Programm genommen
        //ist nur ein Programm in der Liste wird dieses genommen
        DatenProg ret = null;
        if (listeProg.isEmpty()) {
            MVMessageDialog.showMessageDialog(null, "Programme einrichten!",
                    "Kein Programm", JOptionPane.INFORMATION_MESSAGE);
        } else if (listeProg.size() == 1) {
            ret = listeProg.getFirst();
        } else {
            Iterator<DatenProg> it = listeProg.iterator();
            DatenProg prog;
            while (it.hasNext()) {
                prog = it.next();
                if (prog.urlTesten(url)) {
                    ret = prog;
                    break;
                }
            }
            if (!listeProg.isEmpty() && ret == null) {
                ret = listeProg.getLast();
            }
        }
        return ret;
    }

    public String getZielDateiname(String url) {
        //gibt den Zieldateinamen für den Film zurück
        DatenProg prog = this.getProgUrl(url);
        String ret = arr[PROGRAMMSET_ZIEL_DATEINAME_NR];
        if (!checkDownloadDirekt(url) && prog != null) {
            // nur wenn kein direkter Download und ein passendes Programm
            if (!prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].equals("")) {
                ret = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR];
            }
        }
        return ret;
    }

    public String getZielPfad() {
        //gibt den Zielpfad für den Film zurück
        return arr[PROGRAMMSET_ZIEL_PFAD_NR];
    }

    public DatenPset copy() {
        DatenPset ret = new DatenPset();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        //es darf nur einen geben!
        ret.arr[PROGRAMMSET_NAME_NR] = "Kopie-" + arr[PROGRAMMSET_NAME_NR];
        ret.arr[PROGRAMMSET_IST_ABSPIELEN_NR] = Boolean.toString(false);
        ListIterator<DatenProg> it = getListeProg().listIterator(0);
        while (it.hasNext()) {
            ret.addProg(it.next().copy());
        }
        return ret;
    }

    public Color getFarbe() {
        Color ret = null;
        String r, g, b;
        if (!arr[PROGRAMMSET_FARBE_NR].equals("")) {
            r = arr[PROGRAMMSET_FARBE_NR].substring(0, arr[PROGRAMMSET_FARBE_NR].indexOf(','));
            g = arr[PROGRAMMSET_FARBE_NR].substring(arr[PROGRAMMSET_FARBE_NR].indexOf(',') + 1,
                    arr[PROGRAMMSET_FARBE_NR].lastIndexOf(','));
            b = arr[PROGRAMMSET_FARBE_NR].substring(arr[PROGRAMMSET_FARBE_NR].lastIndexOf(',') + 1);
            try {
                ret = new Color(Integer.parseInt(r), Integer.parseInt(g), Integer.parseInt(b));
            } catch (Exception ex) {
                Log.fehlerMeldung(669254033, this.getClass().getName() + ".getFarbe", ex);
            }
        }
        return ret;
    }

    public void setFarbe(Color farbe) {
        arr[PROGRAMMSET_FARBE_NR]
                = farbe.getRed() + "," + farbe.getGreen() + "," + farbe.getBlue();
    }

    public boolean checkDownloadDirekt(String url) {
        //auf direkte prüfen, pref oder suf: wenn angegeben dann muss es stimmen
        if (!this.arr[PROGRAMMSET_PRAEFIX_DIREKT_NR].equals("")
                || !this.arr[PROGRAMMSET_SUFFIX_DIREKT_NR].equals("")) {
            if (GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMMSET_PRAEFIX_DIREKT_NR], url, true)
                    && GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMMSET_SUFFIX_DIREKT_NR], url, false)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        String ret = "";
        ret += "================================================" + Daten.LINE_SEPARATOR;
        ret += "| Programmset" + Daten.LINE_SEPARATOR;
        for (int i = 0; i < MAX_ELEM; ++i) {
            ret += "| " + COLUMN_NAMES[i] + ": " + arr[i] + Daten.LINE_SEPARATOR;
        }
        for (Object aListeProg : listeProg) {
            ret += "|" + Daten.LINE_SEPARATOR;
            ret += aListeProg.toString();
        }
        ret += "|_______________________________________________" + Daten.LINE_SEPARATOR;
        return ret;
    }
    //===================================
    // Private
    //===================================

    private void makeArray() {
        arr = new String[MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
        arr[PROGRAMMSET_IST_ABSPIELEN_NR] = Boolean.toString(false);
        arr[PROGRAMMSET_IST_SPEICHERN_NR] = Boolean.toString(false);
        arr[PROGRAMMSET_IST_BUTTON_NR] = Boolean.toString(false);
        arr[PROGRAMMSET_IST_ABO_NR] = Boolean.toString(false);
        arr[PROGRAMMSET_THEMA_ANLEGEN_NR] = Boolean.toString(true);
        arr[PROGRAMMSET_INFODATEI_NR] = Boolean.toString(false);
        arr[PROGRAMMSET_SPOTLIGHT_NR] = Boolean.toString(SystemInfo.isMacOSX());
    }
}
