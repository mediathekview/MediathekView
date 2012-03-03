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

import java.awt.Color;
import java.util.Iterator;
import java.util.ListIterator;
import javax.swing.JOptionPane;
import mediathek.Log;
import mediathek.controller.io.starter.Starts;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiFunktionenProgramme;

public class DatenPgruppe {

    //Tags Programmgruppen
    public static final String PROGRAMMGRUPPE = "Programmgruppe";
    public static final int PROGRAMMGRUPPE_MAX_ELEM = 12;
    public static final String PROGRAMMGRUPPE_NAME = "Name";
    public static final int PROGRAMMGRUPPE_NAME_NR = 0;
    public static final String PROGRAMMGRUPPE_PRAEFIX_DIREKT = "Praefix";
    public static final int PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR = 1;
    public static final String PROGRAMMGRUPPE_SUFFIX_DIREKT = "Suffix";
    public static final int PROGRAMMGRUPPE_SUFFIX_DIREKT_NR = 2;
    public static final String PROGRAMMGRUPPE_FARBE = "Farbe";
    public static final int PROGRAMMGRUPPE_FARBE_NR = 3;
    public static final String PROGRAMMGRUPPE_ZIEL_PFAD = "Zielpfad";
    public static final int PROGRAMMGRUPPE_ZIEL_PFAD_NR = 4;
    public static final String PROGRAMMGRUPPE_ZIEL_DATEINAME = "Zieldateiname";
    public static final int PROGRAMMGRUPPE_ZIEL_DATEINAME_NR = 5;
    public static final String PROGRAMMGRUPPE_IST_ABSPIELEN = "Abspielen";
    public static final int PROGRAMMGRUPPE_IST_ABSPIELEN_NR = 6;
    public static final String PROGRAMMGRUPPE_IST_SPEICHERN = "Speichern";
    public static final int PROGRAMMGRUPPE_IST_SPEICHERN_NR = 7;
    public static final String PROGRAMMGRUPPE_IST_BUTTON = "Button";
    public static final int PROGRAMMGRUPPE_IST_BUTTON_NR = 8;
    public static final String PROGRAMMGRUPPE_IST_ABO = "Abo";
    public static final int PROGRAMMGRUPPE_IST_ABO_NR = 9;
    public static final String PROGRAMMGRUPPE_LAENGE_BESCHRAENKEN = "Laenge";
    public static final int PROGRAMMGRUPPE_LAENGE_BESCHRAENKEN_NR = 10;
    public static final String PROGRAMMGRUPPE_MAX_LAENGE = "max-Laenge";
    public static final int PROGRAMMGRUPPE_MAX_LAENGE_NR = 11;
    public static final String[] PROGRAMMGRUPPE_COLUMN_NAMES = {PROGRAMMGRUPPE_NAME, PROGRAMMGRUPPE_PRAEFIX_DIREKT, PROGRAMMGRUPPE_SUFFIX_DIREKT,
        PROGRAMMGRUPPE_FARBE, PROGRAMMGRUPPE_ZIEL_PFAD, PROGRAMMGRUPPE_ZIEL_DATEINAME,
        PROGRAMMGRUPPE_IST_ABSPIELEN, PROGRAMMGRUPPE_IST_SPEICHERN, PROGRAMMGRUPPE_IST_BUTTON, PROGRAMMGRUPPE_IST_ABO,
        PROGRAMMGRUPPE_LAENGE_BESCHRAENKEN, PROGRAMMGRUPPE_MAX_LAENGE};
    public String[] arr;
    private ListeProg listeProg = new ListeProg();

    public DatenPgruppe() {
        makeArray();
    }

    public DatenPgruppe(String name) {
        makeArray();
        arr[PROGRAMMGRUPPE_NAME_NR] = name;
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

    public boolean needsPath() {
        // Zielpfad muss angegeben werden wenn:
        // er nicht mit "%p" abgefragt wird
        // und beim Programmschalter mit "**" eingesetzt werden soll
        boolean ret = false;
        if (!this.arr[PROGRAMMGRUPPE_ZIEL_DATEINAME_NR].contains("%p")) {
            Iterator<DatenProg> it = listeProg.iterator();
            DatenProg prog;
            while (it.hasNext()) {
                prog = it.next();
                if (prog.arr[DatenProg.PROGRAMM_SCHALTER_NR].contains("**")) {
                    ret = true;
                    break;
                }
            }
        }
        return ret;
    }

    public boolean istAbspielen() {
        return Boolean.parseBoolean(arr[PROGRAMMGRUPPE_IST_ABSPIELEN_NR]);
    }

    public boolean istSpeichern() {
        return Boolean.parseBoolean(arr[PROGRAMMGRUPPE_IST_SPEICHERN_NR]);
    }

    public boolean istButton() {
        return Boolean.parseBoolean(arr[PROGRAMMGRUPPE_IST_BUTTON_NR]);
    }

    public boolean istAbo() {
        return Boolean.parseBoolean(arr[PROGRAMMGRUPPE_IST_ABO_NR]);
    }

    public boolean isLable() {
        // wenn die Programmliste leer ist und einen Namen hat, ist es ein Lable
        if (this.listeProg.isEmpty()) {
            if (!this.arr[PROGRAMMGRUPPE_NAME_NR].equals("")) {
                return true;
            }
        }
        return false;
    }

    public boolean isFreeLine() {
        //Wenn die Programmgruppe keinen Namen hat, leere Zeile
        if (this.arr[PROGRAMMGRUPPE_NAME_NR].equals("")) {
            return true;
        } else {
            return false;
        }
    }

    public void setAbspielen(DDaten ddaten) {
        Iterator<DatenPgruppe> it = ddaten.listePgruppe.iterator();
        while (it.hasNext()) {
            DatenPgruppe datenPgruppe = it.next();
            datenPgruppe.arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABSPIELEN_NR] = Boolean.FALSE.toString();
        }
        arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABSPIELEN_NR] = Boolean.TRUE.toString();
    }

    public void setSpeichern(boolean set) {
        arr[DatenPgruppe.PROGRAMMGRUPPE_IST_SPEICHERN_NR] = Boolean.toString(set);
    }

    public void setButton(boolean set) {
        arr[DatenPgruppe.PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(set);
    }

    public void setAbo(boolean set) {
        arr[DatenPgruppe.PROGRAMMGRUPPE_IST_ABO_NR] = Boolean.toString(set);
    }

    public DatenProg getProgUrl(String url) {
        //mit einer Url das Passende Programm finden
        //passt nichts, wird das letzte Programm genommen
        //ist nur ein Programm in der Liste wird dieses genommen
        DatenProg ret = null;
        if (listeProg.size() == 0) {
            JOptionPane.showMessageDialog(null, "Programme einrichten!",
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
            if (listeProg.size() > 0 && ret == null) {
                ret = listeProg.getLast();
            }
        }
        return ret;
    }

    public String getZielDateiname(String url) {
        //gibt den Zieldateinamen für den Film zurück
        DatenProg prog = this.getProgUrl(url);
        String ret = arr[PROGRAMMGRUPPE_ZIEL_DATEINAME_NR];
        if ((checkDownloadDirekt(url) != Starts.ART_DOWNLOAD) && prog != null) {
            // nur wenn kein direkter Download und ein passendes Programm
            if (!prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR].equals("")) {
                ret = prog.arr[DatenProg.PROGRAMM_ZIEL_DATEINAME_NR];
            }
        }
        return ret;
    }

    public String getZielPfad() {
        //gibt den Zielpfad für den Film zurück
        return arr[PROGRAMMGRUPPE_ZIEL_PFAD_NR];
    }

    public DatenPgruppe copy() {
        DatenPgruppe ret = new DatenPgruppe();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        //es darf nur einen geben!
        ret.arr[PROGRAMMGRUPPE_NAME_NR] = "Kopie-" + arr[PROGRAMMGRUPPE_NAME_NR];
        ret.arr[PROGRAMMGRUPPE_IST_ABSPIELEN_NR] = Boolean.toString(false);
        ListIterator<DatenProg> it = getListeProg().listIterator(0);
        while (it.hasNext()) {
            ret.addProg(it.next().copy());
        }
        return ret;
    }

    public Color getFarbe(DDaten daten) {
        Color ret = null;
        String r, g, b;
        if (!arr[PROGRAMMGRUPPE_FARBE_NR].equals("")) {
            r = arr[PROGRAMMGRUPPE_FARBE_NR].substring(0, arr[PROGRAMMGRUPPE_FARBE_NR].indexOf(","));
            g = arr[PROGRAMMGRUPPE_FARBE_NR].substring(arr[PROGRAMMGRUPPE_FARBE_NR].indexOf(",") + 1,
                    arr[PROGRAMMGRUPPE_FARBE_NR].lastIndexOf(","));
            b = arr[PROGRAMMGRUPPE_FARBE_NR].substring(arr[PROGRAMMGRUPPE_FARBE_NR].lastIndexOf(",") + 1);
            try {
                ret = new Color(Integer.parseInt(r), Integer.parseInt(g), Integer.parseInt(b));
            } catch (Exception ex) {
                Log.fehlerMeldung(this.getClass().getName() + ".getFarbe", ex);
            }
        }
        return ret;
    }

    public void setFarbe(Color farbe) {
        arr[PROGRAMMGRUPPE_FARBE_NR] =
                String.valueOf(farbe.getRed()) + "," + String.valueOf(farbe.getGreen()) + "," + String.valueOf(farbe.getBlue());
    }

    public int checkDownloadDirekt(String url) {
        int ret = Starts.ART_PROGRAMM;
        //auf direkte prüfen, pref oder suf: wenn angegeben dann muss es stimmen
        if (!this.arr[PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR].equals("")
                || !this.arr[PROGRAMMGRUPPE_SUFFIX_DIREKT_NR].equals("")) {
            if (GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR], url, true)
                    && GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMMGRUPPE_SUFFIX_DIREKT_NR], url, false)) {
                ret = Starts.ART_DOWNLOAD;
            }
        }
        return ret;
    }

    //===================================
    // Private
    //===================================
    private void makeArray() {
        arr = new String[PROGRAMMGRUPPE_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
        arr[PROGRAMMGRUPPE_IST_ABSPIELEN_NR] = Boolean.toString(false);
        arr[PROGRAMMGRUPPE_IST_SPEICHERN_NR] = Boolean.toString(false);
        arr[PROGRAMMGRUPPE_IST_BUTTON_NR] = Boolean.toString(false);
        arr[PROGRAMMGRUPPE_IST_ABO_NR] = Boolean.toString(false);
    }
}
