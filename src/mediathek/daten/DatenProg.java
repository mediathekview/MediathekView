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
package mediathek.daten;

import static mediathek.controller.starter.RuntimeExec.TRENNER_PROG_ARRAY;
import mediathek.tool.GuiFunktionenProgramme;

public class DatenProg extends Data<DatenProg> {

    public static final int PROGRAMM_NAME = 0;
    public static final int PROGRAMM_ZIEL_DATEINAME = 1;
    public static final int PROGRAMM_PROGRAMMPFAD = 2;
    public static final int PROGRAMM_SCHALTER = 3;
    public static final int PROGRAMM_PRAEFIX = 4;
    public static final int PROGRAMM_SUFFIX = 5;
    public static final int PROGRAMM_RESTART = 6;
    public static final int PROGRAMM_DOWNLOADMANAGER = 7;

    public static final int MAX_ELEM = 8;
    public static final String TAG = "Programm";
    public static final String[] COLUMN_NAMES = {"Beschreibung", "Zieldateiname", "Programm",
        "Schalter", "Präfix", "Suffix", "Restart", "Downloadmanager"};
    public static final String[] XML_NAMES = {"Programmname", "Zieldateiname", "Programmpfad",
        "Programmschalter", "Praefix", "Suffix", "Restart", "Downloadmanager"};

    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];
    public String[] arr;

    public DatenProg() {
        makeArr();
        arr[PROGRAMM_RESTART] = Boolean.toString(false);
        arr[PROGRAMM_DOWNLOADMANAGER] = Boolean.toString(false);
    }

    public DatenProg(String name, String programmpfad, String schalter, String restart, String downloadmanager) {
        makeArr();
        arr[PROGRAMM_NAME] = name;
        arr[PROGRAMM_PROGRAMMPFAD] = programmpfad;
        arr[PROGRAMM_SCHALTER] = schalter;
        arr[PROGRAMM_RESTART] = restart.equals("") ? Boolean.toString(false) : restart;
        arr[PROGRAMM_DOWNLOADMANAGER] = downloadmanager.equals("") ? Boolean.toString(false) : downloadmanager;
    }

    public DatenProg copy() {
        DatenProg ret = new DatenProg();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        return ret;
    }

    public boolean isRestart() {
        if (arr[PROGRAMM_RESTART].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[PROGRAMM_RESTART]);
    }

    public boolean isDownloadManager() {
        if (arr[PROGRAMM_DOWNLOADMANAGER].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[PROGRAMM_DOWNLOADMANAGER]);
    }

    public boolean urlTesten(String url) {
        //prüfen ob das Programm zur Url passt
        boolean ret = false;
        if (url != null) {
            //Felder sind entweder leer oder passen
            if (GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMM_PRAEFIX], url, true)
                    && GuiFunktionenProgramme.praefixTesten(this.arr[PROGRAMM_SUFFIX], url, false)) {
                ret = true;
            }
        }
        return ret;
    }

    public String getProgrammAufruf() {
        return arr[PROGRAMM_PROGRAMMPFAD] + " " + arr[PROGRAMM_SCHALTER];
    }

    public String getProgrammAufrufArray() {
        String ret;
        ret = arr[DatenProg.PROGRAMM_PROGRAMMPFAD];
        String[] ar = arr[DatenProg.PROGRAMM_SCHALTER].split(" ");
        for (String s : ar) {
            ret = ret + TRENNER_PROG_ARRAY + s;
        }
        return ret;
    }

    public static String makeProgAufrufArray(String pArray) {
        String[] progArray = pArray.split(TRENNER_PROG_ARRAY);
        String execStr = "";
        for (String s : progArray) {
            execStr = execStr + s + " ";
        }
        execStr = execStr.trim(); // letztes Leerzeichen wieder entfernen
        return execStr;
    }

    @Override
    public String toString() {
        String ret = "";
        for (int i = 0; i < MAX_ELEM; ++i) {
            if (i == 0) {
                ret += "| ***|" + COLUMN_NAMES[i] + ": " + arr[i] + Daten.LINE_SEPARATOR;
            } else {
                ret += "|    |" + COLUMN_NAMES[i] + ": " + arr[i] + Daten.LINE_SEPARATOR;
            }
        }
        return ret;
    }

    //===================================
    // Private
    //===================================
    private void makeArr() {
        arr = new String[MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
