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

import mediathek.tool.GuiFunktionen;

public class DatenProg {

    public static final String PROGRAMM = "Programm";
    public static final int PROGRAMM_MAX_ELEM = 7;
    public static final String PROGRAMM_NAME = "Programmname";
    public static final int PROGRAMM_NAME_NR = 0;
    public static final String PROGRAMM_ZIEL_DATEINAME = "Zieldateiname";
    public static final int PROGRAMM_ZIEL_DATEINAME_NR = 1;
    public static final String PROGRAMM_PROGRAMMPFAD = "Programmpfad";
    public static final int PROGRAMM_PROGRAMMPFAD_NR = 2;
    public static final String PROGRAMM_SCHALTER = "Programmschalter";
    public static final int PROGRAMM_SCHALTER_NR = 3;
    public static final String PROGRAMM_PRAEFIX = "Praefix";
    public static final int PROGRAMM_PRAEFIX_NR = 4;
    public static final String PROGRAMM_SUFFIX = "Suffix";
    public static final int PROGRAMM_SUFFIX_NR = 5;
    public static final String PROGRAMM_RESTART = "Restart";
    public static final int PROGRAMM_RESTART_NR = 6;
    public static final String[] PROGRAMM_COLUMN_NAMES = {PROGRAMM_NAME, PROGRAMM_ZIEL_DATEINAME, PROGRAMM_PROGRAMMPFAD,
        PROGRAMM_SCHALTER, PROGRAMM_PRAEFIX, PROGRAMM_SUFFIX, PROGRAMM_RESTART};
    public String[] arr;

    public DatenProg() {
        makeArr();
        arr[PROGRAMM_RESTART_NR] = Boolean.toString(false);
    }

    public DatenProg(String name, String programmpfad, String schalter, String restart) {
        makeArr();
        arr[PROGRAMM_NAME_NR] = name;
        arr[PROGRAMM_PROGRAMMPFAD_NR] = programmpfad;
        arr[PROGRAMM_SCHALTER_NR] = schalter;
        arr[PROGRAMM_RESTART_NR] = restart.equals("") ? Boolean.toString(false) : restart;
    }

    public DatenProg(String name, String programmpfad, String schalter, String restart, String farbe) {
        makeArr();
        arr[PROGRAMM_NAME_NR] = name;
        arr[PROGRAMM_PROGRAMMPFAD_NR] = programmpfad;
        arr[PROGRAMM_SCHALTER_NR] = schalter;
        arr[PROGRAMM_RESTART_NR] = restart.equals("") ? Boolean.toString(false) : restart;
    }

    public DatenProg copy() {
        DatenProg ret = new DatenProg();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        return ret;
    }

    public boolean isRestart() {
        if (arr[PROGRAMM_RESTART_NR].equals("")) {
            return false;
        }
        return Boolean.parseBoolean(arr[PROGRAMM_RESTART_NR]);
    }

    public boolean urlTesten(String url) {
        //prüfen ob das Programm zur Url passt
        boolean ret = false;
        if (url != null) {
            //Felder sind entweder leer oder passen
            if (GuiFunktionen.praefixTesten(this.arr[PROGRAMM_PRAEFIX_NR], url, true)
                    && GuiFunktionen.praefixTesten(this.arr[PROGRAMM_SUFFIX_NR], url, false)) {
                ret = true;
            }
        }
        return ret;
    }

    public String getProgrammAufruf() {
        return arr[PROGRAMM_PROGRAMMPFAD_NR] + " " + arr[PROGRAMM_SCHALTER_NR];
    }

    //===================================
    // Private
    //===================================
    private void makeArr() {
        arr = new String[PROGRAMM_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
