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

import mediathek.daten.DatenProg;

public class DatenProg__old {

    public String[] arr;

    public DatenProg__old() {
        makeArr();
    }

    public DatenProg__old(String name, String programmpfad, String schalter, String restart) {
        makeArr();
        arr[Konstanten__old.PROGRAMM_NAME_NR] = name;
        arr[Konstanten__old.PROGRAMM_PROGRAMMPFAD_NR] = programmpfad;
        arr[Konstanten__old.PROGRAMM_SCHALTER_NR] = schalter;
        arr[Konstanten__old.PROGRAMM_RESTART_NR] = restart;
    }

    public DatenProg__old(String name, String programmpfad, String schalter, String restart, String farbe) {
        makeArr();
        arr[Konstanten__old.PROGRAMM_NAME_NR] = name;
        arr[Konstanten__old.PROGRAMM_PROGRAMMPFAD_NR] = programmpfad;
        arr[Konstanten__old.PROGRAMM_SCHALTER_NR] = schalter;
        arr[Konstanten__old.PROGRAMM_RESTART_NR] = restart;
    }

    public DatenProg getNewVersion() {
        DatenProg progNeu = new DatenProg();
        progNeu.arr[DatenProg.PROGRAMM_NAME_NR] = this.arr[Konstanten__old.PROGRAMM_NAME_NR];
        progNeu.arr[DatenProg.PROGRAMM_PROGRAMMPFAD_NR] = this.arr[Konstanten__old.PROGRAMM_PROGRAMMPFAD_NR];
        progNeu.arr[DatenProg.PROGRAMM_SCHALTER_NR] = this.arr[Konstanten__old.PROGRAMM_SCHALTER_NR];
        progNeu.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = this.arr[Konstanten__old.PROGRAMM_PRAEFIX_NR];
        progNeu.arr[DatenProg.PROGRAMM_SUFFIX_NR] = this.arr[Konstanten__old.PROGRAMM_SUFFIX_NR];
        progNeu.arr[DatenProg.PROGRAMM_RESTART_NR] = this.arr[Konstanten__old.PROGRAMM_RESTART_NR];
        // anpassen wo möglich
        if (progNeu.arr[DatenProg.PROGRAMM_PRAEFIX_NR].equals("-")) {
            progNeu.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = "rtmp";
            progNeu.arr[DatenProg.PROGRAMM_SCHALTER_NR] = progNeu.arr[DatenProg.PROGRAMM_SCHALTER_NR].replace("%f", "%F");
        }
        if (progNeu.arr[DatenProg.PROGRAMM_PRAEFIX_NR].startsWith("-r rtmp")) {
            progNeu.arr[DatenProg.PROGRAMM_PRAEFIX_NR] = progNeu.arr[DatenProg.PROGRAMM_PRAEFIX_NR].replaceFirst("-r rtmp", "rtmp");
            progNeu.arr[DatenProg.PROGRAMM_SCHALTER_NR] = progNeu.arr[DatenProg.PROGRAMM_SCHALTER_NR].replace("%f", "%F");
        }
        return progNeu;
    }

    public DatenProg__old copy() {
        DatenProg__old ret = new DatenProg__old();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        return ret;
    }

    //===================================
    // Private
    //===================================
    private void makeArr() {
        arr = new String[Konstanten__old.PROGRAMM_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = new String("");
        }
    }
}
