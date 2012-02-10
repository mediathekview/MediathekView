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

import java.util.ListIterator;
import mediathek.daten.DatenPgruppe;

public class DatenPgruppe__old {

    public String[] arr;
    private ListeProg__old listeProg = new ListeProg__old();

    public DatenPgruppe__old() {
        makeArray();
    }

    public DatenPgruppe__old(String name, String suffix, String farbe, String zielPfad, String zielDateiname) {
        makeArray();
        arr[Konstanten__old.PROGRAMMGRUPPE_NAME_NR] = name;
        arr[Konstanten__old.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = suffix;
        arr[Konstanten__old.PROGRAMMGRUPPE_FARBE_NR] = farbe;
        arr[Konstanten__old.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = zielPfad;
        arr[Konstanten__old.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = zielDateiname;
    }

    public DatenPgruppe getNewVersion() {
        DatenPgruppe gruppeNeu = new DatenPgruppe();
        gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_NAME_NR] = this.arr[Konstanten__old.PROGRAMMGRUPPE_NAME_NR];
        gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_PRAEFIX_DIREKT_NR] = this.arr[Konstanten__old.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR];
        gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR] = this.arr[Konstanten__old.PROGRAMMGRUPPE_SUFFIX_DIREKT_NR];
        gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_FARBE_NR] = this.arr[Konstanten__old.PROGRAMMGRUPPE_FARBE_NR];
        gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_PFAD_NR] = this.arr[Konstanten__old.PROGRAMMGRUPPE_ZIEL_PFAD_NR];
        gruppeNeu.arr[DatenPgruppe.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR] = this.arr[Konstanten__old.PROGRAMMGRUPPE_ZIEL_DATEINAME_NR];
        return gruppeNeu;
    }

    public boolean addProg(DatenProg__old prog) {
        return listeProg.add(prog);
    }

    public ListeProg__old getListeProg() {
        return listeProg;
    }

    public DatenProg__old getProg(int i) {
        return listeProg.get(i);
    }

    public DatenPgruppe__old copy() {
        DatenPgruppe__old ret = new DatenPgruppe__old();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        //es darf nur einen geben!
        arr[Konstanten__old.PROGRAMMGRUPPE_DOPPELKLICK_NR] = Boolean.toString(false);
        ListIterator<DatenProg__old> it = getListeProg().listIterator(0);
        while (it.hasNext()) {
            ret.addProg(it.next().copy());
        }
        return ret;
    }

    //===================================
    // Private
    //===================================
    private void makeArray() {
        arr = new String[Konstanten__old.PROGRAMMGRUPPE_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }
}
