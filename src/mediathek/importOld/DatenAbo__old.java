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

import mediathek.daten.DatenAbo;

public class DatenAbo__old implements Comparable<DatenAbo__old> {

    public String[] arr;

    public DatenAbo__old() {
        makeArr();
    }

    public DatenAbo__old(String name, String sender, String thema, boolean exakt, String titel, String ziel, String programmname) {
        makeArr();
        arr[Konstanten__old.ABO_NAME_NR] = name;
        arr[Konstanten__old.ABO_SENDER_NR] = sender;
        arr[Konstanten__old.ABO_THEMA_NR] = thema;
        arr[Konstanten__old.ABO_THEMA_EXAKT_NR] = Boolean.toString(exakt);
        arr[Konstanten__old.ABO_TITEL_NR] = titel;
        arr[Konstanten__old.ABO_ZIELPFAD_NR] = ziel;
        arr[Konstanten__old.ABO_PGRUPPE_NR] = programmname;
    }

    public DatenAbo getNewVersion() {
        DatenAbo abo = new DatenAbo();
        abo.arr[DatenAbo.ABO_NAME_NR] = this.arr[Konstanten__old.ABO_NAME_NR];
        abo.arr[DatenAbo.ABO_SENDER_NR] = this.arr[Konstanten__old.ABO_SENDER_NR];
        abo.arr[DatenAbo.ABO_THEMA_NR] = this.arr[Konstanten__old.ABO_THEMA_NR];
        abo.arr[DatenAbo.ABO_TITEL_NR] = this.arr[Konstanten__old.ABO_TITEL_NR];
        abo.arr[DatenAbo.ABO_ZIELPFAD_NR] = this.arr[Konstanten__old.ABO_ZIELPFAD_NR];
        abo.arr[DatenAbo.ABO_DOWN_DATUM_NR] = this.arr[Konstanten__old.ABO_DOWN_DATUM_NR];
        abo.arr[DatenAbo.ABO_PSET_NR] = this.arr[Konstanten__old.ABO_PGRUPPE_NR];
        return abo;
    }

    public DatenAbo__old getCopy() {
        DatenAbo__old ret = new DatenAbo__old();
        for (int i = 0; i < arr.length; ++i) {
            ret.arr[i] = new String(this.arr[i]);
        }
        return ret;
    }

    public boolean binEinmal() {
        // true wenn EinmalAbo
        if (arr[Konstanten__old.ABO_EINMAL_URL_NR].equals("")) {
            return false;
        } else {
            return true;
        }
    }

    public void aufMichKopieren(DatenAbo__old datenAbo) {
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = new String(datenAbo.arr[i]);
        }
    }

    private void makeArr() {
        arr = new String[Konstanten__old.ABO_MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = new String("");
        }
    }

    @Override
    public int compareTo(DatenAbo__old arg0) {
        return (arr[Konstanten__old.ABO_THEMA_NR].compareToIgnoreCase(((DatenAbo__old) arg0).arr[Konstanten__old.ABO_THEMA_NR]));
    }
}
