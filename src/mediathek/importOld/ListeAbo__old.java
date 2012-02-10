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

import java.util.Collections;
import java.util.LinkedList;

public class ListeAbo__old extends LinkedList<DatenAbo__old> {

    private int nr = 0;

    public void addAbo(DatenAbo__old datenAbo) {
        String str = String.valueOf(nr++);
        while (str.length() < 3) {
            str = "0" + str;
        }
        datenAbo.arr[Konstanten__old.ABO_NR_NR] = str;
        //für die neue Funktion
        if (datenAbo.arr[Konstanten__old.ABO_THEMA_EXAKT_NR].equals("")) {
            datenAbo.arr[Konstanten__old.ABO_THEMA_EXAKT_NR] = Boolean.toString(true);
        }
        super.add(datenAbo);
    }

    public void sort() {
        Collections.<DatenAbo__old>sort(this);
    }
}
