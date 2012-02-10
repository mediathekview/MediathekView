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

import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import javax.swing.JOptionPane;

public class ListePgruppe__old extends LinkedList<DatenPgruppe__old> {

    public ListePgruppe__old getCopyOfMe() {
        //Kopie dieser Liste zurückgeben
        ListePgruppe__old ret = new ListePgruppe__old();
        Iterator<DatenPgruppe__old> it = this.iterator();
        while (it.hasNext()) {
            ret.add(it.next().copy());
        }
        return ret;
    }

    public boolean nameExists(String name) {
        boolean ret = false;
        ListIterator<DatenPgruppe__old> it = this.listIterator(0);
        while (it.hasNext()) {
            if (it.next().arr[Konstanten__old.PROGRAMMGRUPPE_NAME_NR].equals(name)) {
                ret = true;
            }
        }
        return ret;
    }

    public void setDoppelklick(int nr) {
        //Doppelklick für Programmgruppe (Button) mit der Nr setzen und die anderen löschen
        ListIterator<DatenPgruppe__old> it = this.listIterator(0);
        while (it.hasNext()) {
            it.next().arr[Konstanten__old.PROGRAMMGRUPPE_DOPPELKLICK_NR] = Boolean.toString(false);
        }
        this.get(nr).arr[Konstanten__old.PROGRAMMGRUPPE_DOPPELKLICK_NR] = Boolean.toString(true);
    }

    public int getDoppelklick() {
        //Doppelklick (Button) mit der Nr suchen oder -1 zurückgeben
        int ret = -1;
        for (int i = 0; i < this.size(); ++i) {
            if (this.get(i).arr[Konstanten__old.PROGRAMMGRUPPE_DOPPELKLICK_NR].equals(Boolean.toString(true))) {
                ret = i;
            }
        }
        return ret;
    }

    public DatenPgruppe__old getName(String name) {
        //liefert mit dem Namen eine Programmgruppe zurück
        //wird nichts gefunden, wird die erste Programmgruppe genommen
        DatenPgruppe__old ret = null;
        if (this.size() == 0) {
            JOptionPane.showMessageDialog(null, "Programme einrichten!",
                                          "Keine Programmgruppe", JOptionPane.INFORMATION_MESSAGE);
        } else if (this.size() == 1) {
            ret = this.getFirst();
        } else {
            ListIterator<DatenPgruppe__old> it = this.listIterator(0);
            while (it.hasNext()) {
                DatenPgruppe__old gruppe;
                gruppe = it.next();
                if (gruppe.arr[Konstanten__old.PROGRAMMGRUPPE_NAME_NR].equals(name)) {
                    ret = gruppe;
                }
            }
            if (ret == null) {
                ret = this.getFirst();
            }
        }
        return ret;
    }

    public void duplicate(DatenPgruppe__old gruppe) {
        this.add(gruppe.copy());
    }


}
