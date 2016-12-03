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

import mediathek.tool.TModel;

import java.util.Iterator;
import java.util.LinkedList;

@SuppressWarnings("serial")
public class ListeProg extends LinkedList<DatenProg> {
    public DatenProg remove(String name) {
        DatenProg ret = null;
        Iterator<DatenProg> it = this.iterator();
        DatenProg prog;
        while (it.hasNext()) {
            prog = it.next();
            if (prog.arr[DatenProg.PROGRAMM_NAME].equals(name)) {
                it.remove();
                ret = prog;
                break;
            }
        }
        return ret;
    }

    public int auf(int idx, boolean auf) {
        DatenProg prog = this.remove(idx);
        int neu = idx;
        if (auf) {
            if (neu > 0) {
                --neu;
            }
        } else if (neu < this.size()) {
            ++neu;
        }
        this.add(neu, prog);
        return neu;
    }

    public TModel getModel() {
        TModel model;
        Object[][] object;
        DatenProg daten;
        int i = 0;
        if (this.size() > 0) {
            Iterator<DatenProg> iterator = this.iterator();
            object = new Object[this.size()][DatenProg.MAX_ELEM];
            while (iterator.hasNext()) {
                daten = iterator.next();
                object[i] = daten.arr;
                ++i;
            }
            model = new TModel(object, DatenProg.COLUMN_NAMES);
        } else {
            model = new TModel(new Object[0][DatenProg.MAX_ELEM], DatenProg.COLUMN_NAMES);
        }
        return model;
    }

}
