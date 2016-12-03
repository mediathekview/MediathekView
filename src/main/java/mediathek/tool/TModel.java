/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import javax.swing.table.DefaultTableModel;
import java.util.List;

@SuppressWarnings("serial")
public class TModel extends DefaultTableModel {
    public TModel() {
    }

    public TModel(Object[][] data, Object[] columnNames) {
        super(data, columnNames);
    }

    @Override
    public boolean isCellEditable(int i, int j) {
        return false;
    }

    @SuppressWarnings("unchecked")
    public int getIdxRow(int idxWert) {
        // liefert die Zeile in der die erste Spalte idx enthält
        // die Indexspalte ist die SPALTE 0!!!!
        int ret = 0;
        for (List<Integer> list : (Iterable<List<Integer>>) getDataVector()) {
            if (list.get(0) == idxWert) {
                return ret;
            }
            ++ret;
        }
        return -1;
    }
}
