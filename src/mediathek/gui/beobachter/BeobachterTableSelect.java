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

package mediathek.gui.beobachter;

import javax.swing.JTable;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.Log;
import mediathek.daten.DDaten;

public class BeobachterTableSelect implements ListSelectionListener {

    public int selectedRow = -1;
    public boolean stop = false;
    private JTable jTable;
    private DDaten daten;

    public BeobachterTableSelect(DDaten ddaten, JTable t) {
        jTable = t;
        daten = ddaten;
    }

    @Override
    public void valueChanged(ListSelectionEvent event) {
        if (!event.getValueIsAdjusting()) {
            if (!stop) {
                selectedRow = -1;
                int selectedTableRow;
                try {
                    selectedTableRow = jTable.getSelectedRow();
                    if (selectedTableRow >= 0) {
                        selectedRow = jTable.convertRowIndexToModel(selectedTableRow);
                    }
                } catch (Exception ex) {
                    Log.fehlerMeldung(this.getClass().getName(), ex);
                }
                fill();
            }
        }

    }

    public void fill() {
    }

}

