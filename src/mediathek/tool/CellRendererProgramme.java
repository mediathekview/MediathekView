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
package mediathek.tool;

import java.awt.Component;
import javax.swing.JTable;
import javax.swing.table.DefaultTableCellRenderer;
import mSearch.tool.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenProg;
import mediathek.res.GetIcon;

public class CellRendererProgramme extends DefaultTableCellRenderer {

    Daten daten;

    public CellRendererProgramme(Daten d) {
        daten = d;
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        setIcon(null);
        super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        try {
            int c = table.convertColumnIndexToModel(column);
            if (c == DatenProg.PROGRAMM_RESTART_NR || c == DatenProg.PROGRAMM_DOWNLOADMANAGER_NR) {
            	setHorizontalAlignment(CENTER);
                if (getText().equals(Boolean.TRUE.toString())) {
                    setIcon(GetIcon.getProgramIcon("ja_16.png"));
                } else {
                    setIcon(GetIcon.getProgramIcon("nein_12.png"));
                }
                setText("");
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(338740095, ex);
        }
        return this;
    }
}
