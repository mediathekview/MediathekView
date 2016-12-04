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

import mSearch.tool.Log;
import mediathek.config.Icons;
import mediathek.daten.DatenPset;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

@SuppressWarnings("serial")
public class CellRendererPset extends DefaultTableCellRenderer {
    private static final ImageIcon ja_16 = Icons.ICON_TABELLE_EIN;
    private static final ImageIcon nein_12 = Icons.ICON_TABELLE_AUS;

    public CellRendererPset() {
    }

    @Override
    public Component getTableCellRendererComponent(
            JTable table,
            Object value,
            boolean isSelected,
            boolean hasFocus,
            int row,
            int column) {
        setBackground(null);
        setForeground(null);
        setFont(null);
        setIcon(null);
        setHorizontalAlignment(SwingConstants.LEADING);
        super.getTableCellRendererComponent(
                table, value, isSelected, hasFocus, row, column);
        try {
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            DatenPset datenPset = new DatenPset();
            for (int i = 0; i < DatenPset.MAX_ELEM; ++i) {
                datenPset.arr[i] = table.getModel().getValueAt(r, i).toString();
            }
            if (c == DatenPset.PROGRAMMSET_NAME) {
                setForeground(datenPset.getFarbe());
            }
            if (c == DatenPset.PROGRAMMSET_IST_ABSPIELEN) {
                setHorizontalAlignment(SwingConstants.CENTER);
                setText(""); // nur das Icon anzeigen
                if (datenPset.istAbspielen()) {
                    setIcon(ja_16);
                } else {
                    setIcon(nein_12);
                }
            }
            if (c == DatenPset.PROGRAMMSET_IST_SPEICHERN) {
                setHorizontalAlignment(SwingConstants.CENTER);
                setText(""); // nur das Icon anzeigen
                if (datenPset.istSpeichern()) {
                    setIcon(ja_16);
                } else {
                    setIcon(nein_12);
                }
            }
        } catch (Exception ex) {
            Log.errorLog(962380071, ex);
        }
        return this;
    }
}
