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
import java.awt.Font;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.res.GetIcon;

public class CellRendererAbo extends DefaultTableCellRenderer {

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
            DatenAbo abo = Daten.listeAbo.getAboNr(r);
            boolean eingeschaltet = abo.aboIstEingeschaltet();
            if (c == DatenAbo.ABO_NR_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
            }
            if (!eingeschaltet) {
                setFont(new java.awt.Font("Dialog", Font.ITALIC, 12));
                if (isSelected) {
                    setBackground(MVColor.ABO_AUSGESCHALTET_SEL.color);
                } else {
                    setBackground(MVColor.ABO_AUSGESCHALTET.color);
                }
            }
            if (c == DatenAbo.ABO_EINGESCHALTET_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (eingeschaltet) {
                    setIcon(GetIcon.getIcon("ja_16.png"));
                } else {
                    setIcon(GetIcon.getIcon("nein_12.png"));
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(630365892, Log.FEHLER_ART_PROG, this.getClass().getName(), ex);
        }
        return this;
    }
}
