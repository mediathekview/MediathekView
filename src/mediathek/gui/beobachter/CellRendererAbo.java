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

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import mediathek.Log;
import mediathek.daten.DDaten;
import mediathek.daten.DatenAbo;
import mediathek.tool.GuiKonstanten;

public class CellRendererAbo extends DefaultTableCellRenderer {
    
    DDaten daten;
    
    public CellRendererAbo(DDaten d) {
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
            DatenAbo abo = daten.listeAbo.getAboNr(r);
            boolean eingeschaltet = abo.aboIstEingeschaltet();
            boolean exakt = abo.aboIstExakt();
            if (eingeschaltet) {
//                setFont(new java.awt.Font("Dialog", Font.BOLD, 12));
//                    setForeground(GuiKonstanten.ABO_FOREGROUND);
//                if (isSelected) {
//                    setBackground(GuiKonstanten.ABO_SEL);
//                } else {
//                    setBackground(GuiKonstanten.ABO);
//                }
            } else {
                setFont(new java.awt.Font("Dialog", Font.ITALIC, 12));
                if (isSelected) {
                    setBackground(GuiKonstanten.FARBE_GRAU_SEL);
                } else {
                    setBackground(GuiKonstanten.FARBE_GRAU);
                }
            }
            if (c == DatenAbo.ABO_EINGESCHALTET_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (eingeschaltet) {
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
                } else {
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_12.png")));
//                    setText("X");
                }
            }
            if (c == DatenAbo.ABO_THEMA_EXAKT_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
                if (exakt) {
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
                } else {
                    setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_12.png")));
//                    setText("X");
                }
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(this.getClass().getName(), ex);
        }
        return this;
    }
}
