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
import javax.swing.ImageIcon;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.DefaultTableCellRenderer;
import mSearch.tool.Log;
import mSearch.tool.MVColor;
import mediathek.config.Icons;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;

public class CellRendererAbo extends DefaultTableCellRenderer {

    private final MVSenderIconCache senderIconCache;
    private static ImageIcon ja_16 = null;
    private static ImageIcon nein_12 = null;

    public CellRendererAbo() {
        senderIconCache = new MVSenderIconCache();
        ja_16 = Icons.ICON_TABELLE_EIN;
        nein_12 = Icons.ICON_TABELLE_AUS;
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
            if (isSelected) {
                // setFont(new java.awt.Font("Dialog", Font.BOLD, getFont().getSize()));
                setFont(new java.awt.Font("Dialog", Font.BOLD, MVFont.fontSize));
            } else {
                // setFont(getFont());
                setFont(new java.awt.Font("Dialog", Font.PLAIN, MVFont.fontSize));
            }
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            DatenAbo abo = Daten.listeAbo.getAboNr(r);
            boolean eingeschaltet = abo.aboIstEingeschaltet();
            if (c == DatenAbo.ABO_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
            }
            if (c == DatenAbo.ABO_MINDESTDAUER) {
                setHorizontalAlignment(SwingConstants.CENTER);
            }
            if (!eingeschaltet) {
                setFont(new java.awt.Font("Dialog", Font.ITALIC, getFont().getSize()));
                //setFont(new java.awt.Font("Dialog", Font.ITALIC, 12));
                if (isSelected) {
                    setBackground(MVColor.ABO_AUSGESCHALTET_SEL.color);
                } else {
                    setBackground(MVColor.ABO_AUSGESCHALTET.color);
                }
            }
            switch (c) {
                case DatenAbo.ABO_EINGESCHALTET:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    if (eingeschaltet) {
                        setIcon(ja_16);
                    } else {
                        setIcon(nein_12);
                    }
                    break;
                case DatenAbo.ABO_SENDER:
                    if (((MVTable) table).iconAnzeigen) {
                        handleSenderColumn((String) value, ((MVTable) table).iconKlein);
                    }
                    break;
            }
        } catch (Exception ex) {
            Log.errorLog(630365892, ex);
        }
        return this;
    }

    /**
     * Draws the sender icon in the sender model column.
     *
     * @param sender Name of the sender.
     */
    private void handleSenderColumn(String sender, boolean small) {
        setHorizontalAlignment(SwingConstants.CENTER);
        ImageIcon icon = senderIconCache.get(sender, small);
        if (icon != null) {
            setText("");
            setIcon(icon);
        }
    }
}
