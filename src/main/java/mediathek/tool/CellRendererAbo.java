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

import com.jidesoft.utils.SystemInfo;
import mSearch.tool.Log;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVColor;
import mediathek.daten.DatenAbo;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

@SuppressWarnings("serial")
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
            int r = table.convertRowIndexToModel(row);
            int c = table.convertColumnIndexToModel(column);
            DatenAbo abo = Daten.getInstance().getListeAbo().getAboNr(r);
            boolean eingeschaltet = abo.aboIstEingeschaltet();

            if (((MVTable) table).lineBreak) {
                JTextArea textArea;
                switch (c) {
                    case DatenAbo.ABO_IRGENDWO:
                    case DatenAbo.ABO_NAME:
                    case DatenAbo.ABO_THEMA:
                    case DatenAbo.ABO_THEMA_TITEL:
                    case DatenAbo.ABO_TITEL:
                    case DatenAbo.ABO_ZIELPFAD:
                        textArea = new JTextArea();
                        textArea.setLineWrap(true);
                        textArea.setWrapStyleWord(true);
                        textArea.setText(value.toString());
                        textArea.setForeground(getForeground());
                        textArea.setBackground(getBackground());
                        if (!SystemInfo.isMacOSX()) {
                            // On OS X do not change fonts as it violates HIG...
                            if (isSelected) {
                                textArea.setFont(new java.awt.Font("Dialog", Font.BOLD, MVFont.fontSize));
                            } else {
                                textArea.setFont(new java.awt.Font("Dialog", Font.PLAIN, MVFont.fontSize));
                            }
                        }
                        if (!eingeschaltet) {
                            setFont(new java.awt.Font("Dialog", Font.ITALIC, getFont().getSize()));
                            if (isSelected) {
                                textArea.setBackground(MVColor.ABO_AUSGESCHALTET_SEL.color);
                            } else {
                                textArea.setBackground(MVColor.ABO_AUSGESCHALTET.color);
                            }
                        }
                        return textArea;
                }
            }

            if (!SystemInfo.isMacOSX()) {
                // On OS X do not change fonts as it violates HIG...
                if (isSelected) {
                    setFont(new java.awt.Font("Dialog", Font.BOLD, MVFont.fontSize));
                } else {
                    setFont(new java.awt.Font("Dialog", Font.PLAIN, MVFont.fontSize));
                }
            }
            if (c == DatenAbo.ABO_NR) {
                setHorizontalAlignment(SwingConstants.CENTER);
            }
            if (c == DatenAbo.ABO_MINDESTDAUER) {
                setHorizontalAlignment(SwingConstants.CENTER);
            }
            if (!eingeschaltet) {
                if (!SystemInfo.isMacOSX()) {
                    // On OS X do not change fonts as it violates HIG...
                    setFont(new java.awt.Font("Dialog", Font.ITALIC, getFont().getSize()));
                }
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
                case DatenAbo.ABO_MIN:
                    setHorizontalAlignment(SwingConstants.CENTER);
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
