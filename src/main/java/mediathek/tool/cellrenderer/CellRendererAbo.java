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
package mediathek.tool.cellrenderer;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.MVColor;
import mediathek.daten.DatenAbo;
import mediathek.tool.MVSenderIconCache;
import mediathek.tool.table.MVTable;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class CellRendererAbo extends CellRendererBase {
    private static final Logger logger = LogManager.getLogger(CellRendererAbo.class);
    private final Icon checkedIcon;
    private final Icon uncheckedIcon;

    public CellRendererAbo(MVSenderIconCache cache) {
        super(cache);


        checkedIcon = IconFontSwing.buildIcon(FontAwesome.CHECK, 12);
        uncheckedIcon = IconFontSwing.buildIcon(FontAwesome.MINUS, 12);
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
            final int r = table.convertRowIndexToModel(row);
            final int c = table.convertColumnIndexToModel(column);
            DatenAbo abo = Daten.getInstance().getListeAbo().getAboNr(r);
            final boolean aboIstEingeschaltet = abo.aboIstEingeschaltet();

            if (((MVTable) table).isLineBreak()) {
                switch (c) {
                    case DatenAbo.ABO_IRGENDWO:
                    case DatenAbo.ABO_NAME:
                    case DatenAbo.ABO_THEMA:
                    case DatenAbo.ABO_THEMA_TITEL:
                    case DatenAbo.ABO_TITEL:
                    case DatenAbo.ABO_ZIELPFAD:
                        JTextArea textArea = new JTextArea();
                        textArea.setLineWrap(true);
                        textArea.setWrapStyleWord(true);
                        textArea.setText(value.toString());
                        textArea.setForeground(getForeground());
                        textArea.setBackground(getBackground());
                        setSelectionFont(textArea, isSelected);
                        if (!aboIstEingeschaltet)
                            setBackgroundColor(textArea, isSelected);
                        return textArea;
                }
            }

            setSelectionFont(this, isSelected);

            switch (c) {
                case DatenAbo.ABO_NR:
                case DatenAbo.ABO_MINDESTDAUER:
                case DatenAbo.ABO_MIN:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    break;

                case DatenAbo.ABO_EINGESCHALTET:
                    setHorizontalAlignment(SwingConstants.CENTER);
                    setCheckedOrUncheckedIcon(aboIstEingeschaltet);
                    break;

                case DatenAbo.ABO_SENDER:
                    if (((MVTable) table).showSenderIcons()) {
                        setSenderIcon((String) value, ((MVTable) table).useSmallSenderIcons);
                    }
                    break;
            }

            if (!aboIstEingeschaltet)
                setBackgroundColor(this, isSelected);
        } catch (Exception ex) {
            logger.error("Fehler 630365892", ex);
        }
        return this;
    }

    private void setBackgroundColor(Component c, final boolean isSelected) {
        setFontItalic();
        if (isSelected) {
            c.setBackground(MVColor.ABO_AUSGESCHALTET_SEL.color);
        } else {
            c.setBackground(MVColor.ABO_AUSGESCHALTET.color);
        }
    }

    private void setFontItalic() {
        if (!SystemUtils.IS_OS_MAC_OSX) {
            // On OS X do not change fonts as it violates HIG...
            setFont(getFont().deriveFont(Font.ITALIC));
        }
    }

    /**
     * Set icon either to yes or no based on condition
     *
     * @param condition yes if true, no if false
     */
    private void setCheckedOrUncheckedIcon(final boolean condition) {
        final Icon icon;
        if (condition)
            icon = checkedIcon;
        else
            icon = uncheckedIcon;

        setIcon(icon);
    }

}
