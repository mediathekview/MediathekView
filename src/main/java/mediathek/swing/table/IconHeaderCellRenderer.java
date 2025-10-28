/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.swing.table;

import javax.swing.*;
import javax.swing.table.DefaultTableCellRenderer;
import java.awt.*;

public class IconHeaderCellRenderer extends DefaultTableCellRenderer {
    private final Icon icon;
    private final String tooltipText;

    public IconHeaderCellRenderer(Icon icon, String tooltipText) {
        this.icon = icon;
        this.tooltipText = tooltipText;
    }

    @Override
    public Component getTableCellRendererComponent(JTable table,
                                                   Object value,
                                                   boolean isSelected,
                                                   boolean hasFocus,
                                                   int row, int column) {
        var comp = (Component)table.getTableHeader().getColumnModel().getColumn(column).getHeaderRenderer();
        if (comp instanceof JLabel lbl) {
            lbl.setHorizontalTextPosition(SwingConstants.LEFT);
            lbl.setIcon(icon);
            lbl.setText("");
            lbl.setToolTipText(tooltipText);
            lbl.setBorder(UIManager.getBorder("TableHeader.cellBorder"));
        }
        return comp;
    }
}
