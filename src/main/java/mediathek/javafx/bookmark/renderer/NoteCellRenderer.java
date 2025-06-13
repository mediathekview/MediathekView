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

package mediathek.javafx.bookmark.renderer;

import javax.swing.*;
import javax.swing.plaf.UIResource;
import javax.swing.table.TableCellRenderer;
import java.awt.*;

public class NoteCellRenderer extends JPanel implements TableCellRenderer {
    protected final JCheckBox checkBox = new JCheckBox();

    public NoteCellRenderer() {
        setLayout(new BorderLayout());
        checkBox.setHorizontalAlignment(SwingConstants.CENTER);
        add(checkBox, BorderLayout.CENTER);
    }

    protected void performSelectionDrawing(JTable table, boolean isSelected, int row) {
        if (isSelected) {
            setForeground(table.getSelectionForeground());
            setBackground(table.getSelectionBackground());
        }
        else {
            Color background = table.getBackground();
            if (background == null || background instanceof UIResource) {
                Color alternateColor = UIManager.getColor("Table.alternateRowColor");
                if (alternateColor != null && row % 2 != 0) {
                    background = alternateColor;
                }
            }
            setForeground(table.getForeground());
            setBackground(background);
        }
    }

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        if (table == null) {
            return this;
        }

        performSelectionDrawing(table, isSelected, row);

        checkBox.setSelected(value != null);
        if (value != null) {
            setToolTipText((String) value);
        }
        else
            setToolTipText(null);

        return this;
    }
}
