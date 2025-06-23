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

public class TableUtils {
    public static void fitColumnHeaders(JTable table, int padding) {
        var colModel = table.getColumnModel();
        var header = table.getTableHeader();
        for (int i = 0; i < colModel.getColumnCount(); i++) {
            var col = colModel.getColumn(i);
            // get the header renderer (or fall back to default)
            var renderer = col.getHeaderRenderer();
            if (renderer == null) {
                renderer = header.getDefaultRenderer();
            }
            // prepare the renderer component so we can ask its preferred size
            var comp = renderer.getTableCellRendererComponent(
                    table, col.getHeaderValue(), false, false, -1, i);
            // add a bit of padding so it’s not jam-packed
            col.setMinWidth(comp.getPreferredSize().width + padding);
        }
    }
}
