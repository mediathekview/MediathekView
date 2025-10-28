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

package mediathek.gui.bookmark.renderer;

import mediathek.tool.datum.DateUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.time.LocalDate;
import java.time.temporal.ChronoUnit;

public class AvailableUntilCellRenderer extends CenteredCellRenderer {
    private static final long DAYS_UNTIL_END = 5;

    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);

        var date = (LocalDate) value;
        if (date != null) {
            if (!entryHasExpired(date)) {
                setTextForeground(table, isSelected);
            }
            setText(date.format(DateUtil.FORMATTER));
        }
        return this;
    }

    private boolean entryHasExpired(LocalDate date) {
        boolean result = false;
        var today = LocalDate.now();

        if (date.isBefore(today)) {
            setForeground(Color.red);
            result = true;
        }
        else if (Math.abs(ChronoUnit.DAYS.between(today, date)) < DAYS_UNTIL_END) {
            setForeground(Color.orange);
            result = true;
        }

        return result;
    }

    private void setTextForeground(@NotNull JTable table, boolean isSelected) {
        if (isSelected) {
            setForeground(table.getSelectionForeground());
        }
        else {
            setForeground(table.getForeground());
        }
    }
}
