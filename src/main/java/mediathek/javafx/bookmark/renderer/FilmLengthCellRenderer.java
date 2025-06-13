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

import org.apache.commons.lang3.time.DurationFormatUtils;

import javax.swing.*;
import java.awt.*;
import java.util.concurrent.TimeUnit;

public class FilmLengthCellRenderer extends CenteredCellRenderer {
    @Override
    public Component getTableCellRendererComponent(JTable table, Object value, boolean isSelected, boolean hasFocus, int row, int column) {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column);
        long length = (int) value;
        if (length >= 0) {
            var duration = TimeUnit.MILLISECONDS.convert(length, TimeUnit.SECONDS);
            var durationStr = DurationFormatUtils.formatDuration(duration, "HH:mm:ss", true);
            setText(durationStr);
        }
        else
            setText(null);
        return this;
    }
}
