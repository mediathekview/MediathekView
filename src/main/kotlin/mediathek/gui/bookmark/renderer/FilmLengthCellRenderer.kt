/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.gui.bookmark.renderer

import mediathek.audiothek.ui.table.CenteredTextCellRenderer
import org.apache.commons.lang3.time.DurationFormatUtils
import java.awt.Component
import java.util.concurrent.TimeUnit
import javax.swing.JTable

class FilmLengthCellRenderer : CenteredTextCellRenderer() {
    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int,
    ): Component {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

        val length = value as Int
        text = if (length >= 0) {
            val duration = TimeUnit.MILLISECONDS.convert(length.toLong(), TimeUnit.SECONDS)
            DurationFormatUtils.formatDuration(duration, "HH:mm:ss", true)
        } else {
            null
        }

        return this
    }
}
