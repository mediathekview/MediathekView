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
import mediathek.tool.datum.DateUtil
import java.awt.Color
import java.awt.Component
import java.time.LocalDate
import java.time.temporal.ChronoUnit
import javax.swing.JTable

class AvailableUntilCellRenderer : CenteredTextCellRenderer() {
    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int,
    ): Component {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

        val date = value as LocalDate?
        if (date != null) {
            if (!entryHasExpired(date)) {
                setTextForeground(table, isSelected)
            }
            text = date.format(DateUtil.FORMATTER)
        }

        return this
    }

    private fun entryHasExpired(date: LocalDate): Boolean {
        val today = LocalDate.now()

        return when {
            date.isBefore(today) -> {
                foreground = Color.red
                true
            }

            kotlin.math.abs(ChronoUnit.DAYS.between(today, date)) < DAYS_UNTIL_END -> {
                foreground = Color.orange
                true
            }

            else -> false
        }
    }

    private fun setTextForeground(table: JTable, isSelected: Boolean) {
        foreground = if (isSelected) table.selectionForeground else table.foreground
    }

    private companion object {
        const val DAYS_UNTIL_END = 5L
    }
}
