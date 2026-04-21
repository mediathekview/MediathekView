/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.swing.table

import javax.swing.JTable

object TableUtils {
    fun fitColumnHeaders(table: JTable, padding: Int) {
        val columnModel = table.columnModel
        val header = table.tableHeader

        for (columnIndex in 0 until columnModel.columnCount) {
            val column = columnModel.getColumn(columnIndex)
            val renderer = column.headerRenderer ?: header.defaultRenderer
            val component = renderer.getTableCellRendererComponent(
                table,
                column.headerValue,
                false,
                false,
                -1,
                columnIndex
            )
            column.minWidth = component.preferredSize.width + padding
        }
    }
}
