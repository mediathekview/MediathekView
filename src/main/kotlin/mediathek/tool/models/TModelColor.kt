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

package mediathek.tool.models

import mediathek.config.MVColor
import mediathek.tool.MVC
import javax.swing.table.AbstractTableModel

class TModelColor(
    val isDarkMode: Boolean,
) : AbstractTableModel() {
    override fun getRowCount(): Int = MVColor.getColors().size

    override fun getColumnCount(): Int = COLUMN_NAMES.size

    override fun getColumnName(column: Int): String = COLUMN_NAMES[column]

    override fun getValueAt(rowIndex: Int, columnIndex: Int): Any? {
        val mvc = getEntry(rowIndex)
        return when (columnIndex) {
            MVColor.MVC_TEXT -> mvc.text
            MVColor.MVC_COLOR -> mvc
            else -> null
        }
    }

    override fun getColumnClass(columnIndex: Int): Class<*> =
        if (columnIndex == MVColor.MVC_COLOR) {
            MVC::class.java
        } else {
            String::class.java
        }

    override fun isCellEditable(rowIndex: Int, columnIndex: Int): Boolean = false

    fun getEntry(rowIndex: Int): MVC = MVColor.get(rowIndex)

    private companion object {
        private val COLUMN_NAMES = arrayOf("Beschreibung", "Farbe")
    }
}
