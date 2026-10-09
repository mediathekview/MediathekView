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

package mediathek.tool.cellrenderer

import mediathek.daten.DatenPset
import java.awt.Color
import java.awt.Component
import javax.swing.JTable
import javax.swing.table.DefaultTableCellRenderer

class PsetNameCellRenderer : DefaultTableCellRenderer() {
    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int,
    ): Component {
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
        foreground = psetNameColor(table, row) ?: defaultForeground(table, isSelected)

        return this
    }

    private fun psetNameColor(table: JTable, row: Int): Color? {
        val modelRow = table.convertRowIndexToModel(row)
        val datenPset = DatenPset()
        datenPset[DatenPset.PROGRAMMSET_FARBE] = table.model
            .getValueAt(modelRow, DatenPset.PROGRAMMSET_FARBE)
            ?.toString()
            .orEmpty()
        return datenPset.farbe
    }

    private fun defaultForeground(table: JTable, isSelected: Boolean): Color =
        if (isSelected) table.selectionForeground else table.foreground
}
