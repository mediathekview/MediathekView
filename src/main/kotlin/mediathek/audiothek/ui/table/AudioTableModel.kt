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

package mediathek.audiothek.ui.table

import mediathek.audiothek.model.AudioEntry
import javax.swing.table.AbstractTableModel

class AudioTableModel : AbstractTableModel() {
    private val rows = mutableListOf<AudioEntry>()

    fun setRows(entries: List<AudioEntry>) {
        rows.clear()
        rows.addAll(entries)
        fireTableDataChanged()
    }

    fun getEntry(rowIndex: Int): AudioEntry? = rows.getOrNull(rowIndex)

    override fun getRowCount(): Int = rows.size

    override fun getColumnCount(): Int = AudioTableColumn.entries.size

    override fun getColumnName(column: Int): String = AudioTableColumn.entries[column].title

    override fun isCellEditable(rowIndex: Int, columnIndex: Int): Boolean =
        AudioTableColumn.entries[columnIndex].editable

    override fun getValueAt(rowIndex: Int, columnIndex: Int): Any {
        val entry = rows[rowIndex]
        return AudioTableColumn.entries[columnIndex].valueProvider(entry)
    }
}
