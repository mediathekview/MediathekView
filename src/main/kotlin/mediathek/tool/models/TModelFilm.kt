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

import mediathek.daten.DatenFilm
import javax.swing.table.AbstractTableModel

class TModelFilm(capacity: Int = 0) : AbstractTableModel() {
    private val dataList = ArrayList<DatenFilm>(capacity)

    override fun getRowCount(): Int = dataList.size

    override fun getColumnCount(): Int = FilmColumn.TABLE_COLUMN_COUNT

    override fun getColumnClass(columnIndex: Int): Class<*> =
        FilmColumn.fromIndex(columnIndex).valueType

    override fun getColumnName(column: Int): String =
        FilmColumn.fromIndex(column).title()

    override fun getValueAt(row: Int, column: Int): Any {
        val film = dataList[row]
        return FilmColumn.fromIndex(column).valueFrom(film)
    }

    fun addAll(listeFilme: List<DatenFilm>) {
        if (listeFilme.isEmpty()) {
            return
        }
        val oldRowCount = dataList.size
        dataList.addAll(listeFilme)
        fireTableRowsInserted(oldRowCount, dataList.lastIndex)
    }
}
