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
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.event.ListEventListener
import javax.swing.table.AbstractTableModel

/** Converts list events into Swing table-model events and fires them. */
interface TableModelEventAdapter<E> : ListEventListener<E> {
    fun fireTableStructureChanged()

    fun fireTableDataChanged()

    fun fireTableChanged(startIndex: Int, endIndex: Int, listChangeType: Int)

    /** Creates an adapter for an [AbstractTableModel]. */
    fun interface Factory<E> {
        fun create(tableModel: AbstractTableModel): TableModelEventAdapter<E>
    }
}
