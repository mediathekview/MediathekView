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
package ca.odell.glazedlists.impl.swing

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.swing.MutableTableModelEvent
import ca.odell.glazedlists.swing.TableModelEventAdapter
import java.awt.EventQueue
import javax.swing.table.AbstractTableModel

private open class DefaultTableModelEventAdapter<E>(
    private val tableModel: AbstractTableModel,
) : TableModelEventAdapter<E> {
    private val tableModelEvent = MutableTableModelEvent(tableModel)

    override fun listChanged(listChanges: ListEvent<E>) {
        checkAccessThread()
        while (listChanges.nextBlock()) {
            tableModelEvent.setValues(
                listChanges.blockStartIndex,
                listChanges.blockEndIndex,
                listChanges.type,
            )
            tableModel.fireTableChanged(tableModelEvent)
        }
    }

    final override fun fireTableStructureChanged() {
        tableModelEvent.setStructureChanged()
        tableModel.fireTableChanged(tableModelEvent)
    }

    final override fun fireTableDataChanged() {
        tableModelEvent.setAllDataChanged()
        tableModel.fireTableChanged(tableModelEvent)
    }

    final override fun fireTableChanged(startIndex: Int, endIndex: Int, listChangeType: Int) {
        tableModelEvent.setValues(startIndex, endIndex, listChangeType)
        tableModel.fireTableChanged(tableModelEvent)
    }

    protected fun checkAccessThread() {
        check(EventQueue.isDispatchThread()) {
            "Events to ${tableModel.javaClass.simpleName} must arrive on the EDT - " +
                "consider adding source.swingThreadProxyList() somewhere in your list pipeline"
        }
    }
}

private open class ManyToOneTableModelEventAdapter<E>(
    tableModel: AbstractTableModel,
) : DefaultTableModelEventAdapter<E>(tableModel) {
    override fun listChanged(listChanges: ListEvent<E>) {
        if (listChanges.isReordering) {
            super.listChanged(listChanges)
        } else {
            checkAccessThread()
            fireOneTableModelEvent(listChanges)
        }
    }

    private fun fireOneTableModelEvent(listChanges: ListEvent<E>) {
        listChanges.nextBlock()
        val startIndex = listChanges.blockStartIndex
        val endIndex = listChanges.blockEndIndex
        val changeType = listChanges.type

        if (listChanges.nextBlock()) {
            fireTableDataChanged()
        } else {
            fireTableChanged(startIndex, endIndex, changeType)
        }
    }
}

/** Factory for the precise one-table-event-per-list-block adapter. */
internal open class DefaultTableModelEventAdapterFactory<E> : TableModelEventAdapter.Factory<E> {
    override fun create(tableModel: AbstractTableModel): TableModelEventAdapter<E> =
        DefaultTableModelEventAdapter(tableModel)

    companion object {
        private val INSTANCE: TableModelEventAdapter.Factory<Any> =
            DefaultTableModelEventAdapterFactory()

        @Suppress("UNCHECKED_CAST")
        fun <E> getInstance(): TableModelEventAdapter.Factory<E> =
            INSTANCE as TableModelEventAdapter.Factory<E>
    }
}

/** Factory for the adapter that emits at most one table event per list event. */
internal open class ManyToOneTableModelEventAdapterFactory<E> : TableModelEventAdapter.Factory<E> {
    override fun create(tableModel: AbstractTableModel): TableModelEventAdapter<E> =
        ManyToOneTableModelEventAdapter(tableModel)

    companion object {
        private val INSTANCE: TableModelEventAdapter.Factory<Any> =
            ManyToOneTableModelEventAdapterFactory()

        @Suppress("UNCHECKED_CAST")
        fun <E> getInstance(): TableModelEventAdapter.Factory<E> =
            INSTANCE as TableModelEventAdapter.Factory<E>
    }
}
