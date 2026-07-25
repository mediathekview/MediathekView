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

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.gui.AdvancedTableFormat
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.gui.WritableTableFormat
import javax.swing.table.AbstractTableModel

/** Adapts an [EventList] so that each element is one row in a Swing table model. */
open class DefaultEventTableModel<E : Any>(
    source: EventList<E>,
    private val disposeSource: Boolean,
    tableFormat: TableFormat<in E>,
) : AbstractTableModel(), AdvancedTableModel<E>, ListEventListener<E> {
    /** The source event list. Set to `null` after disposal to reject later access. */
    @JvmField
    protected var source: EventList<E>? = source

    override var tableFormat: TableFormat<in E> = tableFormat
        set(value) {
            field = value
            eventAdapter.fireTableStructureChanged()
        }

    var eventAdapter: TableModelEventAdapter<E> =
        GlazedListsSwing.defaultEventAdapterFactory<E>().create(this)

    init {
        source.addListEventListener(this)
    }

    constructor(source: EventList<E>, tableFormat: TableFormat<in E>) :
        this(source, false, tableFormat)

    override fun getElementAt(index: Int): E {
        val currentSource = checkNotNull(source)
        currentSource.readWriteLock.readLock().lock()
        return try {
            currentSource[index]
        } finally {
            currentSource.readWriteLock.readLock().unlock()
        }
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        handleListChange(listChanges)
    }

    protected open fun handleListChange(listChanges: ListEvent<E>) {
        eventAdapter.listChanged(listChanges)
    }

    override fun getColumnName(column: Int): String = tableFormat.getColumnName(column)

    override fun getRowCount(): Int {
        val currentSource = checkNotNull(source)
        currentSource.readWriteLock.readLock().lock()
        return try {
            currentSource.size
        } finally {
            currentSource.readWriteLock.readLock().unlock()
        }
    }

    override fun getColumnCount(): Int = tableFormat.getColumnCount()

    override fun getColumnClass(columnIndex: Int): Class<*> =
        (tableFormat as? AdvancedTableFormat<*>)?.getColumnClass(columnIndex)
            ?: super.getColumnClass(columnIndex)

    override fun getValueAt(row: Int, column: Int): Any? {
        val currentSource = checkNotNull(source)
        currentSource.readWriteLock.readLock().lock()
        val rowObject = try {
            currentSource[row]
        } finally {
            currentSource.readWriteLock.readLock().unlock()
        }
        return tableFormat.getColumnValue(rowObject, column)
    }

    @Suppress("UNCHECKED_CAST")
    override fun isCellEditable(row: Int, column: Int): Boolean {
        val writableTableFormat = tableFormat as? WritableTableFormat<E> ?: return false
        val currentSource = checkNotNull(source)
        currentSource.readWriteLock.readLock().lock()
        val rowObject = try {
            currentSource[row]
        } finally {
            currentSource.readWriteLock.readLock().unlock()
        }
        return writableTableFormat.isEditable(rowObject, column)
    }

    @Suppress("UNCHECKED_CAST")
    override fun setValueAt(editedValue: Any?, row: Int, column: Int) {
        val writableTableFormat = tableFormat as? WritableTableFormat<E>
            ?: throw UnsupportedOperationException("Unexpected setValueAt() on read-only table")
        val currentSource = checkNotNull(source)
        currentSource.readWriteLock.writeLock().lock()
        try {
            val baseObject = currentSource[row]
            val updatedObject = writableTableFormat.setNullableColumnValue(
                baseObject,
                editedValue,
                column,
            )
            if (updatedObject != null) {
                val baseObjectHasNotMoved =
                    row < rowCount && currentSource[row] === baseObject
                if (baseObjectHasNotMoved) currentSource[row] = updatedObject
            }
        } finally {
            currentSource.readWriteLock.writeLock().unlock()
        }
    }

    override fun dispose() {
        val currentSource = checkNotNull(source)
        currentSource.removeListEventListener(this)
        if (disposeSource) currentSource.dispose()
        source = null
    }

    @Suppress("UNCHECKED_CAST")
    private fun <T> uncheckedCast(value: Any?): T = value as T

    @Suppress("RedundantNullableReturnType")
    private fun <T : Any> WritableTableFormat<T>.setNullableColumnValue(
        baseObject: T,
        editedValue: Any?,
        column: Int,
    ): T? = setColumnValue(baseObject, uncheckedCast(editedValue), column)
}
