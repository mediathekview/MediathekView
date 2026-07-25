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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransformedList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import java.util.*
import javax.swing.DefaultListSelectionModel
import javax.swing.ListSelectionModel
import javax.swing.event.*
import javax.swing.table.TableColumn
import javax.swing.table.TableColumnModel

/** A Swing [TableColumnModel] backed by an [EventList] of [TableColumn] values. */
open class EventTableColumnModel<T : TableColumn>(
    source: EventList<T>,
) : TableColumnModel,
    PropertyChangeListener,
    ListSelectionListener,
    ListEventListener<T> {
    /** The event-list view used by this model. It is cleared on disposal. */
    @JvmField
    protected var swingThreadSource: TransformedList<T, T> = nullableValue()

    private val disposeSwingThreadSource: Boolean
    private val listenerList = EventListenerList()

    @Transient
    private val changeEvent = ChangeEvent(this)

    private var selectionModel: ListSelectionModel? = null
    private var columnSelectionAllowed = false
    private var columnMargin = 0
    private var totalColumnWidth = 0

    init {
        setSelectionModel(createSelectionModel())
        columnMargin = 1
        invalidateWidthCache()
        columnSelectionAllowed = false

        val readLock = source.readWriteLock.readLock()
        readLock.lock()
        try {
            for (column in source) {
                column.addPropertyChangeListener(this)
            }

            disposeSwingThreadSource = !source.isSwingThreadProxyList()
            @Suppress("UNCHECKED_CAST")
            val eventSource = if (disposeSwingThreadSource) {
                source.swingThreadProxyList()
            } else {
                source as TransformedList<T, T>
            }
            swingThreadSource = eventSource
            eventSource.addListEventListener(this)
        } finally {
            readLock.unlock()
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun addColumn(column: TableColumn) {
        val currentSource = swingThreadSource
        val writeLock = currentSource.readWriteLock.writeLock()
        writeLock.lock()
        try {
            currentSource.add(column as T)
        } finally {
            writeLock.unlock()
        }
    }

    override fun removeColumn(column: TableColumn) {
        val currentSource = swingThreadSource
        val writeLock = currentSource.readWriteLock.writeLock()
        writeLock.lock()
        try {
            currentSource.remove(column)
        } finally {
            writeLock.unlock()
        }
    }

    override fun moveColumn(columnIndex: Int, newIndex: Int) {
        require(columnIndex in 0 until columnCount) { "columnIndex out of range" }
        require(newIndex in 0 until columnCount) { "newIndex out of range" }

        if (columnIndex == newIndex) {
            fireColumnMoved(TableColumnModelEvent(this, columnIndex, newIndex))
            return
        }

        val currentSource = swingThreadSource
        val writeLock = currentSource.readWriteLock.writeLock()
        writeLock.lock()
        try {
            val currentSelectionModel = selectionModel!!
            val selected = currentSelectionModel.isSelectedIndex(columnIndex)
            currentSource.add(newIndex, currentSource.removeAt(columnIndex))
            if (selected) currentSelectionModel.addSelectionInterval(newIndex, newIndex)
        } finally {
            writeLock.unlock()
        }
    }

    override fun setColumnMargin(newMargin: Int) {
        if (newMargin != columnMargin) {
            columnMargin = newMargin
            fireColumnMarginChanged()
        }
    }

    override fun getColumnMargin(): Int = columnMargin

    override fun getColumnCount(): Int {
        val currentSource = swingThreadSource
        val readLock = currentSource.readWriteLock.readLock()
        readLock.lock()
        return try {
            currentSource.size
        } finally {
            readLock.unlock()
        }
    }

    @Suppress("UNCHECKED_CAST")
    override fun getColumns(): Enumeration<TableColumn> =
        Collections.enumeration(swingThreadSource as List<TableColumn>)

    override fun getColumnIndex(identifier: Any): Int {
        val currentSource = swingThreadSource
        val readLock = currentSource.readWriteLock.readLock()
        readLock.lock()
        try {
            for (index in currentSource.indices) {
                if (identifier == currentSource[index].identifier) return index
            }
            throw IllegalArgumentException("Identifier not found")
        } finally {
            readLock.unlock()
        }
    }

    override fun getColumn(columnIndex: Int): TableColumn {
        val currentSource = swingThreadSource
        val readLock = currentSource.readWriteLock.readLock()
        readLock.lock()
        return try {
            currentSource[columnIndex]
        } finally {
            readLock.unlock()
        }
    }

    override fun getColumnIndexAtX(x: Int): Int {
        if (x < 0) return -1

        val currentSource = swingThreadSource
        val readLock = currentSource.readWriteLock.readLock()
        readLock.lock()
        try {
            var remaining = x
            for (index in currentSource.indices) {
                remaining -= currentSource[index].width
                if (remaining < 0) return index
            }
        } finally {
            readLock.unlock()
        }
        return -1
    }

    override fun getTotalColumnWidth(): Int {
        if (totalColumnWidth == -1) recalcWidthCache()
        return totalColumnWidth
    }

    private fun recalcWidthCache() {
        val currentSource = swingThreadSource
        val readLock = currentSource.readWriteLock.readLock()
        readLock.lock()
        try {
            totalColumnWidth = 0
            for (column in currentSource) totalColumnWidth += column.width
        } finally {
            readLock.unlock()
        }
    }

    private fun invalidateWidthCache() {
        totalColumnWidth = -1
    }

    override fun setColumnSelectionAllowed(flag: Boolean) {
        columnSelectionAllowed = flag
    }

    override fun getColumnSelectionAllowed(): Boolean = columnSelectionAllowed

    override fun getSelectedColumns(): IntArray {
        val currentSelectionModel = selectionModel ?: return IntArray(0)
        val minimum = currentSelectionModel.minSelectionIndex
        val maximum = currentSelectionModel.maxSelectionIndex
        if (minimum == -1 || maximum == -1) return IntArray(0)

        val temporary = IntArray(1 + maximum - minimum)
        var count = 0
        for (index in minimum..maximum) {
            if (currentSelectionModel.isSelectedIndex(index)) temporary[count++] = index
        }
        return temporary.copyOf(count)
    }

    override fun getSelectedColumnCount(): Int {
        val currentSelectionModel = selectionModel ?: return 0
        var count = 0
        for (index in currentSelectionModel.minSelectionIndex..currentSelectionModel.maxSelectionIndex) {
            if (currentSelectionModel.isSelectedIndex(index)) count++
        }
        return count
    }

    override fun setSelectionModel(newModel: ListSelectionModel) {
        if (newModel === selectionModel) return

        selectionModel?.removeListSelectionListener(this)
        selectionModel = newModel
        newModel.addListSelectionListener(this)
    }

    override fun getSelectionModel(): ListSelectionModel = selectionModel!!

    override fun addColumnModelListener(listener: TableColumnModelListener?) {
        listenerList.add(TableColumnModelListener::class.java, listener)
    }

    override fun removeColumnModelListener(listener: TableColumnModelListener?) {
        listenerList.remove(TableColumnModelListener::class.java, listener)
    }

    override fun propertyChange(event: PropertyChangeEvent?) {
        val propertyName = event!!.propertyName
        if (propertyName == "width" || propertyName == "preferredWidth") {
            invalidateWidthCache()
            fireColumnMarginChanged()
        }
    }

    override fun valueChanged(event: ListSelectionEvent?) {
        fireColumnSelectionChanged(event)
    }

    override fun listChanged(listChanges: ListEvent<T>) {
        invalidateWidthCache()

        while (listChanges.next()) {
            val index = listChanges.index
            when (listChanges.type) {
                ListEvent.DELETE -> {
                    selectionModel?.removeIndexInterval(index, index)
                    val oldColumn = listChanges.oldValue
                    oldColumn.removePropertyChangeListener(this)
                    fireColumnRemoved(TableColumnModelEvent(this, index, 0))
                }

                ListEvent.INSERT -> {
                    listChanges.sourceList[index].addPropertyChangeListener(this)
                    fireColumnAdded(TableColumnModelEvent(this, 0, columnCount - 1))
                }

                ListEvent.UPDATE -> {
                    val newColumn = listChanges.sourceList[index]
                    val oldColumn = listChanges.oldValue
                    if (oldColumn !== newColumn) {
                        oldColumn.removePropertyChangeListener(this)
                        newColumn.addPropertyChangeListener(this)
                    }
                    fireColumnMoved(TableColumnModelEvent(this, index, index))
                }
            }
        }
    }

    /** Releases listeners and disposes an internally owned Swing-thread proxy. */
    open fun dispose() {
        val source = swingThreadSource
        val readLock = source.readWriteLock.readLock()
        readLock.lock()
        try {
            for (column in source) column.removePropertyChangeListener(this)
            source.removeListEventListener(this)
        } finally {
            readLock.unlock()
        }

        if (disposeSwingThreadSource) source.dispose()
        swingThreadSource = nullableValue()
    }

    /** Creates the selection model installed by the constructor. */
    protected open fun createSelectionModel(): ListSelectionModel = DefaultListSelectionModel()

    protected open fun fireColumnAdded(event: TableColumnModelEvent?) {
        fireToColumnModelListeners { listener -> listener.columnAdded(event) }
    }

    protected open fun fireColumnRemoved(event: TableColumnModelEvent?) {
        fireToColumnModelListeners { listener -> listener.columnRemoved(event) }
    }

    protected open fun fireColumnMoved(event: TableColumnModelEvent?) {
        fireToColumnModelListeners { listener -> listener.columnMoved(event) }
    }

    protected open fun fireColumnSelectionChanged(event: ListSelectionEvent?) {
        fireToColumnModelListeners { listener -> listener.columnSelectionChanged(event) }
    }

    protected open fun fireColumnMarginChanged() {
        fireToColumnModelListeners { listener -> listener.columnMarginChanged(changeEvent) }
    }

    private inline fun fireToColumnModelListeners(action: (TableColumnModelListener) -> Unit) {
        val listeners = listenerList.listenerList
        var index = listeners.size - 2
        while (index >= 0) {
            if (listeners[index] == TableColumnModelListener::class.java) {
                action(listeners[index + 1] as TableColumnModelListener)
            }
            index -= 2
        }
    }

    @Suppress("UNCHECKED_CAST")
    private fun <V> nullableValue(value: Any? = null): V = value as V
}
