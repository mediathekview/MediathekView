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
package ca.odell.glazedlists.gui

import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.impl.gui.MouseOnlySortingStrategy
import ca.odell.glazedlists.impl.gui.SortingState
import ca.odell.glazedlists.impl.gui.SortingStrategy
import java.util.*

/**
 * Allows a table widget to sort a [SortedList] by selecting table columns.
 */
abstract class AbstractTableComparatorChooser<E : Any> protected constructor(
    private val sortedList: SortedList<E>,
    tableFormat: TableFormat<in E>,
    sortingStrategy: SortingStrategy,
) {
    @JvmRecord
    data class SortKey(
        val column: Int,
        val comparatorIndex: Int,
        val reverse: Boolean,
    ) {
        override fun toString(): String =
            "SortKey[column=$column, comparatorIndex=$comparatorIndex, reverse=$reverse]"
    }

    private val multipleColumnSort = sortingStrategy.supportsMultipleColumnSorting()
    private var disposed = false

    /** The potentially foreign comparator currently associated with the sorted list. */
    @JvmField
    protected var sortedListComparator: Comparator<in E>? = null

    private var sortedListComparatorFullyRepresented = false

    /** Manages which columns are sorted and in which order. */
    @JvmField
    protected val sortingState = SortingState<E>()

    init {
        sortingState.rebuildColumns(tableFormat)
        sortedListComparator = readSortedListComparator()
        sortedListComparatorFullyRepresented = sortingState.detectStateFromComparator(sortedListComparator)
        sortingState.addPropertyChangeListener { rebuildComparator() }
    }

    /** Applies the comparator represented by the current sorting state. */
    protected open fun rebuildComparator() {
        val rebuiltComparator = sortingState.buildComparator()
        val currentSortedList = getSortedList()

        currentSortedList.readWriteLock.writeLock().lock()
        try {
            currentSortedList.comparator = rebuiltComparator
            sortedListComparator = rebuiltComparator
            sortedListComparatorFullyRepresented = true
        } finally {
            currentSortedList.readWriteLock.writeLock().unlock()
        }
    }

    /** Returns the sorted list, rejecting access after disposal. */
    protected fun getSortedList(): SortedList<E> {
        check(!disposed) { "TableComparatorChooser has been disposed" }
        return sortedList
    }

    private fun readSortedListComparator(): Comparator<in E>? {
        val currentSortedList = getSortedList()
        currentSortedList.readWriteLock.readLock().lock()
        return try {
            currentSortedList.comparator
        } finally {
            currentSortedList.readWriteLock.readLock().unlock()
        }
    }

    /** Replaces the table format and clears all active sorting. */
    protected fun setTableFormat(tableFormat: TableFormat<in E>) {
        sortingState.rebuildColumns(tableFormat)
        sortingState.fireSortingChanged()
    }

    /** Disables sorting for [column] and clears an active sort on it. */
    open fun disableSortingForColumn(column: Int) {
        if (sortingState.disableSortingForColumn(column)) {
            sortingState.fireSortingChanged()
        }
    }

    /** Returns an immutable snapshot of the current sort keys. */
    open val sortKeys: List<SortKey>
        get() {
            val sortKeys = ArrayList<SortKey>(sortingState.recentlyClickedColumns.size)
            for (sortingColumn in sortingState.recentlyClickedColumns) {
                sortKeys.add(
                    SortKey(
                        sortingColumn.column,
                        sortingColumn.comparatorIndex,
                        sortingColumn.isReverse,
                    ),
                )
            }
            return Collections.unmodifiableList(sortKeys)
        }

    /** Appends a comparator to the current sequence, subject to the configured strategy. */
    open fun appendComparator(column: Int, comparatorIndex: Int, reverse: Boolean) {
        if (sortingState.appendComparator(column, comparatorIndex, reverse, multipleColumnSort)) {
            sortingState.fireSortingChanged()
        }
    }

    /** Atomically replaces the complete sorting state after validating every key. */
    open fun setSortKeys(sortKeys: List<SortKey>): Boolean {
        val validatedSortKeys = ArrayList<SortKey>(sortKeys.size)
        for (sortKey in sortKeys) {
            sortingState.validateComparator(sortKey.column, sortKey.comparatorIndex)
            validatedSortKeys.add(sortKey)
        }

        val normalizedSortKeys = normalizeSortKeys(validatedSortKeys)
        if (this.sortKeys == normalizedSortKeys &&
            sortedListComparatorFullyRepresented &&
            readSortedListComparator() === sortedListComparator
        ) {
            return false
        }

        sortingState.clearComparators()
        for ((column, comparatorIndex, reverse) in normalizedSortKeys) {
            sortingState.appendComparator(column, comparatorIndex, reverse, true)
        }
        sortingState.fireSortingChanged()
        return true
    }

    private fun normalizeSortKeys(sortKeys: List<SortKey>): List<SortKey> {
        val normalizedSortKeys = ArrayList<SortKey>(sortKeys.size)
        val usedColumns = HashSet<Int>()
        for (sortKey in sortKeys) {
            if (multipleColumnSort) {
                if (usedColumns.add(sortKey.column)) normalizedSortKeys.add(sortKey)
            } else if (normalizedSortKeys.isEmpty() || normalizedSortKeys.first().column != sortKey.column) {
                normalizedSortKeys.clear()
                normalizedSortKeys.add(sortKey)
            }
        }
        return normalizedSortKeys
    }

    /** Clears all sorting and restores source order. */
    open fun clearComparator() {
        if (sortingState.recentlyClickedColumns.isEmpty() && readSortedListComparator() == null) return

        sortingState.clearComparators()
        sortingState.fireSortingChanged()
    }

    /** Redetects sorting state from the current list comparator. */
    protected open fun redetectComparator(currentComparator: Comparator<in E>?) {
        sortedListComparator = currentComparator
        sortedListComparatorFullyRepresented = sortingState.detectStateFromComparator(currentComparator)
    }

    /** Returns the sorting style currently applied to [column]. */
    protected open fun getSortingStyle(column: Int): Int = sortingState.columns[column].sortingStyle

    /** Releases resources owned by this chooser. */
    fun dispose() {
        if (disposed) return

        disposed = true
        disposeInternal()
        sortedListComparator = null
        sortedListComparatorFullyRepresented = false
    }

    /** Hook for subclasses to release their resources. */
    protected open fun disposeInternal() = Unit

    companion object {
        /** Sorts at most one column at a time. */
        @JvmField
        val SINGLE_COLUMN: SortingStrategy = MouseOnlySortingStrategy(false)

        /** Sorts multiple columns without requiring keyboard modifiers. */
        @JvmField
        val MULTIPLE_COLUMN_MOUSE: SortingStrategy = MouseOnlySortingStrategy(true)
    }
}
