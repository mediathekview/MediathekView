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
package ca.odell.glazedlists.impl.gui

import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.gui.AdvancedTableFormat
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.sort.ComparatorChain
import ca.odell.glazedlists.impl.sort.ReverseComparator
import ca.odell.glazedlists.impl.sort.TableColumnComparator
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeSupport

/** Tracks which table columns are sorted and how they contribute to the active comparator. */
class SortingState<E : Any> {
    private var sortingColumns: List<SortingColumn> = emptyList()

    /** Active columns in comparator precedence order. */
    val recentlyClickedColumns: MutableList<SortingColumn> = ArrayList(2)

    private val changeSupport = PropertyChangeSupport(this)

    fun fireSortingChanged() {
        changeSupport.firePropertyChange("comparator", null, null)
    }

    fun addPropertyChangeListener(listener: PropertyChangeListener?) {
        changeSupport.addPropertyChangeListener(listener)
    }

    fun removePropertyChangeListener(listener: PropertyChangeListener?) {
        changeSupport.removePropertyChangeListener(listener)
    }

    fun buildComparator(): Comparator<E>? {
        if (recentlyClickedColumns.isEmpty()) return null

        val comparators = ArrayList<Comparator<E>>(recentlyClickedColumns.size)
        recentlyClickedColumns.forEach { sortingColumn ->
            comparators += sortingColumn.comparator ?: throw IllegalStateException()
        }
        return GlazedLists.chainComparators(comparators)
    }

    fun disableSortingForColumn(column: Int): Boolean {
        val sortingColumn = requireColumn(column)
        val activeSortRemoved = recentlyClickedColumns.remove(sortingColumn)
        sortingColumn.clear()
        sortingColumn.comparators.clear()
        return activeSortRemoved
    }

    fun appendComparator(
        column: Int,
        comparatorIndex: Int,
        reverse: Boolean,
        multipleColumnSort: Boolean,
    ): Boolean {
        val sortingColumn = validateComparator(column, comparatorIndex)
        if (sortingColumn in recentlyClickedColumns) return false

        if (!multipleColumnSort) clearComparators()
        sortingColumn.comparatorIndex = comparatorIndex
        sortingColumn.isReverse = reverse
        recentlyClickedColumns += sortingColumn
        return true
    }

    fun validateComparator(column: Int, comparatorIndex: Int): SortingColumn {
        val sortingColumn = requireColumn(column)
        val comparatorCount = sortingColumn.comparators.size
        require(comparatorIndex in 0 until comparatorCount) {
            "invalid comparator index $comparatorIndex, must be in range [0, $comparatorCount)"
        }
        return sortingColumn
    }

    private fun requireColumn(column: Int): SortingColumn {
        require(column in sortingColumns.indices) {
            "invalid column $column, must be in range [0, ${sortingColumns.size})"
        }
        return sortingColumns[column]
    }

    fun detectStateFromComparator(foreignComparator: Comparator<*>?): Boolean {
        clearComparators()

        val comparators = when (foreignComparator) {
            null -> emptyList()
            is ComparatorChain<*> -> foreignComparator.comparators.asList()
            else -> listOf(foreignComparator)
        }

        var fullyDetected = true
        comparators.forEach { foreignPart ->
            var comparator = foreignPart
            val reverse = comparator is ReverseComparator<*>
            if (reverse) comparator = comparator.sourceComparator

            var detected = false
            for (sortingColumn in sortingColumns) {
                if (sortingColumn in recentlyClickedColumns) continue
                val comparatorIndex = sortingColumn.comparators.indexOf(comparator)
                if (comparatorIndex == -1) continue

                sortingColumn.comparatorIndex = comparatorIndex
                sortingColumn.isReverse = reverse
                recentlyClickedColumns += sortingColumn
                detected = true
                break
            }
            fullyDetected = fullyDetected && detected
        }
        return fullyDetected
    }

    fun clearComparators() {
        recentlyClickedColumns.forEach { it.clear() }
        recentlyClickedColumns.clear()
    }

    /** Rebuilds column state for a replacement table format and clears active sorting. */
    fun rebuildColumns(tableFormat: TableFormat<in E>) {
        sortingColumns = MutableList(tableFormat.getColumnCount()) { column ->
            SortingColumn(tableFormat, column)
        }
        recentlyClickedColumns.clear()
    }

    /** Columns in table-model index order. */
    val columns: List<SortingColumn>
        get() = sortingColumns

    /** Mutable sorting state for one table column. */
    open inner class SortingColumn(
        tableFormat: TableFormat<in E>,
        open val column: Int,
    ) {
        open val comparators: MutableList<Comparator<E>> = ArrayList(1)

        private var reverse: Boolean = false

        open var isReverse: Boolean
            get() = reverse
            set(value) {
                reverse = value
            }

        open var comparatorIndex: Int = -1
            set(value) {
                assert(value < comparators.size)
                field = value
            }

        init {
            if (tableFormat is AdvancedTableFormat<*>) {
                val columnComparator = tableFormat.getColumnComparator(column)
                if (columnComparator != null) {
                    comparators += TableColumnComparator(tableFormat, column, columnComparator)
                }
            } else {
                comparators += TableColumnComparator(tableFormat, column)
            }
        }

        open fun clear() {
            isReverse = false
            comparatorIndex = -1
        }

        open val comparator: Comparator<E>?
            get() {
                if (comparatorIndex == -1) return null
                val selectedComparator = comparators[comparatorIndex]
                return if (isReverse) GlazedLists.reverseComparator(selectedComparator) else selectedComparator
            }

        open val sortingStyle: Int
            get() {
                if (comparatorIndex == -1) return COLUMN_UNSORTED

                val primaryColumn = recentlyClickedColumns.firstOrNull() === this
                val primaryComparator = comparatorIndex == 0
                return when {
                    primaryColumn && !isReverse && primaryComparator -> COLUMN_PRIMARY_SORTED
                    primaryColumn && isReverse && primaryComparator -> COLUMN_PRIMARY_SORTED_REVERSE
                    primaryColumn && !isReverse -> COLUMN_PRIMARY_SORTED_ALTERNATE
                    primaryColumn -> COLUMN_PRIMARY_SORTED_ALTERNATE_REVERSE
                    !isReverse && primaryComparator -> COLUMN_SECONDARY_SORTED
                    isReverse && primaryComparator -> COLUMN_SECONDARY_SORTED_REVERSE
                    !isReverse -> COLUMN_SECONDARY_SORTED_ALTERNATE
                    else -> COLUMN_SECONDARY_SORTED_ALTERNATE_REVERSE
                }
            }
    }

    private companion object {
        private const val COLUMN_UNSORTED = 0
        private const val COLUMN_PRIMARY_SORTED = 1
        private const val COLUMN_PRIMARY_SORTED_REVERSE = 2
        private const val COLUMN_PRIMARY_SORTED_ALTERNATE = 3
        private const val COLUMN_PRIMARY_SORTED_ALTERNATE_REVERSE = 4
        private const val COLUMN_SECONDARY_SORTED = 5
        private const val COLUMN_SECONDARY_SORTED_REVERSE = 6
        private const val COLUMN_SECONDARY_SORTED_ALTERNATE = 7
        private const val COLUMN_SECONDARY_SORTED_ALTERNATE_REVERSE = 8
    }
}
