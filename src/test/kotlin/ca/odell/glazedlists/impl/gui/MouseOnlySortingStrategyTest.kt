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

import ca.odell.glazedlists.gui.TableFormat
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class MouseOnlySortingStrategyTest {
    @Test
    fun reportsConfiguredMultipleColumnSupport() {
        assertFalse(MouseOnlySortingStrategy(false).supportsMultipleColumnSorting())
        assertTrue(MouseOnlySortingStrategy(true).supportsMultipleColumnSorting())
    }

    @Test
    fun ignoresColumnsWithoutComparators() {
        val state = sortingState(columnCount = 1)
        state.disableSortingForColumn(0)
        var changes = 0
        state.addPropertyChangeListener { changes++ }

        MouseOnlySortingStrategy(false).click(state, column = 0)

        assertTrue(state.recentlyClickedColumns.isEmpty())
        assertEquals(-1, state.columns[0].comparatorIndex)
        assertEquals(0, changes)
    }

    @Test
    fun singleColumnModeRetainsOnlyTheMostRecentlyClickedColumn() {
        val state = sortingState(columnCount = 2)
        var changes = 0
        state.addPropertyChangeListener { changes++ }
        val strategy = MouseOnlySortingStrategy(false)

        strategy.click(state, column = 0)
        strategy.click(state, column = 1, shift = true, control = true)

        assertEquals(listOf(1), state.recentlyClickedColumns.map { it.column })
        assertEquals(-1, state.columns[0].comparatorIndex)
        assertEquals(0, state.columns[1].comparatorIndex)
        assertEquals(2, changes)
    }

    @Test
    fun multipleColumnModeRetainsPreviouslyClickedColumns() {
        val state = sortingState(columnCount = 2)
        val strategy = MouseOnlySortingStrategy(true)

        strategy.click(state, column = 0)
        strategy.click(state, column = 1, shift = true, control = true)

        assertEquals(listOf(0, 1), state.recentlyClickedColumns.map { it.column })
        assertEquals(0, state.columns[0].comparatorIndex)
        assertEquals(0, state.columns[1].comparatorIndex)
    }

    @Test
    fun repeatedClicksCycleThroughComparatorsAndDirections() {
        val state = sortingState(columnCount = 1)
        state.columns[0].comparators.add(compareByDescending { it })
        val strategy = MouseOnlySortingStrategy(false)

        strategy.click(state, column = 0)
        assertEquals(0, state.columns[0].comparatorIndex)
        assertFalse(state.columns[0].isReverse)

        strategy.click(state, column = 0)
        assertEquals(0, state.columns[0].comparatorIndex)
        assertTrue(state.columns[0].isReverse)

        strategy.click(state, column = 0)
        assertEquals(1, state.columns[0].comparatorIndex)
        assertFalse(state.columns[0].isReverse)

        strategy.click(state, column = 0)
        assertEquals(1, state.columns[0].comparatorIndex)
        assertTrue(state.columns[0].isReverse)

        strategy.click(state, column = 0)
        assertEquals(0, state.columns[0].comparatorIndex)
        assertFalse(state.columns[0].isReverse)
    }

    @Test
    fun doubleClickClearsOtherColumnsAndRestartsTheClickedColumnAscending() {
        val state = sortingState(columnCount = 2)
        val strategy = MouseOnlySortingStrategy(true)

        strategy.click(state, column = 0)
        strategy.click(state, column = 1)
        strategy.click(state, column = 1, clicks = 2)

        assertEquals(listOf(1), state.recentlyClickedColumns.map { it.column })
        assertEquals(-1, state.columns[0].comparatorIndex)
        assertEquals(0, state.columns[1].comparatorIndex)
        assertFalse(state.columns[1].isReverse)
    }

    private fun <E : Any> MouseOnlySortingStrategy.click(
        sortingState: SortingState<E>,
        column: Int,
        clicks: Int = 1,
        shift: Boolean = false,
        control: Boolean = false,
    ) {
        columnClicked(sortingState, column, clicks, shift, control)
    }

    private fun sortingState(columnCount: Int) = SortingState<Int>().apply {
        rebuildColumns(
            object : TableFormat<Int> {
                override fun getColumnCount(): Int = columnCount

                override fun getColumnName(column: Int): String = "Column $column"

                override fun getColumnValue(baseObject: Int, column: Int): Any = baseObject
            },
        )
    }
}
