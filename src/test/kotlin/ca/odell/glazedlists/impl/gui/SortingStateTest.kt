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
import ca.odell.glazedlists.gui.TableFormat
import org.jspecify.annotations.Nullable
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class SortingStateTest {
    @Test
    fun sortingStylesCoverPrimarySecondaryAlternateAndReverseStates() {
        val state = sortingState()
        val primary = state.columns[0]
        val secondary = state.columns[1]
        primary.comparators += compareByDescending { it.first }
        secondary.comparators += compareByDescending { it.second }

        assertEquals(0, primary.sortingStyle)
        state.appendComparator(column = 0, comparatorIndex = 0, reverse = false, multipleColumnSort = true)
        assertEquals(1, primary.sortingStyle)
        primary.isReverse = true
        assertEquals(2, primary.sortingStyle)
        primary.comparatorIndex = 1
        primary.isReverse = false
        assertEquals(3, primary.sortingStyle)
        primary.isReverse = true
        assertEquals(4, primary.sortingStyle)

        state.appendComparator(column = 1, comparatorIndex = 0, reverse = false, multipleColumnSort = true)
        assertEquals(5, secondary.sortingStyle)
        secondary.isReverse = true
        assertEquals(6, secondary.sortingStyle)
        secondary.comparatorIndex = 1
        secondary.isReverse = false
        assertEquals(7, secondary.sortingStyle)
        secondary.isReverse = true
        assertEquals(8, secondary.sortingStyle)
    }

    @Test
    fun comparatorChainUsesClickedColumnOrderAndReverseFlags() {
        val state = sortingState()
        state.appendComparator(column = 1, comparatorIndex = 0, reverse = false, multipleColumnSort = true)
        state.appendComparator(column = 0, comparatorIndex = 0, reverse = true, multipleColumnSort = true)

        val comparator = requireNotNull(state.buildComparator())

        val values = listOf(Row(1, 2), Row(2, 1), Row(3, 1)).sortedWith(comparator)
        assertEquals(listOf(Row(3, 1), Row(2, 1), Row(1, 2)), values)
    }

    @Test
    fun knownComparatorChainsAreDetectedWithOrderAndDirection() {
        val state = sortingState()
        val secondColumn = state.columns[1].comparators[0]
        val reversedFirstColumn = GlazedLists.reverseComparator(state.columns[0].comparators[0])
        val chain = GlazedLists.chainComparators(listOf(secondColumn, reversedFirstColumn))

        assertTrue(state.detectStateFromComparator(chain))
        assertEquals(listOf(1, 0), state.recentlyClickedColumns.map { it.column })
        assertFalse(state.columns[1].isReverse)
        assertTrue(state.columns[0].isReverse)
    }

    @Test
    fun unknownComparatorsReportPartialDetectionWithoutInventingState() {
        val state = sortingState()
        val known = state.columns[0].comparators[0]
        val foreign = compareBy<Row> { it.first + it.second }
        val chain = GlazedLists.chainComparators(listOf(known, foreign))

        assertFalse(state.detectStateFromComparator(chain))
        assertEquals(listOf(0), state.recentlyClickedColumns.map { it.column })
        assertEquals(0, state.columns[0].comparatorIndex)
        assertEquals(-1, state.columns[1].comparatorIndex)

        state.clearComparators()
        assertNull(state.buildComparator())
    }

    @Test
    fun invalidCoordinatesKeepExactValidationMessages() {
        val state = sortingState()

        val columnFailure = assertThrows(IllegalArgumentException::class.java) {
            state.validateComparator(2, 0)
        }
        assertEquals("invalid column 2, must be in range [0, 2)", columnFailure.message)

        val comparatorFailure = assertThrows(IllegalArgumentException::class.java) {
            state.validateComparator(0, 1)
        }
        assertEquals("invalid comparator index 1, must be in range [0, 1)", comparatorFailure.message)
    }

    @Test
    fun nullableComparatorContractsKeepJSpecifyTypeAnnotations() {
        val comparatorReturn = SortingState::class.java
            .getMethod("buildComparator")
            .annotatedReturnType
        val comparatorParameter = SortingState::class.java
            .getMethod("detectStateFromComparator", Comparator::class.java)
            .annotatedParameterTypes
            .single()

        assertTrue(comparatorReturn.isAnnotationPresent(Nullable::class.java))
        assertTrue(comparatorParameter.isAnnotationPresent(Nullable::class.java))
    }

    private fun sortingState() = SortingState<Row>().apply { rebuildColumns(tableFormat) }

    private data class Row(val first: Int, val second: Int)

    private companion object {
        val tableFormat = object : TableFormat<Row> {
            override fun getColumnCount(): Int = 2

            override fun getColumnName(column: Int): String = "Column $column"

            override fun getColumnValue(baseObject: Row, column: Int): Any =
                if (column == 0) baseObject.first else baseObject.second
        }
    }
}
