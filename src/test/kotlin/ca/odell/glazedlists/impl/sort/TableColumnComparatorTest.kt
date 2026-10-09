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
package ca.odell.glazedlists.impl.sort

import ca.odell.glazedlists.gui.TableFormat
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class TableColumnComparatorTest {
    private val tableFormat = object : TableFormat<Row> {
        override fun getColumnCount(): Int = 2

        override fun getColumnName(column: Int): String = "Column $column"

        override fun getColumnValue(baseObject: Row, column: Int): Any? = baseObject.values[column]
    }

    @Test
    fun defaultComparatorUsesTheSelectedColumnAndKeepsNullFirstOrdering() {
        val comparator = TableColumnComparator(tableFormat, 1)

        assertTrue(comparator.compare(Row("same", 2), Row("same", 1)) > 0)
        assertTrue(comparator.compare(Row("same", null), Row("same", 1)) < 0)
    }

    @Test
    fun customComparatorFailureKeepsTheHelpfulMessageAndCause() {
        val failingComparator = Comparator<Any?> { _, _ -> throw ClassCastException("unsupported") }
        val comparator = TableColumnComparator(tableFormat, 1, failingComparator)

        val failure = assertThrows(IllegalStateException::class.java) {
            comparator.compare(Row("left", 1), Row("right", 2))
        }

        assertEquals(
            "TableComparatorChooser can not sort objects \"1\", \"2\" using the provided Comparator.",
            failure.message,
        )
        assertEquals("unsupported", failure.cause?.message)
    }

    @Test
    fun equalityAndHashCodeUseExactClassFormatColumnAndComparator() {
        val first = TableColumnComparator(tableFormat, 1)
        val equal = TableColumnComparator(tableFormat, 1)

        assertEquals(first, equal)
        assertEquals(first.hashCode(), equal.hashCode())
        assertNotEquals(first, TableColumnComparator(tableFormat, 0))
        assertNotEquals(first, DerivedTableColumnComparator(tableFormat, 1))
    }

    private class Row(vararg val values: Any?)

    private class DerivedTableColumnComparator(
        tableFormat: TableFormat<Row>,
        column: Int,
    ) : TableColumnComparator<Row>(tableFormat, column)
}
