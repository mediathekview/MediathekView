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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.impl.gui.SortingStrategy
import ca.odell.glazedlists.swing.TableComparatorChooser
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.JTable
import javax.swing.SwingUtilities

internal class AbstractTableComparatorChooserTest {
    @Test
    fun changingTableFormatClearsSortingFromStateAndSortedList() {
        val source = BasicEventList<Row>().apply {
            addAll(listOf(Row(first = 2, second = 1), Row(first = 1, second = 2)))
        }
        val sorted = SortedList(source, null)
        val chooser = TestChooser(sorted, rowFormat(useSecond = false))
        chooser.appendComparator(0, 0, false)

        chooser.replaceTableFormat(rowFormat(useSecond = true))

        assertEquals(emptyList<Int>(), chooser.sortKeys.map { it.column })
        assertNull(sorted.comparator)
        assertEquals(source.toList(), sorted.toList())
    }

    @Test
    fun programmaticAppendHonorsSingleColumnStrategy() {
        SwingUtilities.invokeAndWait {
            val source = BasicEventList<Row>().apply {
                addAll(listOf(Row(first = 2, second = 1), Row(first = 1, second = 2)))
            }
            val sorted = SortedList(source, null)
            val chooser = TableComparatorChooser.install(
                JTable(0, 2),
                sorted,
                AbstractTableComparatorChooser.SINGLE_COLUMN,
                twoColumnRowFormat(),
            )

            try {
                chooser.appendComparator(0, 0, false)
                chooser.appendComparator(1, 0, false)

                assertEquals(listOf(1), chooser.sortKeys.map { it.column })
            } finally {
                chooser.dispose()
            }
        }
    }

    @Test
    fun disablingAnActiveColumnClearsItsSortSafely() {
        val source = BasicEventList<Row>().apply {
            addAll(listOf(Row(first = 2, second = 1), Row(first = 1, second = 2)))
        }
        val sorted = SortedList(source, null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        chooser.appendComparator(0, 0, false)

        chooser.disableSortingForColumn(0)

        assertEquals(emptyList<Int>(), chooser.sortKeys.map { it.column })
        assertNull(sorted.comparator)
        chooser.appendComparator(1, 0, false)
        assertEquals(listOf(1), chooser.sortKeys.map { it.column })
    }

    @Test
    fun invalidAppendCoordinatesAreRejectedWithoutMutatingState() {
        val sorted = SortedList(BasicEventList<Row>(), null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        chooser.appendComparator(0, 0, false)
        val activeComparator = sorted.comparator

        assertThrows(IllegalArgumentException::class.java) {
            chooser.appendComparator(-1, 0, false)
        }
        assertThrows(IllegalArgumentException::class.java) {
            chooser.appendComparator(2, 0, false)
        }
        assertThrows(IllegalArgumentException::class.java) {
            chooser.appendComparator(0, -1, false)
        }
        assertThrows(IllegalArgumentException::class.java) {
            chooser.appendComparator(0, 1, false)
        }
        assertEquals(listOf(0), chooser.sortKeys.map { it.column })
        assertSame(activeComparator, sorted.comparator)
    }

    @Test
    fun duplicateAppendDoesNotResortTheList() {
        val source = BasicEventList<Row>().apply {
            addAll(listOf(Row(first = 2, second = 1), Row(first = 1, second = 2)))
        }
        val sorted = SortedList(source, null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        var listEvents = 0
        sorted.addListEventListener { listEvents++ }
        chooser.appendComparator(0, 0, false)
        listEvents = 0

        chooser.appendComparator(0, 0, false)

        assertEquals(0, listEvents)
    }

    @Test
    fun clearingAnAlreadyUnsortedChooserDoesNotResortTheList() {
        val source = BasicEventList<Row>().apply {
            addAll(listOf(Row(first = 2, second = 1), Row(first = 1, second = 2)))
        }
        val sorted = SortedList(source, null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        var listEvents = 0
        sorted.addListEventListener { listEvents++ }

        chooser.clearComparator()

        assertEquals(0, listEvents)
    }

    @Test
    fun clearingAnEmptyChooserStillRemovesAForeignComparator() {
        val source = BasicEventList<Row>().apply {
            addAll(listOf(Row(first = 2, second = 1), Row(first = 1, second = 2)))
        }
        val sorted = SortedList(source, compareBy<Row> { it.first })
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        var listEvents = 0
        sorted.addListEventListener { listEvents++ }

        chooser.clearComparator()

        assertNull(sorted.comparator)
        assertEquals(1, listEvents)
    }

    @Test
    fun clearingAnEmptyChooserRemovesAComparatorInstalledAfterCreation() {
        val sorted = SortedList(BasicEventList<Row>(), null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        sorted.comparator = compareBy { it.first }

        chooser.clearComparator()

        assertNull(sorted.comparator)
    }

    @Test
    fun replacingWithNoSortKeysRemovesAComparatorInstalledAfterCreation() {
        val sorted = SortedList(BasicEventList<Row>(), null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        sorted.comparator = compareBy { it.first }

        assertTrue(chooser.setSortKeys(emptyList()))

        assertNull(sorted.comparator)
    }

    @Test
    fun replacingRecognizedSortKeysRemovesUnrecognizedTieBreakers() {
        val sorted = SortedList(BasicEventList<Row>(), null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        val sortKey = AbstractTableComparatorChooser.SortKey(0, 0, false)
        val foreignComparator = GlazedLists.chainComparators(
            listOf(chooser.comparatorForColumn(0), compareBy<Row> { it.second }),
        )
        sorted.comparator = foreignComparator
        chooser.detectComparator(foreignComparator)

        assertEquals(listOf(sortKey), chooser.sortKeys)
        assertTrue(chooser.setSortKeys(listOf(sortKey)))
        assertNotSame(foreignComparator, sorted.comparator)
    }

    @Test
    fun replacingSortKeysValidatesAllKeysBeforeChangingState() {
        val sorted = SortedList(BasicEventList<Row>(), null)
        val chooser = TestChooser(sorted, twoColumnRowFormat())
        chooser.appendComparator(0, 0, false)
        val activeComparator = sorted.comparator
        var listEvents = 0
        sorted.addListEventListener { listEvents++ }

        assertThrows(IllegalArgumentException::class.java) {
            chooser.setSortKeys(
                listOf(
                    AbstractTableComparatorChooser.SortKey(1, 0, false),
                    AbstractTableComparatorChooser.SortKey(-1, 0, false),
                ),
            )
        }

        assertEquals(listOf(0), chooser.sortKeys.map { it.column })
        assertSame(activeComparator, sorted.comparator)
        assertEquals(0, listEvents)
    }

    @Test
    fun replacingMultipleSortKeysRebuildsTheComparatorOnce() {
        val source = BasicEventList<Row>().apply {
            addAll(listOf(Row(first = 2, second = 2), Row(first = 1, second = 1)))
        }
        val sorted = SortedList(source, null)
        val chooser = TestChooser(sorted, twoColumnRowFormat(), AbstractTableComparatorChooser.MULTIPLE_COLUMN_MOUSE)
        var listEvents = 0
        sorted.addListEventListener { listEvents++ }

        chooser.setSortKeys(
            listOf(
                AbstractTableComparatorChooser.SortKey(0, 0, false),
                AbstractTableComparatorChooser.SortKey(1, 0, true),
            ),
        )

        assertEquals(listOf(0, 1), chooser.sortKeys.map { it.column })
        assertEquals(1, listEvents)
    }

    @Test
    fun replacingSortKeysHonorsSingleColumnStrategy() {
        val chooser = TestChooser(SortedList(BasicEventList(), null), twoColumnRowFormat())

        chooser.setSortKeys(
            listOf(
                AbstractTableComparatorChooser.SortKey(0, 0, false),
                AbstractTableComparatorChooser.SortKey(1, 0, true),
            ),
        )

        assertEquals(listOf(1), chooser.sortKeys.map { it.column })
        assertEquals(true, chooser.sortKeys.single().reverse)
    }

    @Test
    fun sortKeysAreReturnedAsAnImmutableSnapshot() {
        val chooser = TestChooser(SortedList(BasicEventList(), null), twoColumnRowFormat())
        chooser.appendComparator(0, 0, false)
        val snapshot = chooser.sortKeys

        chooser.clearComparator()

        assertEquals(listOf(AbstractTableComparatorChooser.SortKey(0, 0, false)), snapshot)
        assertThrows(UnsupportedOperationException::class.java) {
            (snapshot as MutableList).clear()
        }
    }

    @Test
    fun sortKeyKeepsJavaRecordStringRepresentation() {
        val sortKey = AbstractTableComparatorChooser.SortKey(1, 2, true)

        assertEquals("SortKey[column=1, comparatorIndex=2, reverse=true]", sortKey.toString())
    }

    private class TestChooser(
        sortedList: SortedList<Row>,
        tableFormat: TableFormat<Row>,
        sortingStrategy: SortingStrategy = SINGLE_COLUMN,
    ) : AbstractTableComparatorChooser<Row>(sortedList, tableFormat, sortingStrategy) {
        fun comparatorForColumn(column: Int): Comparator<Row> = sortingState.columns[column].comparators[0]

        fun detectComparator(comparator: Comparator<Row>) {
            redetectComparator(comparator)
        }

        fun replaceTableFormat(tableFormat: TableFormat<Row>) {
            setTableFormat(tableFormat)
        }
    }

    private data class Row(val first: Int, val second: Int)

    private companion object {
        private fun rowFormat(useSecond: Boolean) = object : TableFormat<Row> {
            override fun getColumnCount(): Int = 1

            override fun getColumnName(column: Int): String = "Value"

            override fun getColumnValue(baseObject: Row, column: Int): Any =
                if (useSecond) baseObject.second else baseObject.first
        }

        private fun twoColumnRowFormat() = object : TableFormat<Row> {
            override fun getColumnCount(): Int = 2

            override fun getColumnName(column: Int): String = "Value $column"

            override fun getColumnValue(baseObject: Row, column: Int): Any =
                if (column == 0) baseObject.first else baseObject.second
        }
    }
}
