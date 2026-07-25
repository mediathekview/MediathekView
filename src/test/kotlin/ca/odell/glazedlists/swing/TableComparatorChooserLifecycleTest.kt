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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.sort.TableColumnComparator
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.JTable
import javax.swing.SwingUtilities
import javax.swing.table.AbstractTableModel
import javax.swing.table.DefaultTableModel

internal class TableComparatorChooserLifecycleTest {
    @Test
    fun replacingPlainTableModelTransfersTheChooserListener() {
        SwingUtilities.invokeAndWait {
            val oldModel = DefaultTableModel(0, 1)
            val newModel = DefaultTableModel(0, 1)
            val table = JTable(oldModel)
            val chooser = TableComparatorChooser.install(
                table,
                SortedList(BasicEventList(), null),
                AbstractTableComparatorChooser.SINGLE_COLUMN,
                rowFormat,
            )

            try {
                assertEquals(1, oldModel.chooserListenerCount())

                table.model = newModel

                assertEquals(0, oldModel.chooserListenerCount())
                assertEquals(1, newModel.chooserListenerCount())
            } finally {
                chooser.dispose()
            }

            assertEquals(0, newModel.chooserListenerCount())
        }
    }

    @Test
    fun installationDetectsAnExistingTableColumnComparator() {
        SwingUtilities.invokeAndWait {
            val source = BasicEventList<Row>().apply {
                addAll(listOf(Row(2), Row(1)))
            }
            val comparator = TableColumnComparator(rowFormat, 0, GlazedLists.comparableComparator<Int>())
            val sorted = SortedList(source, comparator)
            val chooser = TableComparatorChooser.install(
                JTable(0, 1),
                sorted,
                AbstractTableComparatorChooser.SINGLE_COLUMN,
                rowFormat,
            )

            try {
                assertEquals(listOf(0), chooser.sortKeys.map { it.column })
            } finally {
                chooser.dispose()
            }
        }
    }

    @Test
    fun disposalIsIdempotentWhenTheHeaderHasNoDefaultRenderer() {
        SwingUtilities.invokeAndWait {
            val table = JTable(0, 1)
            table.tableHeader.defaultRenderer = null
            val chooser = TableComparatorChooser.install(
                table,
                SortedList(BasicEventList(), null),
                AbstractTableComparatorChooser.SINGLE_COLUMN,
                rowFormat,
            )

            assertDoesNotThrow {
                chooser.dispose()
                chooser.dispose()
            }
        }
    }

    @Test
    fun replacingAdvancedTableModelClearsTheActiveComparator() {
        SwingUtilities.invokeAndWait {
            val table = JTable(TestAdvancedTableModel(rowFormat))
            val sorted = SortedList(BasicEventList<Row>().apply { addAll(listOf(Row(2), Row(1))) }, null)
            val chooser = TableComparatorChooser.install(
                table,
                sorted,
                AbstractTableComparatorChooser.SINGLE_COLUMN,
            )

            try {
                chooser.appendComparator(0, 0, false)

                table.model = TestAdvancedTableModel(rowFormat)

                assertEquals(emptyList<AbstractTableComparatorChooser.SortKey>(), chooser.sortKeys)
                assertNull(sorted.comparator)
            } finally {
                chooser.dispose()
            }
        }
    }

    @Test
    fun advancedTableModelStructureChangeClearsTheActiveComparator() {
        SwingUtilities.invokeAndWait {
            val model: AdvancedTableModel<Row> = TestAdvancedTableModel(rowFormat)
            val table = JTable(model)
            val sorted = SortedList(BasicEventList<Row>().apply { addAll(listOf(Row(2), Row(1))) }, null)
            val chooser = TableComparatorChooser.install(
                table,
                sorted,
                AbstractTableComparatorChooser.SINGLE_COLUMN,
            )

            try {
                chooser.appendComparator(0, 0, false)

                model.tableFormat = rowFormat

                assertEquals(emptyList<AbstractTableComparatorChooser.SortKey>(), chooser.sortKeys)
                assertNull(sorted.comparator)
            } finally {
                chooser.dispose()
            }
        }
    }

    private fun AbstractTableModel.chooserListenerCount() =
        tableModelListeners.count { it.javaClass.enclosingClass == TableComparatorChooser::class.java }

    private data class Row(val value: Int)

    private class TestAdvancedTableModel(
        private var currentTableFormat: TableFormat<in Row>,
    ) : AbstractTableModel(), AdvancedTableModel<Row> {
        override fun getRowCount() = 0

        override fun getColumnCount() = currentTableFormat.getColumnCount()

        override fun getValueAt(rowIndex: Int, columnIndex: Int): Any? = null

        override var tableFormat: TableFormat<in Row>
            get() = currentTableFormat
            set(value) {
                currentTableFormat = value
                fireTableStructureChanged()
            }

        override fun getElementAt(index: Int): Row = error("model contains no rows")

        override fun dispose() = Unit
    }

    private companion object {
        private val rowFormat = object : TableFormat<Row> {
            override fun getColumnCount(): Int = 1

            override fun getColumnName(column: Int): String = "Value"

            override fun getColumnValue(baseObject: Row, column: Int): Any = baseObject.value
        }
    }
}
