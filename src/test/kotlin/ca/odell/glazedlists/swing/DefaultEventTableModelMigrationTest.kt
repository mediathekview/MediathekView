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
import ca.odell.glazedlists.TransactionList
import ca.odell.glazedlists.gui.AdvancedTableFormat
import ca.odell.glazedlists.gui.WritableTableFormat
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Modifier
import javax.swing.SwingUtilities
import javax.swing.event.TableModelEvent

internal class DefaultEventTableModelMigrationTest {
    @Test
    fun defaultAdapterPublishesPreciseTableEvents() {
        val source = BasicEventList<Row>()
        val model = DefaultEventTableModel(source, TableFormat)
        val events = mutableListOf<EventSnapshot>()
        model.addTableModelListener { events += EventSnapshot.from(it) }

        SwingUtilities.invokeAndWait {
            source += Row("first")
            source[0] = Row("updated")
            source.removeAt(0)
        }

        assertEquals(
            listOf(
                EventSnapshot(TableModelEvent.INSERT, 0, 0),
                EventSnapshot(TableModelEvent.UPDATE, 0, 0),
                EventSnapshot(TableModelEvent.DELETE, 0, 0),
            ),
            events,
        )
        model.dispose()
    }

    @Test
    fun manyToOneAdapterCollapsesMultipleBlocksToOneWholeDataEvent() {
        val source = BasicEventList<Row>().apply {
            addAll(listOf(Row("first"), Row("second"), Row("third")))
        }
        val transaction = TransactionList(source)
        val model = DefaultEventTableModel(transaction, TableFormat)
        model.eventAdapter = GlazedListsSwing.manyToOneEventAdapterFactory<Row>().create(model)
        val events = mutableListOf<EventSnapshot>()
        model.addTableModelListener { events += EventSnapshot.from(it) }

        SwingUtilities.invokeAndWait {
            transaction.withTransaction {
                this[0] = Row("updated first")
                this[2] = Row("updated third")
            }
        }

        assertEquals(
            listOf(EventSnapshot(TableModelEvent.UPDATE, 0, Int.MAX_VALUE)),
            events,
        )
        model.dispose()
        transaction.dispose()
    }

    @Test
    fun defaultAdapterRejectsEventsArrivingOffTheEdt() {
        val source = BasicEventList<Row>()
        val model = DefaultEventTableModel(source, TableFormat)

        val failure = assertThrows(IllegalStateException::class.java) {
            source += Row("wrong thread")
        }

        assertEquals(
            "Events to DefaultEventTableModel must arrive on the EDT - " +
                "consider adding source.swingThreadProxyList() somewhere in your list pipeline",
            failure.message,
        )
        model.dispose()
    }

    @Test
    fun tableFormatMetadataEditingAndReplacementRemainDelegated() {
        val original = Row("before")
        val source = BasicEventList<Row>().apply { add(original) }
        val model = DefaultEventTableModel(source, TableFormat)
        val events = mutableListOf<EventSnapshot>()
        model.addTableModelListener { events += EventSnapshot.from(it) }

        assertSame(TableFormat, model.tableFormat)
        assertSame(original, model.getElementAt(0))
        assertEquals("value", model.getColumnName(0))
        assertEquals(String::class.java, model.getColumnClass(0))
        assertEquals("before", model.getValueAt(0, 0))
        assertEquals(1, model.rowCount)
        assertEquals(1, model.columnCount)
        assertEquals(true, model.isCellEditable(0, 0))

        SwingUtilities.invokeAndWait {
            model.setValueAt("after", 0, 0)
        }

        assertEquals(Row("after"), source[0])
        assertEquals(listOf(EventSnapshot(TableModelEvent.UPDATE, 0, 0)), events)
        model.dispose()
    }

    @Test
    fun replacingTheTableFormatFiresAStructureChangeAndKeepsTheAdapterProperty() {
        val source = BasicEventList<Row>()
        val model = DefaultEventTableModel(source, TableFormat)
        val adapter = GlazedListsSwing.defaultEventAdapterFactory<Row>().create(model)
        val events = mutableListOf<EventSnapshot>()
        model.addTableModelListener { events += EventSnapshot.from(it) }

        model.eventAdapter = adapter
        model.tableFormat = TableFormat

        assertSame(adapter, model.eventAdapter)
        assertEquals(
            listOf(EventSnapshot(TableModelEvent.UPDATE, TableModelEvent.HEADER_ROW, TableModelEvent.HEADER_ROW)),
            events,
        )
        model.dispose()
    }

    @Test
    fun adapterImplementationsRemainNonPublicAndFactoriesRemainSingletons() {
        val defaultAdapterClass = Class.forName(
            "ca.odell.glazedlists.impl.swing.DefaultTableModelEventAdapter",
        )
        val manyToOneAdapterClass = Class.forName(
            "ca.odell.glazedlists.impl.swing.ManyToOneTableModelEventAdapter",
        )

        assertFalse(Modifier.isPublic(defaultAdapterClass.modifiers))
        assertFalse(Modifier.isPublic(manyToOneAdapterClass.modifiers))
        assertSame(
            GlazedListsSwing.defaultEventAdapterFactory<Row>() as Any,
            GlazedListsSwing.defaultEventAdapterFactory<String>() as Any,
        )
        assertSame(
            GlazedListsSwing.manyToOneEventAdapterFactory<Row>() as Any,
            GlazedListsSwing.manyToOneEventAdapterFactory<String>() as Any,
        )
    }

    private data class Row(val value: String)

    private object TableFormat : WritableTableFormat<Row>, AdvancedTableFormat<Row> {
        override fun getColumnCount(): Int = 1

        override fun getColumnName(column: Int): String = "value"

        override fun getColumnValue(baseObject: Row, column: Int): Any = baseObject.value

        override fun isEditable(baseObject: Row, column: Int): Boolean = true

        override fun setColumnValue(baseObject: Row, editedValue: Any?, column: Int): Row =
            Row(editedValue as String)

        override fun getColumnClass(column: Int): Class<*> = String::class.java

        override fun getColumnComparator(column: Int): Comparator<*> = Comparator.naturalOrder<String>()
    }

    private data class EventSnapshot(
        val type: Int,
        val firstRow: Int,
        val lastRow: Int,
    ) {
        companion object {
            fun from(event: TableModelEvent): EventSnapshot =
                EventSnapshot(event.type, event.firstRow, event.lastRow)
        }
    }
}
