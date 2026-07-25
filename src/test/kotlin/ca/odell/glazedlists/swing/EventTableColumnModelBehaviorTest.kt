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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransformedList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.DefaultListSelectionModel
import javax.swing.SwingUtilities
import javax.swing.event.ChangeEvent
import javax.swing.event.ListSelectionEvent
import javax.swing.event.TableColumnModelEvent
import javax.swing.event.TableColumnModelListener
import javax.swing.table.TableColumn

internal class EventTableColumnModelBehaviorTest {
    @Test
    fun constructorDefaultsEnumerationAndLookupsRemainExact() = onEdt {
        val first = column("first", 40)
        val second = column("second", 60)
        val source = BasicEventList<TableColumn>().apply { addAll(listOf(first, second)) }
        val firstListenerCount = first.propertyChangeListeners.size
        val secondListenerCount = second.propertyChangeListeners.size
        val model = InspectableColumnModel(source)
        try {
            assertEquals(2, model.columnCount)
            assertEquals(1, model.columnMargin)
            assertFalse(model.columnSelectionAllowed)
            assertEquals(100, model.totalColumnWidth)
            assertSame(first, model.getColumn(0))
            assertSame(second, model.getColumn(1))
            assertEquals(listOf(first, second), model.columns.asList())
            assertEquals(0, model.getColumnIndex("first"))
            assertEquals(1, model.getColumnIndex("second"))
            assertEquals(0, model.getColumnIndexAtX(0))
            assertEquals(0, model.getColumnIndexAtX(39))
            assertEquals(1, model.getColumnIndexAtX(40))
            assertEquals(1, model.getColumnIndexAtX(99))
            assertEquals(-1, model.getColumnIndexAtX(100))
            assertEquals(-1, model.getColumnIndexAtX(-1))
            assertEquals(firstListenerCount + 1, first.propertyChangeListeners.size)
            assertEquals(secondListenerCount + 1, second.propertyChangeListeners.size)
            assertEquals("Identifier not found", assertThrows(IllegalArgumentException::class.java) {
                model.getColumnIndex("missing")
            }.message)
        } finally {
            model.dispose()
        }
        assertEquals(firstListenerCount, first.propertyChangeListeners.size)
        assertEquals(secondListenerCount, second.propertyChangeListeners.size)
    }

    @Test
    fun sourceInsertUpdateAndDeletePreserveListenerLifecycleAndEventIndices() = onEdt {
        val first = column("first")
        val second = column("second")
        val source = BasicEventList<TableColumn>().apply { addAll(listOf(first, second)) }
        val model = EventTableColumnModel(source)
        val events = EventRecorder().also(model::addColumnModelListener)
        try {
            val inserted = column("inserted")
            val insertedListeners = inserted.propertyChangeListeners.size
            source.add(1, inserted)
            assertEquals(listOf(first, inserted, second), model.columns.asList())
            assertEquals(insertedListeners + 1, inserted.propertyChangeListeners.size)
            assertEquals(listOf("added:0:2"), events.columnEvents)

            events.clear()
            val replacement = column("replacement")
            val replacementListeners = replacement.propertyChangeListeners.size
            source[1] = replacement
            assertEquals(insertedListeners, inserted.propertyChangeListeners.size)
            assertEquals(replacementListeners + 1, replacement.propertyChangeListeners.size)
            assertEquals(listOf("moved:1:1"), events.columnEvents)

            events.clear()
            source.removeAt(1)
            assertEquals(replacementListeners, replacement.propertyChangeListeners.size)
            assertEquals(listOf("removed:1:0"), events.columnEvents)
        } finally {
            model.dispose()
        }
    }

    @Test
    fun updateTransfersListenersByReferenceIdentityRatherThanStructuralEquality() = onEdt {
        val original = EqualTableColumn("original")
        val replacement = EqualTableColumn("replacement")
        assertEquals(original, replacement)
        assertNotSame(original, replacement)
        val originalListenerCount = original.propertyChangeListeners.size
        val replacementListenerCount = replacement.propertyChangeListeners.size
        val source = BasicEventList<TableColumn>().apply { add(original) }
        val model = EventTableColumnModel(source)
        val events = EventRecorder().also(model::addColumnModelListener)
        try {
            source[0] = replacement
            assertEquals(originalListenerCount, original.propertyChangeListeners.size)
            assertEquals(replacementListenerCount + 1, replacement.propertyChangeListeners.size)
            assertEquals(listOf("moved:0:0"), events.columnEvents)

            events.clear()
            source[0] = replacement
            assertEquals(replacementListenerCount + 1, replacement.propertyChangeListeners.size)
            assertEquals(listOf("moved:0:0"), events.columnEvents)
        } finally {
            model.dispose()
        }
    }

    @Test
    fun modelMutationsWriteThroughAndMovingASelectedColumnPreservesSelection() = onEdt {
        val first = column("first")
        val second = column("second")
        val third = column("third")
        val source = BasicEventList<TableColumn>().apply { addAll(listOf(first, second)) }
        val model = EventTableColumnModel(source)
        try {
            model.addColumn(third)
            assertEquals(listOf(first, second, third), source.toList())
            model.removeColumn(second)
            assertEquals(listOf(first, third), source.toList())

            model.selectionModel.addSelectionInterval(0, 0)
            model.moveColumn(0, 1)
            assertEquals(listOf(third, first), source.toList())
            assertArrayEquals(intArrayOf(1), model.selectedColumns)

            val recorder = EventRecorder().also(model::addColumnModelListener)
            model.moveColumn(1, 1)
            assertEquals(listOf("moved:1:1"), recorder.columnEvents)

            assertEquals("columnIndex out of range", assertThrows(IllegalArgumentException::class.java) {
                model.moveColumn(-1, 0)
            }.message)
            assertEquals("newIndex out of range", assertThrows(IllegalArgumentException::class.java) {
                model.moveColumn(0, 2)
            }.message)
        } finally {
            model.dispose()
        }
    }

    @Test
    fun widthAndSelectionChangesUseTheCurrentModelsAndCachedWidth() = onEdt {
        val first = column("first", 40)
        val second = column("second", 60)
        val model = EventTableColumnModel(BasicEventList<TableColumn>().apply { addAll(listOf(first, second)) })
        val events = EventRecorder().also(model::addColumnModelListener)
        try {
            assertEquals(100, model.totalColumnWidth)
            first.width = 50
            assertEquals(110, model.totalColumnWidth)
            assertEquals(1, events.marginChanges)

            model.columnMargin = 3
            model.columnMargin = 3
            assertEquals(3, model.columnMargin)
            assertEquals(2, events.marginChanges)

            model.columnSelectionAllowed = true
            val oldSelection = model.selectionModel
            oldSelection.addSelectionInterval(0, 0)
            assertArrayEquals(intArrayOf(0), model.selectedColumns)
            assertEquals(1, model.selectedColumnCount)
            assertTrue(events.selectionChanges > 0)

            val replacement = DefaultListSelectionModel()
            model.selectionModel = replacement
            events.selectionChanges = 0
            oldSelection.addSelectionInterval(1, 1)
            assertEquals(0, events.selectionChanges)
            replacement.addSelectionInterval(1, 1)
            assertEquals(1, events.selectionChanges)
            assertArrayEquals(intArrayOf(1), model.selectedColumns)
        } finally {
            model.dispose()
        }
    }

    @Test
    fun proxyOwnershipAndDisposalRemainDistinct() = onEdt {
        val ownedSource = BasicEventList<TableColumn>().apply { add(column("owned")) }
        val ownedModel = InspectableColumnModel(ownedSource)
        val ownedProxy = ownedModel.exposedSource()
        assertNotSame(ownedSource, ownedProxy)
        ownedModel.dispose()
        assertNull(ownedModel.exposedSourceOrNull())
        ownedSource.add(column("after-dispose"))
        assertEquals(1, ownedProxy.size)
        assertThrows(NullPointerException::class.java) { ownedModel.columnCount }

        val externalSource = BasicEventList<TableColumn>().apply { add(column("external")) }
        val externalProxy = externalSource.swingThreadProxyList()
        val externalModel = InspectableColumnModel(externalProxy)
        assertSame(externalProxy, externalModel.exposedSource())
        externalModel.dispose()
        externalSource.add(column("still-live"))
        assertEquals(2, externalProxy.size)
        externalProxy.dispose()
    }

    @Test
    fun columnsEnumerationRemainsLiveAfterItIsCreated() = onEdt {
        val first = column("first")
        val second = column("second")
        val source = BasicEventList<TableColumn>().apply { add(first) }
        val model = EventTableColumnModel(source)
        try {
            val columns = model.columns
            source.add(second)
            assertEquals(listOf(first, second), columns.asList())
        } finally {
            model.dispose()
        }
    }

    @Test
    fun selectionForwardingSurvivesDisposalAndDoubleDisposalFails() = onEdt {
        val model = EventTableColumnModel(
            BasicEventList<TableColumn>().apply { addAll(listOf(column("first"), column("second"))) },
        )
        val events = EventRecorder().also(model::addColumnModelListener)
        val selectionModel = model.selectionModel

        model.dispose()
        selectionModel.addSelectionInterval(0, 0)
        assertEquals(1, events.selectionChanges)
        assertThrows(NullPointerException::class.java) { model.dispose() }
    }

    private fun column(identifier: Any, width: Int = 75): TableColumn =
        TableColumn().apply {
            this.identifier = identifier
            this.width = width
        }

    private fun onEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) action() else SwingUtilities.invokeAndWait(action)
    }

    private fun <E> java.util.Enumeration<E>.asList(): List<E> = buildList {
        while (hasMoreElements()) add(nextElement())
    }

    private class EqualTableColumn(identifier: Any) : TableColumn() {
        init {
            this.identifier = identifier
        }

        override fun equals(other: Any?): Boolean = other is EqualTableColumn

        override fun hashCode(): Int = EqualTableColumn::class.java.hashCode()
    }

    private class InspectableColumnModel(source: EventList<TableColumn>) : EventTableColumnModel<TableColumn>(source) {
        fun exposedSource(): TransformedList<TableColumn, TableColumn> = swingThreadSource
        fun exposedSourceOrNull(): TransformedList<TableColumn, TableColumn>? = swingThreadSource
    }

    private class EventRecorder : TableColumnModelListener {
        val columnEvents = mutableListOf<String>()
        var marginChanges = 0
        var selectionChanges = 0

        override fun columnAdded(event: TableColumnModelEvent) {
            columnEvents += "added:${event.fromIndex}:${event.toIndex}"
        }

        override fun columnRemoved(event: TableColumnModelEvent) {
            columnEvents += "removed:${event.fromIndex}:${event.toIndex}"
        }

        override fun columnMoved(event: TableColumnModelEvent) {
            columnEvents += "moved:${event.fromIndex}:${event.toIndex}"
        }

        override fun columnMarginChanged(event: ChangeEvent) {
            marginChanges++
        }

        override fun columnSelectionChanged(event: ListSelectionEvent) {
            selectionChanges++
        }

        fun clear() {
            columnEvents.clear()
            marginChanges = 0
            selectionChanges = 0
        }
    }
}
