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
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.SwingUtilities
import javax.swing.event.ListDataEvent
import javax.swing.event.ListDataListener

internal class DefaultEventModelsTest {
    @Test
    fun singleListChangesUsePreciseSwingEvents() {
        val source = BasicEventList<String>()
        val model = DefaultEventListModel(source)
        val events = mutableListOf<EventSnapshot>()
        model.addListDataListener(CapturingListDataListener(events))

        SwingUtilities.invokeAndWait {
            source += "first"
            source[0] = "updated"
            source.removeAt(0)
        }

        assertEquals(
            listOf(
                EventSnapshot(ListDataEvent.INTERVAL_ADDED, 0, 0),
                EventSnapshot(ListDataEvent.CONTENTS_CHANGED, 0, 0),
                EventSnapshot(ListDataEvent.INTERVAL_REMOVED, 0, 0),
            ),
            events,
        )
        model.dispose()
    }

    @Test
    fun multipleListEventBlocksFallBackToAWholeDataChange() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second", "third")) }
        val transaction = TransactionList(source)
        val model = DefaultEventListModel(transaction)
        val events = mutableListOf<EventSnapshot>()
        model.addListDataListener(CapturingListDataListener(events))

        SwingUtilities.invokeAndWait {
            transaction.withTransaction {
                this[0] = "updated first"
                this[2] = "updated third"
            }
        }

        assertEquals(
            listOf(EventSnapshot(ListDataEvent.CONTENTS_CHANGED, 0, Int.MAX_VALUE)),
            events,
        )
        model.dispose()
        transaction.dispose()
    }

    @Test
    fun listChangesArrivingOffTheEdtAreRejected() {
        val source = BasicEventList<String>()
        val model = DefaultEventListModel(source)

        val failure = assertThrows(IllegalStateException::class.java) {
            source += "wrong thread"
        }

        assertEquals(
            "Events to DefaultEventListModel must arrive on the EDT - " +
                "consider adding source.swingThreadProxyList() somewhere in your list pipeline",
            failure.message,
        )
        model.dispose()
    }

    @Test
    fun comboBoxSelectionUsesReferenceIdentityAndReusableChangeEvents() {
        val source = BasicEventList<String>()
        val model = DefaultEventComboBoxModel(source)
        val events = mutableListOf<EventSnapshot>()
        model.addListDataListener(CapturingListDataListener(events))
        val first = String(charArrayOf('v', 'a', 'l', 'u', 'e'))
        val equalButDistinct = String(charArrayOf('v', 'a', 'l', 'u', 'e'))

        model.selectedItem = first
        assertSame(first, model.selectedItem)
        model.selectedItem = first
        model.selectedItem = equalButDistinct
        assertSame(equalButDistinct, model.selectedItem)
        model.selectedItem = null

        assertEquals(
            List(3) { EventSnapshot(ListDataEvent.CONTENTS_CHANGED, -1, -1) },
            events,
        )
        model.dispose()
    }

    private data class EventSnapshot(
        val type: Int,
        val index0: Int,
        val index1: Int,
    )

    private class CapturingListDataListener(
        private val events: MutableList<EventSnapshot>,
    ) : ListDataListener {
        override fun intervalAdded(event: ListDataEvent) = capture(event)

        override fun intervalRemoved(event: ListDataEvent) = capture(event)

        override fun contentsChanged(event: ListDataEvent) = capture(event)

        private fun capture(event: ListDataEvent) {
            events += EventSnapshot(event.type, event.index0, event.index1)
        }
    }
}
