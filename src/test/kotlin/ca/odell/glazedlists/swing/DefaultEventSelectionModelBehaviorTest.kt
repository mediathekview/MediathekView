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
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.ListSelectionModel
import javax.swing.event.ListSelectionEvent

internal class DefaultEventSelectionModelBehaviorTest {
    @Test
    fun disabledModelIgnoresProgrammaticSelectionChanges() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second")) }
        val model = DefaultEventSelectionModel(source)
        model.enabled = false

        model.setSelectionInterval(0, 1)
        model.addSelectionInterval(0, 1)
        model.setAnchorSelectionIndex(1)
        model.setLeadSelectionIndex(1)
        model.clearSelection()

        assertTrue(model.isSelectionEmpty)
        assertEquals(-1, model.anchorSelectionIndex)
        assertEquals(-1, model.leadSelectionIndex)
    }

    @Test
    fun adjustingChangesProduceIndividualEventsAndOneCombinedFinalEvent() {
        val source = BasicEventList<String>().apply { addAll(listOf("zero", "one", "two")) }
        val model = DefaultEventSelectionModel(source)
        val events = mutableListOf<ListSelectionEvent>()
        model.addListSelectionListener(events::add)

        model.valueIsAdjusting = true
        model.addSelectionInterval(0, 0)
        model.addSelectionInterval(2, 2)
        model.valueIsAdjusting = false

        assertTrue(events.dropLast(1).all(ListSelectionEvent::getValueIsAdjusting))
        val finalEvent = events.last()
        assertFalse(finalEvent.valueIsAdjusting)
        assertEquals(0, finalEvent.firstIndex)
        assertEquals(2, finalEvent.lastIndex)
        assertEquals(listOf("zero", "two"), model.selected.toList())
    }

    @Test
    fun selectionTracksSourceInsertionsWithoutSelectingTheNewElementInDefensiveMode() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "selected")) }
        val model = DefaultEventSelectionModel(source)
        model.selectionMode = ca.odell.glazedlists.ListSelection.MULTIPLE_INTERVAL_SELECTION_DEFENSIVE
        model.setSelectionInterval(1, 1)

        source.add(1, "inserted")

        assertEquals(listOf("selected"), model.selected.toList())
        assertFalse(model.isSelectedIndex(1))
        assertTrue(model.isSelectedIndex(2))
    }

    @Test
    fun emptySwingClearPathAndListenerRemovalRemainSafe() {
        val source = BasicEventList<String>()
        val model = DefaultEventSelectionModel(source)
        var events = 0
        val listener = javax.swing.event.ListSelectionListener { events++ }
        model.addListSelectionListener(listener)

        assertDoesNotThrow { model.removeSelectionInterval(0, 0) }
        source.add("value")
        model.setSelectionInterval(0, 0)
        events = 0
        model.removeListSelectionListener(listener)
        model.selectionMode = ListSelectionModel.SINGLE_SELECTION
        model.clearSelection()

        assertEquals(0, events)
    }

    @Test
    fun selectionViewsAreStableAndRetainTheirMutationRoles() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second", "third")) }
        val model = DefaultEventSelectionModel(source)
        model.setSelectionInterval(0, 0)

        val selected = model.selected
        val togglingSelected = model.togglingSelected
        val deselected = model.deselected
        val togglingDeselected = model.togglingDeselected

        assertSame(selected, model.selected)
        assertSame(togglingSelected, model.togglingSelected)
        assertSame(deselected, model.deselected)
        assertSame(togglingDeselected, model.togglingDeselected)

        togglingSelected.add("second")
        assertEquals(listOf("first", "second"), selected.toList())
        togglingSelected.remove("first")
        assertEquals(listOf("second"), selected.toList())

        togglingDeselected.add("second")
        assertTrue(selected.isEmpty())
        togglingDeselected.remove("third")
        assertEquals(listOf("third"), selected.toList())

        selected.remove("third")
        assertEquals(listOf("first", "second"), source.toList())
        deselected.remove("first")
        assertEquals(listOf("second"), source.toList())
    }

    @Test
    fun disabledStateDoesNotSuppressInversionOrSourceDrivenSelectionShifts() {
        val source = BasicEventList<String>().apply { addAll(listOf("first", "second")) }
        val model = DefaultEventSelectionModel(source)
        model.setSelectionInterval(0, 0)
        model.enabled = false

        model.invertSelection()
        assertEquals(listOf("second"), model.selected.toList())

        source.add(0, "inserted")
        assertEquals(listOf("second"), model.selected.toList())
        assertTrue(model.isSelectedIndex(2))
    }

    @Test
    fun listenersReceiveTheSameEventInstance() {
        val source = BasicEventList<String>().apply { add("value") }
        val model = DefaultEventSelectionModel(source)
        val events = mutableListOf<ListSelectionEvent>()
        model.addListSelectionListener(events::add)
        model.addListSelectionListener(events::add)

        model.setSelectionInterval(0, 0)

        assertEquals(2, events.size)
        assertSame(events[0], events[1])
        assertSame(model, events[0].source)
    }
}
