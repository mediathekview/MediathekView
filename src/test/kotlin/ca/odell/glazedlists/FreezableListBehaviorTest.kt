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
package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class FreezableListBehaviorTest {
    @Test
    fun freezeSnapshotsTheSourceUntilThawReconnectsIt() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B")) }
        val freezable = FreezableList(source)
        val thawChanges = mutableListOf<Change>()
        freezable.addListEventListener { event ->
            while (event.next()) {
                thawChanges += Change(event.type, event.index)
            }
        }

        source += "C"
        assertEquals(listOf("A", "B", "C"), freezable.toList())

        freezable.freeze()
        thawChanges.clear()
        source[0] = "updated"
        source += "D"

        assertTrue(freezable.isFrozen())
        assertEquals(listOf("A", "B", "C"), freezable.toList())
        assertTrue(thawChanges.isEmpty())

        freezable.thaw()

        assertFalse(freezable.isFrozen())
        assertEquals(listOf("updated", "B", "C", "D"), freezable.toList())
        assertEquals(
            listOf(
                Change(ListEvent.INSERT, 0),
                Change(ListEvent.INSERT, 1),
                Change(ListEvent.INSERT, 2),
                Change(ListEvent.INSERT, 3),
                Change(ListEvent.DELETE, 4),
                Change(ListEvent.DELETE, 4),
                Change(ListEvent.DELETE, 4),
            ),
            thawChanges,
        )

        source += "E"
        assertEquals(listOf("updated", "B", "C", "D", "E"), freezable.toList())
    }

    @Test
    fun invalidStateTransitionsAndFrozenWritesAreRejected() {
        val freezable = FreezableList(BasicEventList<String>().apply { add("value") })

        val thawFailure = assertThrows(IllegalStateException::class.java, freezable::thaw)
        assertEquals("Cannot thaw a list that is not frozen", thawFailure.message)

        freezable.freeze()
        val freezeFailure = assertThrows(IllegalStateException::class.java, freezable::freeze)
        assertEquals("Cannot freeze a list that is already frozen", freezeFailure.message)

        val writeFailure = assertThrows(IllegalStateException::class.java) { freezable += "other" }
        assertEquals("Non-writable List cannot be modified", writeFailure.message)
    }

    private data class Change(val type: Int, val index: Int)
}
