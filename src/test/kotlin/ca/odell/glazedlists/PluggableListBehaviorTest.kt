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
import java.util.concurrent.locks.ReentrantReadWriteLock

internal class PluggableListBehaviorTest {
    @Test
    fun replacingTheSourceRebuildsTheViewAndForwardsOnlyTheActiveSource() {
        val original = BasicEventList<String>().apply { addAll(listOf("old-1", "old-2")) }
        val pluggable = PluggableList(original)
        val replacement = pluggable.createSourceList().apply { addAll(listOf("new-1", "new-2")) }
        val changes = mutableListOf<Change>()
        pluggable.addListEventListener { event ->
            while (event.next()) changes += Change(event.type, event.index)
        }

        pluggable.setSource(replacement)

        assertEquals(listOf("new-1", "new-2"), pluggable.toList())
        assertEquals(
            listOf(
                Change(ListEvent.DELETE, 0),
                Change(ListEvent.DELETE, 0),
                Change(ListEvent.INSERT, 0),
                Change(ListEvent.INSERT, 1),
            ),
            changes,
        )

        changes.clear()
        original += "ignored"
        assertEquals(emptyList<Change>(), changes)
        assertEquals(listOf("new-1", "new-2"), pluggable.toList())

        replacement += "forwarded"
        assertEquals(listOf(Change(ListEvent.INSERT, 2)), changes)
        assertEquals(listOf("new-1", "new-2", "forwarded"), pluggable.toList())

        pluggable += "written-through"
        assertEquals(listOf("new-1", "new-2", "forwarded", "written-through"), replacement.toList())
    }

    @Test
    fun createdSourcesShareInfrastructureAndRepeatedSourceIsANoOp() {
        val publisher = BasicEventList<String>().publisher
        val lock = ReentrantReadWriteLock()
        val pluggable = PluggableList<String>(publisher, lock)
        val replacement = pluggable.createSourceList()
        var eventCount = 0
        pluggable.addListEventListener { eventCount++ }

        assertSame(publisher, replacement.publisher)
        assertSame(lock, replacement.readWriteLock)

        pluggable.setSource(replacement)
        eventCount = 0
        pluggable.setSource(replacement)

        assertEquals(0, eventCount)
    }

    @Test
    fun replacementRequiresTheSameLockAndPublisher() {
        val publisher = BasicEventList<String>().publisher
        val lock = ReentrantReadWriteLock()
        val pluggable = PluggableList<String>(publisher, lock)

        val lockFailure = assertThrows(IllegalArgumentException::class.java) {
            pluggable.setSource(BasicEventList(publisher, ReentrantReadWriteLock()))
        }
        assertEquals("source list must share lock with PluggableList", lockFailure.message)

        val publisherFailure = assertThrows(IllegalArgumentException::class.java) {
            pluggable.setSource(BasicEventList(BasicEventList<String>().publisher, lock))
        }
        assertEquals("source list must share publisher with PluggableList", publisherFailure.message)
    }

    private data class Change(val type: Int, val index: Int)
}
