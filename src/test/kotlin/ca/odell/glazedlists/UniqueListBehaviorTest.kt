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

internal class UniqueListBehaviorTest {
    @Test
    fun naturalFactoryCollapsesDuplicatesAndExposesCountsAndCopies() {
        val source = BasicEventList<String>().apply { addAll(listOf("b", "a", "b", "c", "a")) }
        val unique = UniqueList.create(source)

        assertEquals(listOf("a", "b", "c"), unique)
        assertEquals(2, unique.getCount(0))
        assertEquals(2, unique.getCount("b"))
        assertEquals(0, unique.getCount("missing"))
        assertEquals(listOf("a", "a"), unique.getAll(0))

        val copy = unique.getAll("b")
        copy.clear()
        assertEquals(listOf("a", "b", "c"), unique)
        assertEquals(listOf("b", "b"), unique.getAll("b"))

        val missing = unique.getAll("missing")
        assertTrue(missing.isEmpty())
        assertThrows(UnsupportedOperationException::class.java) { missing.add("unexpected") }
    }

    @Test
    fun comparatorEqualityDefinesUniquenessAndIndexLookup() {
        val first = Row(1, "first")
        val second = Row(1, "second")
        val third = Row(2, "third")
        val source = BasicEventList<Row>().apply { addAll(listOf(third, first, second)) }
        val unique = UniqueList(source, compareBy(Row::key))

        assertEquals(listOf(first, third), unique)
        assertEquals(0, unique.indexOf(Row(1, "missing")))
        assertEquals(2, unique.getCount(Row(1, "missing")))
        assertEquals(listOf(first, second), unique.getAll(Row(1, "missing")))
    }

    @Test
    fun comparatorReplacementRebuildsGroupsAndNullRestoresNaturalOrder() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "a", "B")) }
        val unique = UniqueList(source, String.CASE_INSENSITIVE_ORDER)
        val events = unique.recordChanges()

        assertEquals(listOf("A", "B"), unique)

        unique.setComparator(naturalOrder())
        assertEquals(listOf("A", "B", "a"), unique)
        assertTrue(events.any { changes -> changes.any { it.first == ListEvent.DELETE } })
        assertTrue(events.any { changes -> changes.any { it.first == ListEvent.INSERT } })

        unique.setComparator(null)
        assertEquals(listOf("A", "B", "a"), unique)
    }

    @Test
    fun removingAndReplacingAUniqueValueMutateAllRepresentedSourceValues() {
        val source = BasicEventList<String>().apply { addAll(listOf("b", "a", "b", "c", "a")) }
        val unique = UniqueList(source)

        assertEquals("b", unique.removeAt(1))
        assertEquals(listOf("a", "c", "a"), source)
        assertEquals(listOf("a", "c"), unique)

        assertEquals("a", unique.set(0, "d"))
        assertEquals(listOf("d", "c"), source)
        assertEquals(listOf("c", "d"), unique)
    }

    @Test
    fun sourceChangesPublishGroupEventsAndDisposeStopsForwarding() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "b")) }
        val unique = UniqueList(source)
        val events = unique.recordChanges()

        source.add("a")
        assertEquals(listOf(listOf(ListEvent.UPDATE to 0)), events)

        source.add("c")
        assertEquals(listOf(ListEvent.INSERT to 2), events.last())

        source[0] = "d"
        assertEquals(listOf("a", "b", "c", "d"), unique)
        assertTrue(events.last().isNotEmpty())

        unique.dispose()
        val eventCount = events.size
        source.add("e")
        assertEquals(eventCount, events.size)
        assertFalse(unique.contains("e"))
    }

    private fun <E> EventList<E>.recordChanges() = mutableListOf<List<Pair<Int, Int>>>().also { events ->
        addListEventListener { changes ->
            val event = mutableListOf<Pair<Int, Int>>()
            while (changes.next()) event += changes.type to changes.index
            events += event
        }
    }

    private data class Row(val key: Int, val id: String)
}
