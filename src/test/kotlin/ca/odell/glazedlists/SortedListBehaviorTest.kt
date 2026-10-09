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
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.impl.UpgradeDetectingReadWriteLock
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class SortedListBehaviorTest {
    @Test
    fun naturalOrderFactoryAndConstructorSortComparableElements() {
        val source = BasicEventList<Int>().apply { addAll(listOf(3, 1, 2)) }

        val fromFactory = SortedList.create(source)
        val fromConstructor = SortedList(source)

        assertEquals(listOf(1, 2, 3), fromFactory)
        assertEquals(listOf(1, 2, 3), fromConstructor)
        assertEquals(SortedList.STRICT_SORT_ORDER, fromFactory.mode)
    }

    @Test
    fun equalComparatorValuesRemainInSourceOrderAcrossInsertion() {
        val first = Row(1, "first")
        val second = Row(1, "second")
        val third = Row(1, "third")
        val source = BasicEventList<Row>().apply { addAll(listOf(first, second)) }
        val sorted = SortedList(source, compareBy(Row::key))
        val events = EventRecorder(sorted)

        source.add(1, third)

        assertEquals(listOf(first, third, second), sorted)
        assertEquals(listOf(Change(ListEvent.INSERT, 1)), events.last().changes)
    }

    @Test
    fun sourceReorderIsTranslatedForComparatorEqualElements() {
        val first = Row(1, "first")
        val second = Row(1, "second")
        val third = Row(1, "third")
        val source = ReorderableEventList(listOf(first, second, third))
        val sorted = SortedList(source, compareBy(Row::key))
        val events = EventRecorder(sorted)

        source.reorder(intArrayOf(2, 0, 1))

        assertEquals(listOf(third, first, second), sorted)
        assertEquals(listOf(2, 0, 1), events.last().reorderMap)
    }

    @Test
    fun sourceReorderWithDistinctValuesStaysSilentAndRetainsTreeMappings() {
        val source = ReorderableEventList(listOf(3, 1, 2))
        val sorted = SortedList(source, naturalOrder())
        val events = EventRecorder(sorted)

        source.reorder(intArrayOf(2, 0, 1))

        assertEquals(listOf(2, 3, 1), source)
        assertEquals(listOf(1, 2, 3), sorted)
        assertTrue(events.isEmpty())

        source[1] = 0
        assertEquals(listOf(0, 1, 2), sorted)
        assertEquals(
            listOf(Change(ListEvent.INSERT, 0), Change(ListEvent.DELETE, 3)),
            events.last().changes,
        )

        source.removeAt(0)
        assertEquals(listOf(0, 1), sorted)
        assertEquals(listOf(Change(ListEvent.DELETE, 2)), events.last().changes)
    }

    @Test
    fun strictModeMovesUpdatedElementsWithDeleteAndInsertChanges() {
        val one = Row(1, "one")
        val two = Row(2, "two")
        val three = Row(3, "three")
        val moved = Row(4, "moved")
        val source = BasicEventList<Row>().apply { addAll(listOf(two, one, three)) }
        val sorted = SortedList(source, compareBy(Row::key))
        val events = EventRecorder(sorted)

        source[0] = moved

        assertEquals(listOf(one, three, moved), sorted)
        assertEquals(
            listOf(
                Change(ListEvent.DELETE, 1),
                Change(ListEvent.INSERT, 2),
            ),
            events.single().changes,
        )
    }

    @Test
    fun avoidMovingModeForwardsUpdateUntilStrictModeRestoresOrder() {
        val one = Row(1, "one")
        val two = Row(2, "two")
        val three = Row(3, "three")
        val moved = Row(4, "moved")
        val source = BasicEventList<Row>().apply { addAll(listOf(two, one, three)) }
        val sorted = SortedList(source, compareBy(Row::key))
        val events = EventRecorder(sorted)

        sorted.mode = SortedList.AVOID_MOVING_ELEMENTS
        source[0] = moved

        assertEquals(listOf(one, moved, three), sorted)
        assertEquals(listOf(Change(ListEvent.UPDATE, 1)), events.single().changes)

        sorted.mode = SortedList.STRICT_SORT_ORDER

        assertEquals(listOf(one, three, moved), sorted)
        assertEquals(listOf(0, 2, 1), events.last().reorderMap)
    }

    @Test
    fun comparatorChangesAndNullComparatorPublishExactReorderMaps() {
        val source = BasicEventList<Int>().apply { addAll(listOf(2, 1, 3)) }
        val sorted = SortedList(source, naturalOrder())
        val events = EventRecorder(sorted)

        sorted.comparator = reverseOrder()

        assertEquals(listOf(3, 2, 1), sorted)
        assertEquals(listOf(2, 1, 0), events.single().reorderMap)

        sorted.comparator = null

        assertEquals(listOf(2, 1, 3), sorted)
        assertEquals(listOf(1, 2, 0), events.last().reorderMap)
    }

    @Test
    fun comparatorRebuildReadsEachSourceElementOnlyOnce() {
        val source = CountingEventList((0 until 128).reversed().toList())
        val sorted = SortedList(source, naturalOrder())
        source.resetGetCalls()

        sorted.comparator = reverseOrder()

        assertEquals(source.size, source.getCalls)
        assertEquals((127 downTo 0).toList(), sorted)
    }

    @Test
    fun failingComparatorReplacementLeavesPreviousStateUsable() {
        val source = BasicEventList<Int>().apply { addAll(listOf(3, 1, 2)) }
        val previousComparator = naturalOrder<Int>()
        val sorted = SortedList(source, previousComparator)
        val events = EventRecorder(sorted)
        val failure = IllegalStateException("broken comparator")

        val thrown = assertThrows(IllegalStateException::class.java) {
            sorted.comparator = Comparator { _, _ -> throw failure }
        }

        assertSame(failure, thrown)
        assertSame(previousComparator, sorted.comparator)
        assertEquals(listOf(1, 2, 3), sorted)
        assertTrue(events.isEmpty())

        source.add(0)
        source[0] = 4
        source.removeAt(1)

        assertEquals(listOf(0, 2, 4), sorted)
        assertSame(previousComparator, sorted.comparator)
        assertEquals(
            listOf(
                Change(ListEvent.INSERT, 0),
                Change(ListEvent.UPDATE, 3),
                Change(ListEvent.DELETE, 1),
            ),
            events.allChanges(),
        )
    }

    @Test
    fun sortLocationsAndEqualitySearchRetainDistinctContracts() {
        val first = Row(1, "first")
        val second = Row(2, "second")
        val third = Row(2, "third")
        val source = BasicEventList<Row>().apply { addAll(listOf(third, first, second, Row(4, "four"))) }
        val sorted = SortedList(source, compareBy(Row::key))

        assertEquals(1, sorted.sortIndex(Row(2, "missing")))
        assertEquals(2, sorted.lastSortIndex(Row(2, "missing")))
        assertEquals(3, sorted.sortIndex(Row(3, "missing")))
        assertEquals(3, sorted.lastSortIndex(Row(3, "missing")))
        assertEquals(-1, sorted.indexOf(Row(2, "missing")))
        assertEquals(2, sorted.indexOf(second))
        assertEquals(1, sorted.lastIndexOf(third))
        assertTrue(sorted.contains(third))
        assertFalse(sorted.contains(Row(2, "missing")))

        sorted.comparator = null
        assertThrows(IllegalStateException::class.java) { sorted.sortIndex(first) }
        assertThrows(IllegalStateException::class.java) { sorted.lastSortIndex(first) }
    }

    @Test
    fun weakComparatorSearchSupportsNullsWithoutConfusingComparatorEqualityWithEquals() {
        val source = BasicEventList<String?>().apply { addAll(listOf("bb", "a", null, "c")) }
        val byLength = compareBy<String?, Int?>(nullsFirst()) { value -> value?.length }
        val sorted = SortedList(source, byLength)

        assertEquals(listOf(null, "a", "c", "bb"), sorted)
        assertEquals(0, sorted.indexOf(null))
        assertEquals(-1, sorted.indexOf("z"))
        assertEquals(1, sorted.sortIndex("z"))
        assertEquals(2, sorted.lastSortIndex("z"))
        assertThrows(ClassCastException::class.java) { sorted.sortIndex(1) }
    }

    @Test
    fun writesAndIteratorRemovalMapBackToSourceIndices() {
        val source = BasicEventList<Int>().apply { addAll(listOf(30, 10, 20)) }
        val sorted = SortedList(source, naturalOrder())

        sorted[1] = 25
        assertEquals(listOf(30, 10, 25), source)
        assertEquals(listOf(10, 25, 30), sorted)

        val iterator = sorted.iterator()
        assertEquals(10, iterator.next())
        iterator.remove()

        assertEquals(listOf(30, 25), source)
        assertEquals(listOf(25, 30), sorted)

        sorted.add(1, 27)
        assertEquals(listOf(27, 30, 25), source)
        assertEquals(listOf(25, 27, 30), sorted)
    }

    @Test
    fun rejectsUnsupportedModesWithoutChangingState() {
        val sorted = SortedList(BasicEventList<Int>())

        val exception = assertThrows(IllegalArgumentException::class.java) { sorted.mode = 2 }

        assertEquals(
            "Mode must be either SortedList.STRICT_SORT_ORDER or SortedList.AVOID_MOVING_ELEMENTS",
            exception.message,
        )
        assertEquals(SortedList.STRICT_SORT_ORDER, sorted.mode)
    }

    private data class Row(val key: Int, val id: String)

    private data class Change(val type: Int, val index: Int)

    private data class RecordedEvent(
        val changes: List<Change> = emptyList(),
        val reorderMap: List<Int> = emptyList(),
    )

    private class EventRecorder<E>(source: EventList<E>) : ListEventListener<E> {
        private val recordedEvents = mutableListOf<RecordedEvent>()

        init {
            source.addListEventListener(this)
        }

        override fun listChanged(listChanges: ListEvent<E>) {
            if (listChanges.isReordering) {
                recordedEvents += RecordedEvent(reorderMap = listChanges.reorderMap.toList())
                return
            }

            val changes = mutableListOf<Change>()
            while (listChanges.next()) {
                changes += Change(listChanges.type, listChanges.index)
            }
            recordedEvents += RecordedEvent(changes = changes)
        }

        fun single(): RecordedEvent = recordedEvents.single()

        fun last(): RecordedEvent = recordedEvents.last()

        fun isEmpty(): Boolean = recordedEvents.isEmpty()

        fun allChanges(): List<Change> = recordedEvents.flatMap(RecordedEvent::changes)
    }

    private class ReorderableEventList<E>(elements: List<E>) : AbstractEventList<E>() {
        private var data = elements.toMutableList()

        init {
            readWriteLock = UpgradeDetectingReadWriteLock()
        }

        override val size: Int
            get() = data.size

        override fun get(index: Int): E = data[index]

        override fun set(index: Int, element: E): E {
            updates.beginEvent()
            val previous = data.set(index, element)
            updates.elementUpdated(index, previous, element)
            updates.commitEvent()
            return previous
        }

        override fun removeAt(index: Int): E {
            updates.beginEvent()
            val removed = data.removeAt(index)
            updates.elementDeleted(index, removed)
            updates.commitEvent()
            return removed
        }

        override fun dispose() = Unit

        fun reorder(reorderMap: IntArray) {
            val previous = data
            data = reorderMap.mapTo(ArrayList(previous.size), previous::get)
            updates.beginEvent()
            updates.reorder(reorderMap)
            updates.commitEvent()
        }
    }

    private class CountingEventList<E>(private val data: List<E>) : AbstractEventList<E>() {
        var getCalls: Int = 0
            private set

        init {
            readWriteLock = UpgradeDetectingReadWriteLock()
        }

        override val size: Int
            get() = data.size

        override fun get(index: Int): E {
            getCalls++
            return data[index]
        }

        override fun dispose() = Unit

        fun resetGetCalls() {
            getCalls = 0
        }
    }
}
