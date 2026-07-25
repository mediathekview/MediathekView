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
import ca.odell.glazedlists.impl.UpgradeDetectingReadWriteLock
import ca.odell.glazedlists.matchers.AbstractMatcherEditor
import ca.odell.glazedlists.matchers.Matcher
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class FilterListBehaviorTest {
    @Test
    fun matcherConstructorAndSourceChangesTranslateExactVisibleEvents() {
        val source = BasicEventList<Int>().apply { addAll(listOf(1, 2, 3, 4)) }
        val filtered = FilterList(source, Matcher<Int> { it % 2 == 0 })
        val events = filtered.recordEvents()

        assertEquals(listOf(2, 4), filtered)

        source.add(1, 6)
        assertEquals(listOf(6, 2, 4), filtered)
        assertEquals(listOf(ListEvent.INSERT to 0), events.last().changes)

        source[2] = 5
        assertEquals(listOf(6, 4), filtered)
        assertEquals(listOf(ListEvent.DELETE to 1), events.last().changes)

        source[0] = 8
        assertEquals(listOf(8, 6, 4), filtered)
        assertEquals(listOf(ListEvent.INSERT to 0), events.last().changes)

        source.removeAt(1)
        assertEquals(listOf(8, 4), filtered)
        assertEquals(listOf(ListEvent.DELETE to 1), events.last().changes)

        source[0] = 10
        assertEquals(listOf(10, 4), filtered)
        assertEquals(listOf(ListEvent.UPDATE to 0), events.last().changes)

        val eventCount = events.size
        source[1] = 7
        source.add(9)
        source.removeAt(1)
        assertEquals(eventCount, events.size)
    }

    @Test
    fun matcherEditorTransitionsConstrainRelaxChangeMatchNoneAndMatchAll() {
        val source = BasicEventList<Int>().apply { addAll((1..6).toList()) }
        val editor = TestMatcherEditor(Matcher<Int> { it % 2 == 0 })
        val filtered = FilterList(source, editor)

        assertEquals(listOf(2, 4, 6), filtered)

        editor.constrainTo { it % 4 == 0 }
        assertEquals(listOf(4), filtered)

        editor.relaxTo { it % 2 == 0 }
        assertEquals(listOf(2, 4, 6), filtered)

        editor.changeTo { it > 3 }
        assertEquals(listOf(4, 5, 6), filtered)

        editor.matchNone()
        assertTrue(filtered.isEmpty())

        editor.matchAll()
        assertEquals((1..6).toList(), filtered)
    }

    @Test
    fun replacingEditorOrMatcherDetachesThePreviousEditorAndNullMatchesAll() {
        val source = BasicEventList<Int>().apply { addAll((1..4).toList()) }
        val firstEditor = TestMatcherEditor(Matcher<Int> { it % 2 == 0 })
        val secondEditor = TestMatcherEditor(Matcher<Int> { it > 2 })
        val filtered = FilterList(source, firstEditor)

        filtered.setMatcherEditor(secondEditor)
        assertEquals(listOf(3, 4), filtered)

        firstEditor.matchAll()
        assertEquals(listOf(3, 4), filtered)

        filtered.setMatcher(Matcher { it == 1 })
        assertEquals(listOf(1), filtered)

        secondEditor.matchAll()
        assertEquals(listOf(1), filtered)

        filtered.setMatcher(null)
        assertEquals((1..4).toList(), filtered)
    }

    @Test
    fun sourceReorderPublishesFilteredReorderMap() {
        val source = ReorderableEventList(listOf(1, 2, 3, 4))
        val filtered = FilterList(source, Matcher<Int> { it % 2 != 0 })
        val events = filtered.recordEvents()

        source.reorder(intArrayOf(2, 0, 3, 1))

        assertEquals(listOf(3, 1), filtered)
        assertEquals(listOf(1, 0), events.last().reorderMap)
    }

    @Test
    fun writesThroughFilteredIndicesAndDisposeStopsMatcherAndSourceEvents() {
        val source = BasicEventList<Int>().apply { addAll(listOf(1, 2, 3, 4)) }
        val editor = TestMatcherEditor(Matcher<Int> { it % 2 == 0 })
        val filtered = FilterList(source, editor)
        val events = filtered.recordEvents()

        filtered[0] = 6
        filtered.add(1, 8)
        assertEquals(listOf(1, 6, 3, 8, 4), source)
        assertEquals(listOf(6, 8, 4), filtered)

        assertEquals(8, filtered.removeAt(1))
        assertEquals(listOf(1, 6, 3, 4), source)

        filtered.dispose()
        val eventCount = events.size
        editor.matchAll()
        source.add(10)
        assertEquals(eventCount, events.size)
        assertFalse(filtered.contains(10))
    }

    @Test
    fun insertionAtFilteredEndMapsAfterTrailingHiddenRowsAndNonmatchingWritesRemainHidden() {
        val source = BasicEventList<Int>().apply { addAll(listOf(1, 2, 3, 4, 5)) }
        val filtered = FilterList(source, Matcher<Int> { it % 2 == 0 })

        filtered.add(filtered.size, 6)
        filtered.add(filtered.size, 7)

        assertEquals(listOf(1, 2, 3, 4, 5, 6, 7), source)
        assertEquals(listOf(2, 4, 6), filtered)

        assertEquals(2, filtered.set(0, 9))
        assertEquals(listOf(1, 9, 3, 4, 5, 6, 7), source)
        assertEquals(listOf(4, 6), filtered)
    }

    private data class RecordedEvent(
        val changes: List<Pair<Int, Int>> = emptyList(),
        val reorderMap: List<Int> = emptyList(),
    )

    private fun <E> EventList<E>.recordEvents() = mutableListOf<RecordedEvent>().also { events ->
        addListEventListener { listChanges ->
            if (listChanges.isReordering) {
                events += RecordedEvent(reorderMap = listChanges.reorderMap.toList())
            } else {
                val changes = mutableListOf<Pair<Int, Int>>()
                while (listChanges.next()) changes += listChanges.type to listChanges.index
                events += RecordedEvent(changes = changes)
            }
        }
    }

    private class TestMatcherEditor<E>(initialMatcher: Matcher<E>) : AbstractMatcherEditor<E>() {
        init {
            fireChanged(initialMatcher)
        }

        fun changeTo(matcher: Matcher<E>) = fireChanged(matcher)

        fun constrainTo(matcher: Matcher<E>) = fireConstrained(matcher)

        fun relaxTo(matcher: Matcher<E>) = fireRelaxed(matcher)

        fun matchAll() = fireMatchAll()

        fun matchNone() = fireMatchNone()
    }

    private class ReorderableEventList<E>(elements: List<E>) : AbstractEventList<E>() {
        private var data = elements.toMutableList()

        init {
            readWriteLock = UpgradeDetectingReadWriteLock()
        }

        override val size: Int
            get() = data.size

        override fun get(index: Int): E = data[index]

        override fun dispose() = Unit

        fun reorder(reorderMap: IntArray) {
            val previous = data
            data = reorderMap.mapTo(ArrayList(previous.size), previous::get)
            updates.beginEvent()
            updates.reorder(reorderMap)
            updates.commitEvent()
        }
    }
}
