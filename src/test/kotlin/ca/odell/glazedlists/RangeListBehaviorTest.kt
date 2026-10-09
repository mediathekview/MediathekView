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
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class RangeListBehaviorTest {
    @Test
    fun rangeModesClampIndicesAndExposeExpectedElements() {
        val source = BasicEventList<Int>().apply { addAll(0..5) }
        val range = RangeList(source)

        assertEquals((0..5).toList(), range.toList())

        range.setHeadRange(1, 4)
        assertRange(range, 1, 4, listOf(1, 2, 3))

        range.setMiddleRange(1, 1)
        assertRange(range, 1, 5, listOf(1, 2, 3, 4))

        range.setTailRange(3, 1)
        assertRange(range, 3, 5, listOf(3, 4))

        range.setHeadRange(-20, 20)
        assertRange(range, 0, 6, (0..5).toList())

        range.setHeadRange(4, 2)
        assertRange(range, 4, 4, emptyList())
    }

    @Test
    fun writesTranslateVisibleIndicesIntoTheSource() {
        val source = BasicEventList<String>().apply { addAll(listOf("prefix", "A", "B", "C", "suffix")) }
        val range = RangeList(source).apply { setHeadRange(1, 4) }

        range.add(1, "X")
        assertEquals(listOf("prefix", "A", "X", "B", "C", "suffix"), source)

        assertEquals("A", range.set(0, "AA"))
        assertEquals(listOf("prefix", "AA", "X", "B", "C", "suffix"), source)

        assertEquals("B", range.removeAt(2))
        assertEquals(listOf("prefix", "AA", "X", "C", "suffix"), source)
    }

    @Test
    fun sourceChangesBeforeInsideAndAfterRangeRestoreDesiredSourceIndices() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B", "C", "D", "E")) }
        val range = RangeList(source).apply { setHeadRange(1, 4) }

        source.add(0, "before")
        assertEquals(listOf("A", "B", "C"), range)

        source.add(2, "inside")
        assertEquals(listOf("A", "inside", "B"), range)

        source.add("after")
        assertEquals(listOf("A", "inside", "B"), range)

        source[2] = "updated"
        assertEquals(listOf("A", "updated", "B"), range)

        source.removeAt(0)
        assertEquals(listOf("updated", "B", "C"), range)
    }

    @Test
    fun rangeAndSourceChangesPublishValuesAndVisibleIndices() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B", "C", "D", "E")) }
        val range = RangeList(source)
        val changes = mutableListOf<Change<String>>()
        range.addListEventListener { event ->
            while (event.next()) {
                changes += Change(event.type, event.index, event.oldValue, event.newValue)
            }
        }

        range.setHeadRange(1, 4)
        assertEquals(
            listOf(
                Change(ListEvent.DELETE, 0, "A", ListEvent.UNKNOWN_VALUE),
                Change(ListEvent.DELETE, 3, "E", ListEvent.UNKNOWN_VALUE),
            ),
            changes,
        )

        changes.clear()
        source[2] = "CC"
        assertEquals(listOf(Change(ListEvent.UPDATE, 1, "C", "CC")), changes)
    }

    private fun <E> assertRange(range: RangeList<E>, start: Int, end: Int, values: List<E>) {
        assertEquals(start, range.startIndex)
        assertEquals(end, range.endIndex)
        assertEquals(values, range.toList())
    }

    private data class Change<E>(
        val type: Int,
        val index: Int,
        val oldValue: E?,
        val newValue: E?,
    )
}
