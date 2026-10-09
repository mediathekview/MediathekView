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

internal class ThresholdListBehaviorTest {
    @Test
    fun inclusiveThresholdsRetainDuplicatesAndPublishExactBoundaryEvents() {
        val five = Scored(5, "five")
        val ten = Scored(10, "ten")
        val fifteen = Scored(15, "fifteen")
        val twentyA = Scored(20, "twenty-a")
        val twentyB = Scored(20, "twenty-b")
        val twentyFive = Scored(25, "twenty-five")
        val thirty = Scored(30, "thirty")
        val source = BasicEventList<Scored>().apply {
            addAll(listOf(thirty, ten, twentyA, twentyB, five))
        }
        val threshold = ThresholdList(source, ThresholdList.Evaluator(Scored::score))
        val events = mutableListOf<Change>()
        threshold.addListEventListener { event ->
            while (event.next()) {
                events += Change(event.type, event.index, event.oldValue, event.newValue)
            }
        }

        assertEquals(listOf(five, ten, twentyA, twentyB, thirty), threshold)
        assertSame(threshold.evaluator, threshold.evaluator)

        threshold.lowerThreshold = 10
        assertEquals(listOf(ten, twentyA, twentyB, thirty), threshold)
        assertEquals(listOf(Change(ListEvent.DELETE, 0, five, ListEvent.UNKNOWN_VALUE)), events)

        events.clear()
        threshold.upperThreshold = 20
        assertEquals(listOf(ten, twentyA, twentyB), threshold)
        assertEquals(listOf(Change(ListEvent.DELETE, 3, thirty, ListEvent.UNKNOWN_VALUE)), events)

        events.clear()
        source.add(fifteen)
        assertEquals(listOf(ten, fifteen, twentyA, twentyB), threshold)
        assertEquals(
            listOf(Change(ListEvent.INSERT, 1, ListEvent.UNKNOWN_VALUE, ListEvent.UNKNOWN_VALUE)),
            events,
        )

        events.clear()
        source.add(twentyFive)
        assertEquals(listOf(ten, fifteen, twentyA, twentyB), threshold)
        assertEquals(emptyList<Change>(), events)

        events.clear()
        source.remove(twentyA)
        assertEquals(listOf(ten, fifteen, twentyB), threshold)
        assertEquals(listOf(Change(ListEvent.DELETE, 2, twentyA, ListEvent.UNKNOWN_VALUE)), events)

        threshold.dispose()
    }

    @Test
    fun objectThresholdsRangeMethodsAndLookupsKeepLegacySemantics() {
        val values = listOf(
            Scored(30, "thirty"),
            Scored(10, "ten"),
            Scored(20, "twenty-a"),
            Scored(20, "twenty-b"),
            Scored(5, "five"),
        )
        val source = BasicEventList<Scored>().apply { addAll(values) }

        ThresholdList(source, ThresholdList.Evaluator(Scored::score)).use { threshold ->
            threshold.setLowerThreshold(Scored(10, "lower"))
            threshold.setUpperThreshold(Scored(20, "upper"))

            assertEquals(listOf("ten", "twenty-a", "twenty-b"), threshold.map(Scored::name))
            assertTrue(threshold.contains(values[1]))
            assertFalse(threshold.contains(Scored(15, "absent")))
            assertEquals(2, threshold.indexOf(values[2]))
            assertEquals(3, threshold.lastIndexOf(values[3]))

            threshold.lowerThreshold = Int.MIN_VALUE
            threshold.upperThreshold = Int.MAX_VALUE
            threshold.setHeadRange(1, 3)

            assertEquals(10, threshold.lowerThreshold)
            assertEquals(20, threshold.upperThreshold)
            assertEquals(listOf("ten", "twenty-a", "twenty-b"), threshold.map(Scored::name))
        }
    }

    @Test
    fun writesMapThroughTheSortedRangeAndDisposeFreezesTheView() {
        val five = Scored(5, "five")
        val ten = Scored(10, "ten")
        val fifteen = Scored(15, "fifteen")
        val twenty = Scored(20, "twenty")
        val source = BasicEventList<Scored>().apply { addAll(listOf(twenty, five, fifteen, ten)) }
        val threshold = ThresholdList(source, ThresholdList.Evaluator(Scored::score))
        threshold.lowerThreshold = 10
        threshold.upperThreshold = 20

        assertSame(fifteen, threshold.removeAt(1))
        assertFalse(source.contains(fifteen))
        assertEquals(listOf(ten, twenty), threshold)

        var events = 0
        threshold.addListEventListener { events++ }
        threshold.dispose()
        source.add(Scored(12, "after-dispose"))

        assertEquals(listOf(ten, twenty), threshold)
        assertEquals(0, events)
    }

    @Test
    fun thresholdComparatorUsesIntegerArgumentsDirectlyAndRetainsValueSemantics() {
        val evaluator: ThresholdList.Evaluator<Scored> = ThresholdList.Evaluator(Scored::score)
        val comparator = ThresholdList.ThresholdComparator(evaluator)
        val equalComparator = ThresholdList.ThresholdComparator(evaluator)
        val otherEvaluator = ThresholdList.Evaluator<Scored> { element -> element.score }

        assertTrue(comparator.compare(Scored(Int.MIN_VALUE, "minimum"), Scored(Int.MAX_VALUE, "maximum")) < 0)
        assertEquals(comparator, equalComparator)
        assertEquals(comparator.hashCode(), equalComparator.hashCode())
        assertNotEquals(comparator, ThresholdList.ThresholdComparator(otherEvaluator))
    }

    private data class Scored(val score: Int, val name: String)

    private data class Change(
        val type: Int,
        val index: Int,
        val oldValue: Any?,
        val newValue: Any?,
    )
}
