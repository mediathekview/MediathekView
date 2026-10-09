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
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

internal class SequenceListBehaviorTest {
    @Test
    fun naturalOrderConstructionCoversSourceAndSequenceValuesRemainStable() {
        val source = BasicEventList<Int>().apply { addAll(listOf(27, 13, 20)) }

        SequenceList(source, tensSequencer()).use { sequence ->
            assertEquals(listOf(10, 20, 30), sequence)
            assertEquals(10, sequence.getPreviousSequenceValue(13))
            assertEquals(20, sequence.getPreviousSequenceValue(20))
            assertEquals(20, sequence.getNextSequenceValue(13))
            assertEquals(20, sequence.getNextSequenceValue(20))
        }
    }

    @Test
    fun sourceChangesExpandContractAndClearTheSequenceWithExactEvents() {
        val source = BasicEventList<Int>().apply { addAll(listOf(13, 27)) }

        SequenceList(source, tensSequencer()).use { sequence ->
            val events = mutableListOf<Change>()
            sequence.addListEventListener { event ->
                while (event.next()) events += Change(event.type, event.index, event.oldValue, event.newValue)
            }

            source.add(42)
            assertEquals(listOf(10, 20, 30, 40, 50), sequence)
            assertEquals(
                listOf(
                    Change(ListEvent.INSERT, 3, ListEvent.UNKNOWN_VALUE, 40),
                    Change(ListEvent.INSERT, 4, ListEvent.UNKNOWN_VALUE, 50),
                ),
                events,
            )

            events.clear()
            source.remove(42)
            assertEquals(listOf(10, 20, 30), sequence)
            assertEquals(
                listOf(
                    Change(ListEvent.DELETE, 3, 40, ListEvent.UNKNOWN_VALUE),
                    Change(ListEvent.DELETE, 3, 50, ListEvent.UNKNOWN_VALUE),
                ),
                events,
            )

            events.clear()
            source.clear()
            assertTrueSequenceIsEmpty(sequence)
            assertEquals(
                listOf(10, 20, 30).map { Change(ListEvent.DELETE, 0, it, ListEvent.UNKNOWN_VALUE) },
                events,
            )
        }
    }

    @Test
    fun emptySourceSeedsSequenceOnFirstInsertion() {
        val source = BasicEventList<Int>()

        SequenceList(source, tensSequencer()).use { sequence ->
            assertTrueSequenceIsEmpty(sequence)

            source.add(15)

            assertEquals(listOf(10, 20), sequence)
        }
    }

    @Test
    fun disposalStopsSequenceUpdates() {
        val source = BasicEventList<Int>().apply { add(15) }
        val sequence = SequenceList(source, tensSequencer())
        val snapshot = sequence.toList()
        var events = 0
        sequence.addListEventListener { events++ }

        sequence.dispose()
        source.add(35)

        assertEquals(snapshot, sequence)
        assertEquals(0, events)
    }

    @Test
    fun writesRetainTheReadOnlyFailureContract() {
        val source = BasicEventList<Int>().apply { add(15) }

        SequenceList(source, tensSequencer()).use { sequence ->
            val failure = assertThrows(IllegalStateException::class.java) { sequence.add(25) }
            assertEquals("Non-writable List cannot be modified", failure.message)
        }
    }

    private fun tensSequencer(): SequenceList.Sequencer<Int> = object : SequenceList.Sequencer<Int> {
        override fun previous(value: Int): Int = Math.floorDiv(value - 1, 10) * 10

        override fun next(value: Int): Int = (Math.floorDiv(value, 10) + 1) * 10
    }

    private fun assertTrueSequenceIsEmpty(sequence: SequenceList<Int>) {
        assertEquals(0, sequence.size)
        assertEquals(emptyList<Int>(), sequence)
    }

    private data class Change(val type: Int, val index: Int, val oldValue: Any?, val newValue: Any?)
}
