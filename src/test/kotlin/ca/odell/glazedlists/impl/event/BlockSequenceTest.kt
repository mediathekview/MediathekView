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
package ca.odell.glazedlists.impl.event

import ca.odell.glazedlists.event.ListEvent
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class BlockSequenceTest {
    @Test
    fun contiguousUnknownUpdatesAreCombined() {
        val sequence = BlockSequence<Any>()

        assertTrue(sequence.update(2, 4))
        assertTrue(sequence.update(4, 6))

        val iterator = sequence.iterator()
        assertTrue(iterator.nextBlock())
        assertEquals(ListEvent.UPDATE, iterator.type)
        assertEquals(2, iterator.blockStart)
        assertEquals(6, iterator.blockEnd)
        assertFalse(iterator.hasNextBlock())
        assertEquals("U2-6", sequence.toString())
    }

    @Test
    fun equalButDistinctValuesRemainSeparateBlocks() {
        val sequence = BlockSequence<Any>()
        val unknownValue = ListEvent.unknownValue<Any>()
        val firstValue = String(charArrayOf('v'))
        val secondValue = String(charArrayOf('v'))

        assertTrue(sequence.addChange(ListEvent.INSERT, 0, 1, unknownValue, firstValue))
        assertTrue(sequence.addChange(ListEvent.INSERT, 1, 2, unknownValue, secondValue))

        val iterator = sequence.iterator()
        assertTrue(iterator.nextBlock())
        assertSame(firstValue, iterator.newValue)
        assertTrue(iterator.nextBlock())
        assertSame(secondValue, iterator.newValue)
        assertFalse(iterator.hasNextBlock())
    }

    @Test
    fun outOfOrderChangeIsRejectedWithoutMutation() {
        val sequence = BlockSequence<Any>()
        assertTrue(sequence.insert(5, 6))

        assertFalse(sequence.update(4, 5))

        assertEquals("+5-6", sequence.toString())
    }

    @Test
    fun deleteIterationKeepsTheIndexAtTheBlockStart() {
        val sequence = BlockSequence<Any>()
        assertTrue(sequence.delete(2, 5))

        val iterator = sequence.iterator()
        assertTrue(iterator.next())
        assertEquals(2, iterator.index)
        assertEquals(2, iterator.blockStart)
        assertEquals(5, iterator.blockEnd)
        assertTrue(iterator.next())
        assertEquals(2, iterator.index)
        assertTrue(iterator.next())
        assertEquals(2, iterator.index)
        assertFalse(iterator.next())
    }

    @Test
    fun copiedIteratorRetainsAnIndependentPosition() {
        val sequence = BlockSequence<Any>()
        assertTrue(sequence.insert(1, 3))
        assertTrue(sequence.update(3, 5))

        val iterator = sequence.iterator()
        assertTrue(iterator.next())
        assertEquals(1, iterator.index)
        val copy = iterator.copy()

        assertTrue(iterator.next())
        assertEquals(2, iterator.index)
        assertEquals(1, copy.index)
        assertTrue(copy.next())
        assertEquals(2, copy.index)
        assertTrue(iterator.nextBlock())
        assertEquals(3, iterator.blockStart)
        assertEquals(ListEvent.UPDATE, iterator.type)
    }

    @Test
    fun storageGrowsAndCanBeReusedAfterReset() {
        val sequence = BlockSequence<Any>()
        repeat(25) { block ->
            val start = block * 2
            assertTrue(sequence.update(start, start + 1))
        }

        val iterator = sequence.iterator()
        repeat(25) { block ->
            assertTrue(iterator.nextBlock())
            assertEquals(block * 2, iterator.blockStart)
            assertEquals(block * 2 + 1, iterator.blockEnd)
        }
        assertFalse(iterator.hasNextBlock())

        sequence.reset()
        assertTrue(sequence.isEmpty)
        assertFalse(sequence.iterator().hasNext())
        assertTrue(sequence.insert(10, 11))
        assertEquals("+10-11", sequence.toString())
    }
}
