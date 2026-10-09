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

internal class TransformedListBehaviorTest {
    @Test
    fun translatedReadsAndWritesDelegateToSourceIndices() {
        val source = BasicEventList<String>().apply { addAll(listOf("prefix", "A", "B")) }
        val transformed = OffsetTransformedList(source)

        assertEquals(listOf("A", "B"), transformed.toList())
        assertSame(source.publisher, transformed.publisher)
        assertSame(source.readWriteLock, transformed.readWriteLock)

        transformed.add(1, "X")
        assertEquals(listOf("prefix", "A", "X", "B"), source)
        assertEquals("A", transformed.set(0, "AA"))
        assertEquals("B", transformed.removeAt(2))
        transformed.add(transformed.size, "tail")

        assertEquals(listOf("prefix", "AA", "X", "tail"), source)
        assertEquals(listOf("AA", "X", "tail"), transformed.toList())
    }

    @Test
    fun nonWritableMutationsRetainExactDiagnostics() {
        val source = BasicEventList<String>().apply { addAll(listOf("prefix", "A")) }
        val transformed = OffsetTransformedList(source, writable = false)

        assertFailure("Non-writable List cannot be modified") { transformed.add(0, "B") }
        assertFailure("Non-writable List cannot be modified") { transformed.removeAt(0) }
        assertFailure("List ${transformed.javaClass.name} cannot be modified in the current state") {
            transformed[0] = "B"
        }
        assertEquals(listOf("prefix", "A"), source)
    }

    @Test
    fun boundsFailuresRetainExactDiagnostics() {
        val source = BasicEventList<String>().apply { addAll(listOf("prefix", "A", "B")) }
        val transformed = OffsetTransformedList(source)

        assertBoundsFailure("Cannot add at -1 on list of size 2") { transformed.add(-1, "X") }
        assertBoundsFailure("Cannot add at 3 on list of size 2") { transformed.add(3, "X") }
        assertBoundsFailure("Cannot get at -1 on list of size 2") { readAt(transformed, -1) }
        assertBoundsFailure("Cannot get at 2 on list of size 2") { readAt(transformed, 2) }
        assertBoundsFailure("Cannot remove at 2 on list of size 2") { transformed.removeAt(2) }
        assertBoundsFailure("Cannot set at 2 on list of size 2") { transformed[2] = "X" }
    }

    @Test
    fun sourceEventsAreForwardedUntilDisposal() {
        val source = BasicEventList<String>().apply { addAll(listOf("prefix", "A")) }
        val transformed = OffsetTransformedList(source)
        var eventCount = 0
        transformed.addListEventListener { eventCount++ }

        source += "B"
        assertEquals(1, eventCount)

        transformed.dispose()
        source += "C"
        assertEquals(1, eventCount)
    }

    private fun assertFailure(message: String, action: () -> Unit) {
        val failure = assertThrows(IllegalStateException::class.java, action)
        assertEquals(message, failure.message)
    }

    private fun assertBoundsFailure(message: String, action: () -> Unit) {
        val failure = assertThrows(IndexOutOfBoundsException::class.java, action)
        assertEquals(message, failure.message)
    }

    private fun <E> readAt(list: List<E>, index: Int): E = list[index]

    private class OffsetTransformedList<E>(
        source: EventList<E>,
        private val writable: Boolean = true,
    ) : TransformedList<E, E>(source) {
        init {
            source.addListEventListener(this)
        }

        override val size: Int
            get() = source!!.size - 1

        override fun getSourceIndex(mutationIndex: Int): Int = mutationIndex + 1

        override fun isWritable(): Boolean = writable

        override fun listChanged(listChanges: ListEvent<E>) {
            updates.forwardEvent(listChanges)
        }
    }
}
