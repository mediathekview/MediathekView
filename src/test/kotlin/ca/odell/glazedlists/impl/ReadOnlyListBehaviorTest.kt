package ca.odell.glazedlists.impl

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class ReadOnlyListBehaviorTest {
    @Test
    fun reflectsSourceChangesAndForwardsEvents() {
        val source = BasicEventList<String?>()
        source.addAll(listOf("a", null, "a"))
        val readOnly = ReadOnlyList(source)
        val events = mutableListOf<Pair<Int, Int>>()
        readOnly.addListEventListener(ListEventListener { changes ->
            while (changes.next()) events += changes.type to changes.index
        })

        source.add(1, "b")
        source[0] = "A"
        source.removeAt(2)

        assertEquals(listOf("A", "b", "a"), readOnly)
        assertEquals(
            listOf(
                ListEvent.INSERT to 1,
                ListEvent.UPDATE to 0,
                ListEvent.DELETE to 2,
            ),
            events,
        )
    }

    @Test
    fun delegatesAccessorsAndListEqualityToTheSource() {
        val source = BasicEventList<String?>()
        source.addAll(listOf("a", null, "a"))
        val readOnly = ReadOnlyList(source)

        assertTrue(readOnly.contains(null))
        assertTrue(readOnly.containsAll(listOf("a", null)))
        assertEquals(0, readOnly.indexOf("a"))
        assertEquals(2, readOnly.lastIndexOf("a"))
        assertEquals(source, readOnly)
        assertEquals(readOnly, source)
        assertEquals(source.hashCode(), readOnly.hashCode())
    }

    @Test
    fun arraySnapshotsPreserveJavaListSemantics() {
        val source = BasicEventList<String?>()
        source.addAll(listOf("a", null, "b"))
        val readOnly = ReadOnlyList(source)

        val first = readOnly.toArray()
        val second = readOnly.toArray()
        assertArrayEquals(arrayOf("a", null, "b"), first)
        assertNotSame(first, second)

        val exact = arrayOfNulls<String>(3)
        assertSame(exact, readOnly.toArray(exact))
        assertArrayEquals(arrayOf("a", null, "b"), exact)

        val oversized = arrayOf("old", "old", "old", "tail", "untouched")
        assertSame(oversized, readOnly.toArray(oversized))
        assertArrayEquals(arrayOf("a", null, "b", null, "untouched"), oversized)

        val undersized = emptyArray<String>()
        val expanded = readOnly.toArray(undersized)
        assertNotSame(undersized, expanded)
        assertEquals(Array<String>::class.java, expanded.javaClass)
        assertArrayEquals(arrayOf("a", null, "b"), expanded)
    }
}
