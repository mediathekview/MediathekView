package ca.odell.glazedlists.impl

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class EventListIteratorAndSubListBehaviorTest {
    @Test
    fun iteratorTracksExternalChangesAndMutatesTheCurrentElement() {
        val source = BasicEventList<String>()
        source.addAll(listOf("a", "b", "c"))
        val iterator = EventListIterator(source, 1, false)

        assertEquals("b", iterator.next())
        source.add(0, "x")
        iterator.set("B")
        assertEquals(listOf("x", "a", "B", "c"), source)

        iterator.remove()
        assertEquals(listOf("x", "a", "c"), source)
        assertEquals("c", iterator.next())
    }

    @Test
    fun iteratorInsertAdvancesTheCursorAndIsVisibleToPrevious() {
        val source = BasicEventList<String>()
        source.addAll(listOf("a", "b"))
        val iterator = EventListIterator(source, 1)

        iterator.add("x")

        assertEquals(listOf("a", "x", "b"), source)
        assertEquals(2, iterator.nextIndex())
        assertEquals("x", iterator.previous())
    }

    @Test
    fun iteratorRetainsExactFailureDiagnostics() {
        val source = BasicEventList<String>()
        val iterator = EventListIterator(source, 0, false)

        assertEquals(
            "Cannot retrieve element 0 on a list of size 0",
            assertThrows(NoSuchElementException::class.java, iterator::next).message,
        )
        assertEquals(
            "Cannot retrieve element 0 on a list of size 0",
            assertThrows(NoSuchElementException::class.java, iterator::previous).message,
        )
        assertEquals(
            "Cannot remove() without a prior call to next() or previous()",
            assertThrows(IllegalStateException::class.java, iterator::remove).message,
        )
        assertEquals(
            "Cannot set() without a prior call to next() or previous()",
            assertThrows(IllegalStateException::class.java) { iterator.set("x") }.message,
        )
    }

    @Test
    fun subListTracksChangesRelativeToItsRangeAndTranslatesEvents() {
        val source = BasicEventList<String>()
        source.addAll(listOf("a", "b", "c", "d"))
        val subList = SubEventList(source, 1, 3, false)
        val events = mutableListOf<Pair<Int, Int>>()
        subList.addListEventListener(ListEventListener { changes ->
            while (changes.next()) events += changes.type to changes.index
        })

        source.add(0, "before")
        source.add(2, "at-start")
        source.add(4, "inside")
        source[3] = "B"
        source.removeAt(4)
        source.add(5, "after")

        assertEquals(listOf("B", "c"), subList)
        assertEquals(
            listOf(
                ListEvent.INSERT to 1,
                ListEvent.UPDATE to 0,
                ListEvent.DELETE to 1,
            ),
            events,
        )
    }

    @Test
    fun subListWritesThroughToTheSource() {
        val source = BasicEventList<String>()
        source.addAll(listOf("a", "b", "c", "d"))
        val subList = SubEventList(source, 1, 3, false)

        subList[0] = "B"
        subList.add(1, "x")
        assertEquals("c", subList.removeAt(2))

        assertEquals(listOf("a", "B", "x", "d"), source)
        assertEquals(listOf("B", "x"), subList)
    }

    @Test
    fun abstractEventListSubListUsesTheAutomaticListenerPath() {
        val source = BasicEventList<String>()
        source.addAll(listOf("a", "b", "c"))

        val subList = source.subList(1, 3)
        source.add(2, "x")

        assertEquals(listOf("b", "x", "c"), subList)
        assertTrue(subList is EventList<*>)
    }

    @Test
    fun singleElementSubListFollowsReordering() {
        val source = BasicEventList<String>()
        source.addAll(listOf("c", "a", "b"))
        val sorted = SortedList(source, Comparator.naturalOrder())
        val subList = SubEventList(sorted, 0, 1, false)

        sorted.comparator = Comparator.reverseOrder()

        assertEquals(listOf("a"), subList)
    }

    @Test
    fun subListRetainsExactInvalidRangeFailure() {
        val source = BasicEventList<String>()
        source.add("a")

        assertEquals(
            "The range -1-1 is not valid over a list of size 1",
            assertThrows(IllegalArgumentException::class.java) {
                SubEventList(source, -1, 1, false)
            }.message,
        )
    }
}
