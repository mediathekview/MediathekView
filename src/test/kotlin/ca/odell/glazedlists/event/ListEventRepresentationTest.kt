package ca.odell.glazedlists.event

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class ListEventRepresentationTest {
    @Test
    fun linearRepresentationMatchesExactBlockSequenceOutput() {
        val repeatedInsert = Any()
        val deleted = Any()
        val event = pendingEvent(sourceSize = 0) {
            it.elementInserted(0, repeatedInsert)
            it.elementInserted(1, repeatedInsert)
            it.elementDeleted(2, deleted)
            it.elementDeleted(2, deleted)
        }

        event.reset()

        assertEquals("ListEvent: +0-2, X2-4", event.toString())
        assertTrue(event.nextBlock())
        assertEquals(0, event.blockStartIndex)
        assertEquals(1, event.blockEndIndex)
        assertSame(repeatedInsert, event.newValue)
        assertTrue(event.nextBlock())
        assertEquals(2, event.blockStartIndex)
        assertEquals(3, event.blockEndIndex)
        assertSame(deleted, event.oldValue)
        assertFalse(event.nextBlock())
    }

    @Test
    fun treeRepresentationMatchesExactFourColorOutput() {
        val repeated = Any()
        val event = pendingEvent(sourceSize = 3) {
            it.elementUpdated(2, repeated, repeated)
            it.elementUpdated(0, repeated, repeated)
            it.elementUpdated(1, repeated, repeated)
        }

        event.reset()

        assertEquals("ListEvent: UUU", event.toString())
        assertEquals(2, event.blocksRemaining)
        assertTrue(event.nextBlock())
        assertEquals(0, event.blockStartIndex)
        assertEquals(1, event.blockEndIndex)
        assertSame(repeated, event.oldValue)
        assertSame(repeated, event.newValue)
        assertTrue(event.nextBlock())
        assertEquals(2, event.blockStartIndex)
        assertEquals(2, event.blockEndIndex)
        assertSame(repeated, event.oldValue)
        assertSame(repeated, event.newValue)
        assertFalse(event.nextBlock())
    }

    @Test
    fun directEventClassRetainsPackagePrivateConstructionPathUsedByAssembler() {
        val source = BasicEventList<Any>()
        val assembler = ListEventAssembler(source, source.publisher)
        val eventClass = Class.forName("ca.odell.glazedlists.event.Tree4DeltasListEvent")
        val constructor = eventClass.getDeclaredConstructor(ListEventAssembler::class.java, EventList::class.java)
        val listEventField = ListEventAssembler::class.java.getDeclaredField("listEvent")

        listEventField.isAccessible = true
        val assemblerEvent = listEventField.get(assembler) as ListEvent<*>
        val reflectedEvent = constructor.newInstance(assembler, source) as ListEvent<*>

        assertEquals("ca.odell.glazedlists.event.Tree4DeltasListEvent", assemblerEvent.javaClass.name)
        assertEquals("ca.odell.glazedlists.event.Tree4DeltasListEvent", reflectedEvent.javaClass.name)
        assertSame(source, assemblerEvent.sourceList)
        assertSame(source, reflectedEvent.sourceList)
    }

    private fun pendingEvent(
        sourceSize: Int,
        addChanges: (ListEventAssembler<Any>) -> Unit,
    ): ListEvent<Any> {
        val source = BasicEventList<Any>()
        repeat(sourceSize) { source += Any() }
        val assembler = ListEventAssembler(source, source.publisher)
        assembler.beginEvent()
        addChanges(assembler)
        return newTree4DeltasListEvent(assembler, source)
    }

    @Suppress("UNCHECKED_CAST")
    private fun newTree4DeltasListEvent(
        assembler: ListEventAssembler<Any>,
        source: EventList<Any>,
    ): ListEvent<Any> {
        val eventClass = Class.forName("ca.odell.glazedlists.event.Tree4DeltasListEvent")
        val constructor = eventClass.getDeclaredConstructor(ListEventAssembler::class.java, EventList::class.java)
        return constructor.newInstance(assembler, source) as ListEvent<Any>
    }
}
