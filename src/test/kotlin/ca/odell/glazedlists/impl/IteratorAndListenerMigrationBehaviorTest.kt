package ca.odell.glazedlists.impl

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.event.ListEventListener
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class IteratorAndListenerMigrationBehaviorTest {
    @Test
    fun simpleIteratorTraversesAndRemovesFromTheSource() {
        val source = mutableListOf("first", "second")
        val iterator = SimpleIterator(source)

        assertTrue(iterator.hasNext())
        assertEquals("first", iterator.next())
        iterator.remove()
        assertEquals(listOf("second"), source)
        assertEquals("second", iterator.next())
        assertFalse(iterator.hasNext())
    }

    @Test
    fun simpleIteratorPreservesItsFailureMessages() {
        val emptyIterator = SimpleIterator(mutableListOf<String>())
        assertEquals(
            "Cannot retrieve element 0 on a list of size 0",
            assertThrows(NoSuchElementException::class.java, emptyIterator::next).message,
        )

        val iterator = SimpleIterator(mutableListOf("value"))
        assertEquals(
            "Cannot remove() without a prior call to next() or previous()",
            assertThrows(IllegalStateException::class.java, iterator::remove).message,
        )
    }

    @Test
    fun weakReferenceProxyForwardsUntilDisposed() {
        val source = BasicEventList<String>()
        var forwardedChanges = 0
        val target = ListEventListener<String> { changes ->
            while (changes.next()) forwardedChanges++
        }
        val proxy = WeakReferenceProxy(source, target)
        source.addListEventListener(proxy)

        assertSame(target, proxy.referent)
        source += "first"
        assertEquals(1, forwardedChanges)

        proxy.dispose()
        source += "second"
        assertEquals(1, forwardedChanges)
    }
}
