package ca.odell.glazedlists

import ca.odell.glazedlists.impl.gui.ThreadProxyEventList
import ca.odell.glazedlists.impl.swing.SwingThreadProxyEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.awt.EventQueue
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

internal class ThreadProxyEventListTest {
    @Test
    fun queuedChangesAreCoalescedAndAppliedToTheSnapshot() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B")) }
        val proxy = ManualThreadProxyEventList(source)
        var eventCount = 0
        proxy.addListEventListener { eventCount++ }

        source.add(1, "C")
        source[0] = "D"
        source.removeAt(2)

        assertEquals(listOf("A", "B"), proxy)
        assertEquals(0, eventCount)

        proxy.runPending()

        assertEquals(listOf("D", "C"), proxy)
        assertEquals(1, eventCount)
    }

    @Test
    fun swingProxyUpdatesAndNotifiesOnTheEventDispatchThread() {
        val source = BasicEventList<String>()
        val proxy = SwingThreadProxyEventList(source)
        val delivered = CountDownLatch(1)
        val deliveredOnEdt = AtomicBoolean(false)
        proxy.addListEventListener {
            deliveredOnEdt.set(EventQueue.isDispatchThread())
            delivered.countDown()
        }

        assertFalse(EventQueue.isDispatchThread())
        source += "A"

        assertTrue(delivered.await(2, TimeUnit.SECONDS))
        assertTrue(deliveredOnEdt.get())
        EventQueue.invokeAndWait { assertEquals(listOf("A"), proxy) }
    }

    @Test
    fun disposedProxyIgnoresPendingAndSubsequentSourceChanges() {
        val source = BasicEventList<String>().apply { add("A") }
        val proxy = ManualThreadProxyEventList(source)
        var eventCount = 0
        proxy.addListEventListener { eventCount++ }

        source += "B"
        proxy.dispose()
        source += "C"
        proxy.runPending()

        assertEquals(listOf("A"), proxy)
        assertEquals(0, eventCount)
    }

    private class ManualThreadProxyEventList<E>(source: EventList<E>) : ThreadProxyEventList<E>(source) {
        private var pending: Runnable? = null

        override fun schedule(runnable: Runnable) {
            check(pending == null)
            pending = runnable
        }

        fun runPending() {
            requireNotNull(pending).also { pending = null }.run()
        }
    }
}
