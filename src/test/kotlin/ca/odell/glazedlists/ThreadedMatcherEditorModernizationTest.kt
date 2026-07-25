package ca.odell.glazedlists

import ca.odell.glazedlists.matchers.AbstractMatcherEditor
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.MatcherEditor
import ca.odell.glazedlists.matchers.ThreadedMatcherEditor
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executor
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

internal class ThreadedMatcherEditorModernizationTest {
    @Test
    fun queuedEventsRemainCoalesced() {
        val source = TestMatcherEditor()
        val executor = CapturingExecutor()
        val threaded = ThreadedMatcherEditor(source, executor)
        val eventTypes = mutableListOf<Int>()
        threaded.addMatcherEditorListener { eventTypes += it.type }

        source.constrain()
        source.relax()

        assertTrue(eventTypes.isEmpty())
        executor.runPending()
        assertEquals(listOf(MatcherEditor.Event.CHANGED), eventTypes)
    }

    @Test
    fun rejectedSchedulingFallsBackWithoutLeavingQueueStuck() {
        val source = TestMatcherEditor()
        val executor = RejectOnceExecutor()
        val threaded = ThreadedMatcherEditor(source, executor)
        val delivered = CountDownLatch(1)
        val secondDelivered = CountDownLatch(1)
        val deliveryCount = AtomicInteger()
        threaded.addMatcherEditorListener {
            if (deliveryCount.incrementAndGet() == 2) secondDelivered.countDown()
            delivered.countDown()
        }

        source.constrain()
        assertTrue(delivered.await(1, TimeUnit.SECONDS))

        source.relax()
        executor.runPending()
        assertTrue(secondDelivered.await(1, TimeUnit.SECONDS))
        assertEquals(2, deliveryCount.get())
    }

    @Test
    fun defaultExecutorUsesVirtualThread() {
        val source = TestMatcherEditor()
        val threaded = ThreadedMatcherEditor(source)
        val delivered = CountDownLatch(1)
        var deliveryThread: Thread? = null
        threaded.addMatcherEditorListener {
            deliveryThread = Thread.currentThread()
            delivered.countDown()
        }

        source.constrain()

        assertTrue(delivered.await(1, TimeUnit.SECONDS))
        assertTrue(deliveryThread?.isVirtual == true)
        assertTrue(deliveryThread?.name?.startsWith("MatcherQueueThread") == true)
    }

    @Test
    fun closeIsIdempotentAndIgnoresLaterSourceEvents() {
        val source = TestMatcherEditor()
        val executor = CapturingExecutor()
        val threaded = ThreadedMatcherEditor(source, executor)
        var deliveryCount = 0
        threaded.addMatcherEditorListener { deliveryCount++ }

        threaded.close()
        threaded.close()
        source.constrain()

        assertEquals(0, deliveryCount)
    }

    private class TestMatcherEditor : AbstractMatcherEditor<String>() {
        private val testMatcher = Matcher<String> { true }

        fun constrain() = fireConstrained(testMatcher)
        fun relax() = fireRelaxed(testMatcher)
    }

    private class CapturingExecutor : Executor {
        private var pending: Runnable? = null

        override fun execute(command: Runnable) {
            check(pending == null)
            pending = command
        }

        fun runPending() {
            pending.also { pending = null }?.run()
        }
    }

    private class RejectOnceExecutor : Executor {
        private var reject = true
        private var pending: Runnable? = null

        override fun execute(command: Runnable) {
            if (reject) {
                reject = false
                throw IllegalStateException("rejected")
            }
            pending = command
        }

        fun runPending() {
            pending.also { pending = null }?.run()
        }
    }
}
