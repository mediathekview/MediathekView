package mediathek.tool.notification

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicReference
import javax.swing.SwingUtilities
import kotlin.time.Duration.Companion.seconds

class GenericNotificationCenterTest {
    @Test
    fun `close suppresses notification waiting on EDT`() {
        val edtBlocked = CountDownLatch(1)
        val releaseEdt = CountDownLatch(1)
        val closeStarted = CountDownLatch(1)
        val closeThread = AtomicReference<Thread>()
        val displays = AtomicInteger()
        val notificationCenter = GenericNotificationCenter { displays.incrementAndGet() }
        val executor = Executors.newSingleThreadExecutor()

        try {
            SwingUtilities.invokeLater {
                edtBlocked.countDown()
                check(releaseEdt.await(5, TimeUnit.SECONDS)) { "EDT was not released" }
            }
            assertTrue(edtBlocked.await(5, TimeUnit.SECONDS), "EDT blocker did not start")

            notificationCenter.publish(NotificationMessage("Title", "Message", MessageType.INFO))
            val close = executor.submit {
                closeThread.set(Thread.currentThread())
                closeStarted.countDown()
                notificationCenter.close()
            }
            assertTrue(closeStarted.await(5, TimeUnit.SECONDS), "Notification close did not start")
            waitUntilBlocked(closeThread.get())
            releaseEdt.countDown()
            close.get(5, TimeUnit.SECONDS)
            SwingUtilities.invokeAndWait {}

            assertEquals(0, displays.get())
        } finally {
            releaseEdt.countDown()
            executor.shutdownNow()
        }
    }

    private fun waitUntilBlocked(thread: Thread) {
        val deadline = System.nanoTime() + 5.seconds.inWholeNanoseconds
        while (thread.state !in BLOCKED_STATES) {
            check(System.nanoTime() < deadline) { "Notification close did not wait for the EDT" }
            Thread.onSpinWait()
        }
    }

    private companion object {
        private val BLOCKED_STATES = setOf(Thread.State.BLOCKED, Thread.State.WAITING, Thread.State.TIMED_WAITING)
    }
}
