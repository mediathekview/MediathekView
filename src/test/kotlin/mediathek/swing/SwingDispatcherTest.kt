package mediathek.swing

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.SwingUtilities

class SwingDispatcherTest {
    @Test
    fun `dispatch runs immediately when already on EDT`() {
        val completed = CountDownLatch(1)
        var executionOrder = "not run"

        SwingUtilities.invokeAndWait {
            SwingDispatch.dispatch {
                executionOrder = "dispatched"
            }
            executionOrder += " before return"
            completed.countDown()
        }

        assertTrue(completed.await(1, TimeUnit.SECONDS))
        assertEquals("dispatched before return", executionOrder)
    }

    @Test
    fun `dispatch from background thread runs action on EDT`() {
        val completed = CountDownLatch(1)
        val ranOnEdt = AtomicBoolean(false)

        SwingDispatch.dispatch {
            ranOnEdt.set(SwingUtilities.isEventDispatchThread())
            completed.countDown()
        }

        assertTrue(completed.await(1, TimeUnit.SECONDS))
        assertTrue(ranOnEdt.get())
    }
}
