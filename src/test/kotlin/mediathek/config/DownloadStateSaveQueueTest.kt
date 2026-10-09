package mediathek.config

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import kotlin.time.Duration.Companion.milliseconds

internal class DownloadStateSaveQueueTest {
    @Test
    fun rapidRequestsProduceOneSaveBeforeShutdown() {
        val saves = AtomicInteger()
        val queue = DownloadStateSaveQueue(saves::incrementAndGet, 100.milliseconds)

        repeat(20) { queue.requestSave() }
        queue.stopAndFlush()

        assertEquals(1, saves.get())
    }

    @Test
    fun slowSaveDoesNotBlockNewRequestsAndShutdownWaitsForLatestSave() {
        val saveStarted = CountDownLatch(1)
        val releaseSave = CountDownLatch(1)
        val saves = AtomicInteger()
        val queue = DownloadStateSaveQueue({
            saveStarted.countDown()
            assertTrue(releaseSave.await(5, TimeUnit.SECONDS))
            saves.incrementAndGet()
        }, 1.milliseconds)

        queue.requestSave()
        assertTrue(saveStarted.await(5, TimeUnit.SECONDS))
        queue.requestSave()
        releaseSave.countDown()
        queue.stopAndFlush()

        assertEquals(2, saves.get())
    }
}
