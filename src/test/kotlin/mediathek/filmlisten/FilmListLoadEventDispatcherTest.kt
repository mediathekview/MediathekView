package mediathek.filmlisten

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

class FilmListLoadEventDispatcherTest {
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.Default)

    @AfterEach
    fun cancelScope() {
        scope.cancel()
    }

    @Test
    fun `notifyFinished calls loadFinished for every completion but firstLoadFinished only for the first`() {
        val dispatcher = FilmListLoadEventDispatcher(scope)
        val finishedProgress = mutableListOf<FilmListLoadProgress>()
        val firstFinishedProgress = mutableListOf<FilmListLoadProgress>()
        val completionsDelivered = CountDownLatch(3)
        dispatcher.addListener(
            object : FilmListLoadListener {
                override fun loadFinished(progress: FilmListLoadProgress) {
                    finishedProgress += progress
                    completionsDelivered.countDown()
                }

                override fun firstLoadFinished(progress: FilmListLoadProgress) {
                    firstFinishedProgress += progress
                    completionsDelivered.countDown()
                }
            },
        )
        val firstProgress = progress(progress = 75)
        val secondProgress = progress(progress = 100)

        dispatcher.notifyFinished(firstProgress)
        dispatcher.notifyFinished(secondProgress)

        assertTrue(completionsDelivered.await(5, TimeUnit.SECONDS))
        assertEquals(listOf(firstProgress, secondProgress), finishedProgress)
        assertEquals(listOf(firstProgress), firstFinishedProgress)
    }

    @Test
    fun `removeListener stops later notifications`() {
        val dispatcher = FilmListLoadEventDispatcher(scope)
        val startCount = AtomicInteger(0)
        val listener = object : FilmListLoadListener {
            override fun loadStarted(progress: FilmListLoadProgress) {
                startCount.incrementAndGet()
            }
        }

        dispatcher.addListener(listener)
        dispatcher.removeListener(listener)
        dispatcher.notifyStart(progress(progress = 0))

        Thread.sleep(200)
        assertEquals(0, startCount.get())
    }

    private fun progress(progress: Int = 100): FilmListLoadProgress =
        FilmListLoadProgress("", "", 100, progress, failed = false)
}
