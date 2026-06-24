package mediathek.filmlisten

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.cancel
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
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
    fun `notifyFinished calls fertig for every completion but fertigOnlyOne once`() {
        val dispatcher = FilmListLoadEventDispatcher(scope)
        val fertigCount = AtomicInteger(0)
        val fertigOnlyOneCount = AtomicInteger(0)
        val completionsDelivered = CountDownLatch(3)
        dispatcher.addListener(
            object : ListenerFilmeLaden() {
                override fun fertig(event: ListenerFilmeLadenEvent) {
                    fertigCount.incrementAndGet()
                    completionsDelivered.countDown()
                }

                override fun fertigOnlyOne(event: ListenerFilmeLadenEvent) {
                    fertigOnlyOneCount.incrementAndGet()
                    completionsDelivered.countDown()
                }
            },
        )

        dispatcher.notifyFinished(ListenerFilmeLadenEvent("", "", 100, 100, false))
        dispatcher.notifyFinished(ListenerFilmeLadenEvent("", "", 100, 100, false))

        assertTrue(completionsDelivered.await(5, TimeUnit.SECONDS))
        assertEquals(2, fertigCount.get())
        assertEquals(1, fertigOnlyOneCount.get())
    }

    @Test
    fun `removeListener stops later notifications`() {
        val dispatcher = FilmListLoadEventDispatcher(scope)
        val startCount = AtomicInteger(0)
        val listener = object : ListenerFilmeLaden() {
            override fun start(event: ListenerFilmeLadenEvent) {
                startCount.incrementAndGet()
            }
        }

        dispatcher.addListener(listener)
        dispatcher.removeListener(listener)
        dispatcher.notifyStart(ListenerFilmeLadenEvent("", "", 100, 0, false))

        Thread.sleep(200)
        assertEquals(0, startCount.get())
    }
}
