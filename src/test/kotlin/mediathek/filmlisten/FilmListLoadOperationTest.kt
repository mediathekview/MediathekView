package mediathek.filmlisten

import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

class FilmListLoadOperationTest {
    @Test
    fun `begin returns skipped handle when a load is already running`() = runBlocking {
        val state = FilmListLoadState()
        val firstOperation = FilmListLoadOperation.begin(state)

        val secondOperation = FilmListLoadOperation.begin(state)

        assertTrue(firstOperation.handle.started)
        assertFalse(secondOperation.handle.started)
        assertTrue(secondOperation.handle.completion.await().skipped)
        firstOperation.finish(FilmListLoadProgress.completed(failed = false))
    }

    @Test
    fun `finish completes the handle result and releases the running state`() = runBlocking {
        val state = FilmListLoadState()
        val operation = FilmListLoadOperation.begin(state)

        operation.finish(FilmListLoadProgress.completed(failed = true))

        assertTrue(operation.handle.completion.await().failed)
        assertFalse(state.isRunning)
    }

    @Test
    fun `startPostLoad keeps the load running until finish`() {
        val state = FilmListLoadState()
        val operation = FilmListLoadOperation.begin(state)

        operation.startPostLoad()

        assertTrue(state.isRunning)
        operation.finish(FilmListLoadProgress.completed(failed = false))
        assertFalse(state.isRunning)
    }
}
