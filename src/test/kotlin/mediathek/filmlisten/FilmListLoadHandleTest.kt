package mediathek.filmlisten

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.runBlocking
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class FilmListLoadHandleTest {
    @Test
    fun `started handle exposes completion result`() = runBlocking {
        val result = FilmListLoadResult.finished(failed = false)
        val completion = CompletableDeferred(result)

        val handle = FilmListLoadHandle.started(completion)

        assertTrue(handle.started)
        assertSame(result, handle.completion.await())
    }

    @Test
    fun `skipped handle is already completed successfully`() = runBlocking {
        val handle = FilmListLoadHandle.skipped()

        assertFalse(handle.started)
        val result = handle.completion.await()
        assertFalse(result.failed)
        assertTrue(result.skipped)
    }
}
