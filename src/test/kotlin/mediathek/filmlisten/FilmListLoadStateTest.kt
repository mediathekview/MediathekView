package mediathek.filmlisten

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class FilmListLoadStateTest {
    @Test
    fun `load remains running while post load work is active`() {
        val state = FilmListLoadState()

        assertTrue(state.tryBegin())
        state.startPostLoad()

        assertTrue(state.isRunning)
        assertFalse(state.tryBegin())

        state.finish()

        assertFalse(state.isRunning)
        assertTrue(state.tryBegin())
    }

    @Test
    fun `post load work cannot reopen an idle load`() {
        val state = FilmListLoadState()

        assertThrows(IllegalStateException::class.java) {
            state.startPostLoad()
        }

        assertFalse(state.isRunning)
    }
}
