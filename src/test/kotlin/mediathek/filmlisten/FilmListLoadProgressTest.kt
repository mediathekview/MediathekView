package mediathek.filmlisten

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class FilmListLoadProgressTest {
    @Test
    fun `completed creates successful completion progress`() {
        val progress = FilmListLoadProgress.completed(failed = false)

        assertEquals("", progress.senderUrl)
        assertEquals("", progress.text)
        assertEquals(100, progress.max)
        assertEquals(100, progress.progress)
        assertFalse(progress.failed)
    }

    @Test
    fun `completed creates failed completion progress`() {
        val progress = FilmListLoadProgress.completed(failed = true)

        assertEquals("", progress.senderUrl)
        assertEquals("", progress.text)
        assertEquals(100, progress.max)
        assertEquals(100, progress.progress)
        assertTrue(progress.failed)
    }

    @Test
    fun `started creates reader start progress`() {
        val progress = FilmListLoadProgress.started("https://example.invalid/list.json")

        assertEquals("https://example.invalid/list.json", progress.senderUrl)
        assertEquals("", progress.text)
        assertEquals(100, progress.max)
        assertEquals(0, progress.progress)
        assertFalse(progress.failed)
    }

    @Test
    fun `downloading creates reader download progress`() {
        val progress = FilmListLoadProgress.downloading("https://example.invalid/list.json", progress = 42)

        assertEquals("https://example.invalid/list.json", progress.senderUrl)
        assertEquals("Download", progress.text)
        assertEquals(100, progress.max)
        assertEquals(42, progress.progress)
        assertFalse(progress.failed)
    }

    @Test
    fun `finished creates reader finished progress`() {
        val progress = FilmListLoadProgress.finished("https://example.invalid/list.json", progress = 87)

        assertEquals("https://example.invalid/list.json", progress.senderUrl)
        assertEquals("", progress.text)
        assertEquals(100, progress.max)
        assertEquals(87, progress.progress)
        assertFalse(progress.failed)
    }
}
