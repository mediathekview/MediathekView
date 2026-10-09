package mediathek.logging

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class LogPathRedactorTest {
    @Test
    fun `redacts unix style paths`() {
        val text = "Saved to /Users/tester/work/output/file.txt and cache /tmp/downloads/cache.db"

        val redacted = LogPathRedactor.redact(text)

        assertEquals("Saved to <redacted-path>/file.txt and cache <redacted-path>/cache.db", redacted)
    }

    @Test
    fun `redacts home relative paths`() {
        val text = "Reading ~/.config/mediathek/settings.json"

        val redacted = LogPathRedactor.redact(text)

        assertEquals("Reading <redacted-path>/settings.json", redacted)
    }

    @Test
    fun `redacts windows style paths`() {
        val text = """Executable at C:\Users\tester\AppData\Local\app\tool.exe failed"""

        val redacted = LogPathRedactor.redact(text)

        assertEquals("""Executable at <redacted-path>\tool.exe failed""", redacted)
    }

    @Test
    fun `does not redact urls`() {
        val text = "Fetching https://example.org/media/file.mp4 from server"

        val redacted = LogPathRedactor.redact(text)

        assertEquals(text, redacted)
    }
}
