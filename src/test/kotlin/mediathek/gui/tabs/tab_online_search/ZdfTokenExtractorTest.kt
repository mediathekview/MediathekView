package mediathek.gui.tabs.tab_online_search

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import java.time.OffsetDateTime

class ZdfTokenExtractorTest {
    @Test
    fun `extracts token from escaped appToken JSON`() {
        val html = """
            <html><body><script>
            window.__CONFIG__ = "{\"appToken\":{\"apiToken\":\"abc123token\",\"expiresAt\":\"2026-06-16T06:01:42+02:00\"}}";
            </script></body></html>
        """.trimIndent()

        val token = ZdfTokenExtractor.extract(html)

        assertEquals("abc123token", token?.value)
        assertEquals(OffsetDateTime.parse("2026-06-16T06:01:42+02:00").toInstant(), token?.expiresAt)
    }

    @Test
    fun `returns null when token is absent`() {
        assertNull(ZdfTokenExtractor.extract("<html><body></body></html>"))
    }
}
