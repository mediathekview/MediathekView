package mediathek.daten

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.time.LocalDateTime

internal class DatenDownloadTest {

    @Test
    fun stripDotsAndColons() {
        val download = DatenDownload()

        assertEquals("20250927", download.stripDotsAndColons("2025.09.27"))
        assertEquals("201500", download.stripDotsAndColons("20:15:00"))
    }

    @Test
    fun formatTimeRemaining() {
        val download = DatenDownload()

        assertEquals("6 Min.", download.formatTimeRemaining(360))
        assertEquals("5 Min.", download.formatTimeRemaining(240))
        assertEquals("4 Min.", download.formatTimeRemaining(180))
        assertEquals("3 Min.", download.formatTimeRemaining(120))
        assertEquals("2 Min.", download.formatTimeRemaining(70))
        assertEquals("1 Min.", download.formatTimeRemaining(40))
        assertEquals("30 s", download.formatTimeRemaining(25))
        assertEquals("20 s", download.formatTimeRemaining(15))
        assertEquals("10 s", download.formatTimeRemaining(8))
    }

    @Test
    fun replaceYearParameter() {
        val film = DatenFilm()
        val download = DatenDownload()
        val year = LocalDateTime.now().year.toString()

        assertEquals(year, download.replaceYearParameter("%3", film))
        assertEquals(year.takeLast(2), download.replaceYearParameter("%3_2", film))
    }
}
