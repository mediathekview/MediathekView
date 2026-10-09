package mediathek.gui.tabs.tab_online_search

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.time.Duration
import java.time.LocalDateTime

class OnlineSearchFilmAdapterTest {
    @Test
    fun `converts result to DatenFilm only at action boundary`() {
        val source = OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = "Tatort",
            title = "Borowski und der gute Mensch",
            description = "Beschreibung",
            websiteUrl = "https://www.ardmediathek.de/video/example",
            normalQualityUrl = "https://cdn.example/video.mp4",
            lowQualityUrl = "https://cdn.example/video-low.mp4",
            highQualityUrl = "https://cdn.example/video-hd.mp4",
            subtitleUrl = "https://cdn.example/sub.vtt",
            broadcastTime = LocalDateTime.of(2026, 1, 2, 20, 15),
            duration = Duration.ofMinutes(89),
        )

        val film = OnlineSearchFilmAdapter.toDatenFilm(source)

        assertEquals("ARD", film.sender)
        assertEquals("Tatort", film.thema)
        assertEquals("Borowski und der gute Mensch", film.title)
        assertEquals("Beschreibung", film.description)
        assertEquals("https://www.ardmediathek.de/video/example", film.websiteUrl)
        assertEquals("https://cdn.example/video.mp4", film.urlNormalQuality)
        assertEquals("https://cdn.example/video-low.mp4", film.lowQualityUrl)
        assertEquals("https://cdn.example/video-hd.mp4", film.highQualityUrl)
        assertTrue(film.hasSubtitle())
        assertEquals(89 * 60, film.filmLength)
        assertEquals("02.01.2026", film.sendeDatum)
        assertEquals("20:15:00", film.sendeZeit)
    }
}
