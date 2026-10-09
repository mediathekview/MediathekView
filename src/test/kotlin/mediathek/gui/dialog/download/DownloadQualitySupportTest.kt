package mediathek.gui.dialog.download

import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.tool.FileSize
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class DownloadQualitySupportTest {
    @Test
    fun `size lookup skips absent high and low quality urls`() {
        val film = DatenFilm().apply {
            urlNormalQuality = "https://example.org/video/master.m3u8"
        }
        val requestedResolutions = ArrayList<FilmResolution.Enum>()

        val result = DownloadQualitySupport.loadResolutionSizeResult(film) { resolution ->
            requestedResolutions += resolution
            FileSize.LookupResult(12_000_000)
        }

        assertEquals(listOf(FilmResolution.Enum.NORMAL), requestedResolutions)
        assertEquals(
            DownloadQualityResolutionSizes(
                high = "",
                normal = "12",
                low = "",
            ),
            result.sizes,
        )
    }

    @Test
    fun `size lookup probes high and low quality urls when present`() {
        val film = DatenFilm().apply {
            urlNormalQuality = "https://example.org/video/normal.mp4"
            highQualityUrl = "https://example.org/video/high.mp4"
            lowQualityUrl = "https://example.org/video/low.mp4"
        }
        val requestedResolutions = ArrayList<FilmResolution.Enum>()

        val result = DownloadQualitySupport.loadResolutionSizeResult(film) { resolution ->
            requestedResolutions += resolution
            FileSize.LookupResult(
                when (resolution) {
                    FilmResolution.Enum.HIGH_QUALITY -> 30_000_000
                    FilmResolution.Enum.NORMAL -> 20_000_000
                    FilmResolution.Enum.LOW -> 10_000_000
                },
            )
        }

        assertEquals(
            listOf(
                FilmResolution.Enum.HIGH_QUALITY,
                FilmResolution.Enum.NORMAL,
                FilmResolution.Enum.LOW,
            ),
            requestedResolutions,
        )
        assertEquals(
            DownloadQualityResolutionSizes(
                high = "30",
                normal = "20",
                low = "10",
            ),
            result.sizes,
        )
    }
}
