package mediathek.gui.dialog.download

import mediathek.daten.DatenFilm
import mediathek.daten.FilmResolution
import mediathek.tool.FileSize
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.EnumSource
import org.junit.jupiter.params.provider.ValueSource

class DownloadQualitySupportTest {
    private val arteMasterUrl =
        "https://manifest-arte.akamaized.net/api/manifest/v1/Generate/123456-000-A/VOF/2/master.m3u8"

    @ParameterizedTest
    @ValueSource(
        strings = ["ARTE", "ARTE.DE", "ARTE.FR", "ARTE.EN", "ARTE.ES", "ARTE.PL", "ARTE.IT", "ARTE.RO", "arte.fr"]
    )
    fun `ARTE master enables all qualities without changing the film list`(sender: String) {
        val original = DatenFilm().apply {
            this.sender = sender
            urlNormalQuality = arteMasterUrl
        }

        val prepared = DownloadQualitySupport.prepareFilmForQualitySelection(original)

        assertEquals(arteMasterUrl, prepared.urlNormalQuality)
        assertEquals(arteMasterUrl, prepared.highQualityUrl)
        assertEquals(arteMasterUrl, prepared.lowQualityUrl)
        assertEquals("", original.highQualityUrl)
        assertEquals("", original.lowQualityUrl)
        val resolutions = ArrayList<FilmResolution.Enum>()
        DownloadQualitySupport.loadResolutionSizeResult(prepared) {
            resolutions += it
            FileSize.LookupResult(12_000_000)
        }
        assertEquals(
            listOf(FilmResolution.Enum.HIGH_QUALITY, FilmResolution.Enum.NORMAL, FilmResolution.Enum.LOW),
            resolutions,
        )
    }

    @ParameterizedTest
    @ValueSource(strings = ["ZDF", "Das Erste", "ARTE.DEU", "ARTEFACT"])
    fun `other senders are unchanged even with an ARTE URL`(sender: String) {
        val film = DatenFilm().apply {
            this.sender = sender
            urlNormalQuality = arteMasterUrl
        }
        assertSame(film, DownloadQualitySupport.prepareFilmForQualitySelection(film))
    }

    @ParameterizedTest
    @ValueSource(strings = ["https://example.org/master.m3u8", "https://arte.tv/video.mp4"])
    fun `ARTE URLs without quality program selection are unchanged`(url: String) {
        val film = DatenFilm().apply {
            sender = "ARTE.FR"
            urlNormalQuality = url
        }
        assertSame(film, DownloadQualitySupport.prepareFilmForQualitySelection(film))
    }

    @ParameterizedTest
    @EnumSource(value = FilmResolution.Enum::class, names = ["HIGH_QUALITY", "LOW"])
    fun `existing ARTE quality URLs are preserved`(existingResolution: FilmResolution.Enum) {
        val existingUrl = "https://example.org/existing.mp4"
        val film = DatenFilm().apply {
            sender = "ARTE.DE"
            urlNormalQuality = arteMasterUrl
            if (existingResolution == FilmResolution.Enum.HIGH_QUALITY) {
                highQualityUrl = existingUrl
            } else {
                lowQualityUrl = existingUrl
            }
        }
        val prepared = DownloadQualitySupport.prepareFilmForQualitySelection(film)
        assertEquals(existingUrl, prepared.getUrlFuerAufloesung(existingResolution))
        val missingResolution = if (existingResolution == FilmResolution.Enum.HIGH_QUALITY) {
            FilmResolution.Enum.LOW
        } else {
            FilmResolution.Enum.HIGH_QUALITY
        }
        assertEquals(arteMasterUrl, prepared.getUrlFuerAufloesung(missingResolution))
        assertEquals("", if (missingResolution == FilmResolution.Enum.LOW) film.lowQualityUrl else film.highQualityUrl)
        assertSame(prepared, DownloadQualitySupport.prepareFilmForQualitySelection(prepared))
    }

    @Test
    fun `ARTE films with all quality URLs are unchanged`() {
        val film = DatenFilm().apply {
            sender = "ARTE.DE"
            urlNormalQuality = arteMasterUrl
            highQualityUrl = "https://example.org/high.mp4"
            lowQualityUrl = "https://example.org/low.mp4"
        }
        assertSame(film, DownloadQualitySupport.prepareFilmForQualitySelection(film))
    }

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
