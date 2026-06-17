package mediathek.daten

import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.FileSize
import okhttp3.HttpUrl.Companion.toHttpUrl
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class DatenFilmGeoBlockingTest {

    @Test
    fun filmWithoutCountriesIsNotGeoBlocked() {
        val film = DatenFilm()

        assertFalse(film.isGeoBlockedForLocation(Country.DE))
    }

    @Test
    fun euFilmIsNotBlockedForEuLocation() {
        val film = DatenFilm()
        film.addCountry(Country.EU)

        assertFalse(film.isGeoBlockedForLocation(Country.DE))
    }

    @Test
    fun euFilmIsBlockedOutsideEu() {
        val film = DatenFilm()
        film.addCountry(Country.EU)

        assertTrue(film.isGeoBlockedForLocation(Country.OTHER))
    }

    @Test
    fun countrySpecificFilmIsBlockedOutsideAllowedCountry() {
        val film = DatenFilm()
        film.addCountry(Country.DE)

        assertFalse(film.isGeoBlockedForLocation(Country.DE))
        assertTrue(film.isGeoBlockedForLocation(Country.AT))
    }

    @Test
    fun forbiddenHlsSizeLookupMarksConfiguredCountryBlocked() {
        val previousLocation = ApplicationConfiguration.getInstance().geographicLocation
        ApplicationConfiguration.getInstance().geographicLocation = Country.AT
        try {
            val film = DatenFilm()
            val url = "https://example.org/video/master.m3u8"

            film.applyFileSizeLookupResult(
                url,
                FileSize.LookupResult(
                    byteLength = FileSize.INVALID_SIZE.toLong(),
                    httpStatusCode = 403,
                    resolutionUrl = "https://example.org/video/chunklist.m3u8".toHttpUrl(),
                    quality = FilmResolution.Enum.HIGH_QUALITY.name,
                ),
            )

            assertFalse(film.hasCountries())
            assertFalse(film.isGeoBlockedForLocation(Country.DE))
            assertTrue(film.isGeoBlockedForLocation(Country.AT))
            assertEquals(
                403,
                film.lookupFileSizeForUrl(url, false, FilmResolution.Enum.HIGH_QUALITY.name).httpStatusCode,
            )
        } finally {
            ApplicationConfiguration.getInstance().geographicLocation = previousLocation
        }
    }
}
