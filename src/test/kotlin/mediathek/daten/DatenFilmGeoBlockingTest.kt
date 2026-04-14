package mediathek.daten

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
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
}
