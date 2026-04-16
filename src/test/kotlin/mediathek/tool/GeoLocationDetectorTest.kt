package mediathek.tool

import mediathek.daten.Country
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

internal class GeoLocationDetectorTest {
    @Test
    fun mapsSupportedCountryCodesDirectly() {
        assertEquals(Country.DE, GeoLocationDetector.mapCountryCode("DE", "EU"))
        assertEquals(Country.AT, GeoLocationDetector.mapCountryCode("AT", "EU"))
        assertEquals(Country.CH, GeoLocationDetector.mapCountryCode("CH", "EU"))
        assertEquals(Country.FR, GeoLocationDetector.mapCountryCode("FR", "EU"))
    }

    @Test
    fun mapsOtherEuropeanCountriesToEu() {
        assertEquals(Country.EU, GeoLocationDetector.mapCountryCode("NL", "EU"))
        assertEquals(Country.EU, GeoLocationDetector.mapCountryCode("es", "eu"))
    }

    @Test
    fun mapsNonEuropeanCountriesToOther() {
        assertEquals(Country.OTHER, GeoLocationDetector.mapCountryCode("US", "NA"))
        assertEquals(Country.OTHER, GeoLocationDetector.mapCountryCode(null, null))
    }
}
