package mediathek.tool

import mediathek.daten.Country
import okhttp3.Interceptor
import okhttp3.MediaType.Companion.toMediaType
import okhttp3.OkHttpClient
import okhttp3.Protocol
import okhttp3.Response
import okhttp3.ResponseBody.Companion.toResponseBody
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test
import java.net.SocketTimeoutException

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

    @Test
    fun detectLocationUsesInjectedHttpClientForIpAndGeoLookup() {
        val client = OkHttpClient.Builder()
            .addInterceptor(
                Interceptor { chain ->
                    val request = chain.request()
                    when (request.url.host) {
                        "api64.ipify.org" -> jsonResponse(chain, """{"ip":"203.0.113.5"}""")
                        "geo.mediathekview.de" -> {
                            assertEquals("/203.0.113.5", request.url.encodedPath)
                            jsonResponse(chain, """{"country":"AT","continent":"EU"}""")
                        }
                        else -> error("Unexpected URL ${request.url}")
                    }
                },
            )
            .build()

        val detectedLocation = GeoLocationDetector.detectLocation(client, "MediathekView-Test")

        assertEquals("AT", detectedLocation?.countryCode)
        assertEquals(Country.AT, detectedLocation?.mappedCountry)
    }

    @Test
    fun detectLocationReturnsNullOnIpLookupTimeout() {
        val client = OkHttpClient.Builder()
            .addInterceptor { throw SocketTimeoutException("timeout") }
            .build()

        assertNull(GeoLocationDetector.detectLocation(client, "MediathekView-Test"))
    }

    private fun jsonResponse(chain: Interceptor.Chain, body: String): Response {
        return Response.Builder()
            .request(chain.request())
            .protocol(Protocol.HTTP_1_1)
            .code(200)
            .message("OK")
            .body(body.toResponseBody("application/json".toMediaType()))
            .build()
    }
}
