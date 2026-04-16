package mediathek.tool

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.config.Konstanten
import mediathek.daten.Country
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.OkHttpClient
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import java.io.IOException

object GeoLocationDetector {
    private val logger = LogManager.getLogger(GeoLocationDetector::class.java)
    private val json = Json { ignoreUnknownKeys = true }

    fun detectCountry(): Country? =
        detectLocation()?.mappedCountry

    fun detectLocation(
        httpClient: OkHttpClient = MVHttpClient.getInstance().httpClient,
        userAgent: String = readUserAgent(),
    ): DetectedLocation? {
        return runCatching {
            val publicIp = Ipify.publicIp
            val request = Request.Builder()
                .url(
                    GEO_LOOKUP_URL.toHttpUrl().newBuilder()
                        .addPathSegment(publicIp)
                        .build(),
                )
                .header("Accept", "application/json")
                .header("User-Agent", userAgent)
                .get()
                .build()

            httpClient.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    throw IOException("Failed to fetch geo location: HTTP ${response.code}")
                }

                val body = response.body.string()
                val geoResponse = json.decodeFromString(GeoLookupResponse.serializer(), body)
                DetectedLocation(
                    countryCode = geoResponse.country.trim().uppercase(),
                    mappedCountry = mapCountryCode(geoResponse.country, geoResponse.continent),
                )
            }
        }.onFailure { ex ->
            logger.debug("Geo location detection failed", ex)
        }.getOrNull()
    }

    fun mapCountryCode(countryCode: String?, continent: String?): Country {
        return when (countryCode?.trim()?.uppercase()) {
            Country.DE.name -> Country.DE
            Country.AT.name -> Country.AT
            Country.CH.name -> Country.CH
            Country.FR.name -> Country.FR
            else -> {
                if (continent?.trim()?.uppercase() == "EU") {
                    Country.EU
                } else {
                    Country.OTHER
                }
            }
        }
    }

    private fun readUserAgent(): String {
        return ApplicationConfiguration.getConfiguration()
            .getString(ApplicationConfiguration.APPLICATION_USER_AGENT, Konstanten.PROGRAMMNAME)
            .ifBlank { Konstanten.PROGRAMMNAME }
    }

    @Serializable
    private data class GeoLookupResponse(
        val country: String,
        val continent: String = "",
    )

    data class DetectedLocation(
        val countryCode: String,
        val mappedCountry: Country,
    )

    private const val GEO_LOOKUP_URL = "https://geo.mediathekview.de/"
}
