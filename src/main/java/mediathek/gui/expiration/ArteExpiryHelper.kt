/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.expiration

import kotlinx.serialization.json.*
import mediathek.config.Konstanten
import mediathek.tool.datum.DateUtil
import mediathek.tool.http.MVHttpClient
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import org.jsoup.Jsoup
import java.io.IOException
import java.net.URI
import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import java.util.*
import java.util.regex.Pattern

object ArteExpiryHelper {
    private val LOG = LogManager.getLogger()
    private val JSON = Json {
        ignoreUnknownKeys = true
        isLenient = true
    }

    // Multilingual pattern for DE, EN, FR, ES, IT, PL
    private val TEXT_DATE_PATTERN: Pattern = Pattern.compile(
        "(Verfügbar bis zum|Available until|Disponible hasta el|Disponible jusqu'?au|Disponibile fino al|Dostępny do)\\s*(\\d{2}/\\d{2}/\\d{4})"
    )

    @JvmStatic
    fun getExpiryInfo(url: String): Optional<ExpiryInfo> {
        val request = Request.Builder()
            .url(url)
            .get()
            .header("User-Agent", Konstanten.JSOUP_USER_AGENT)
            .build()

        try {
            MVHttpClient.getInstance().httpClient.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    LOG.error("Could not fetch expiry data from {}", url)
                    return Optional.empty()
                }

                val doc = Jsoup.parse(response.body.string(), url)

                // 1) Search JSON-LD scripts
                for (script in doc.select("script[type=application/ld+json]")) {
                    parseExpiryInfo(script.html())?.let { return Optional.of(it) }
                }

                // 2) API endpoint via data-configuration
                val video = doc.selectFirst("video[data-configuration], video[data-href]")
                if (video != null) {
                    val cfgUrl = if (video.hasAttr("data-configuration")) {
                        video.attr("data-configuration")
                    } else {
                        video.attr("data-href")
                    }
                    if (cfgUrl.isNotBlank()) {
                        val resolvedCfgUrl = resolveConfigUrl(url, cfgUrl)
                        if (resolvedCfgUrl == null) {
                            LOG.debug("Ignoring invalid ARTE config URL '{}' (page '{}')", cfgUrl, url)
                        } else {
                            fetchJsonBody(resolvedCfgUrl)?.let { cfgBody ->
                                parseAvailabilityEnds(cfgBody)?.let { return Optional.of(buildInfo(it)) }
                            }
                        }
                    }
                }

                // 3) Visible multilingual text hint
                val bodyText = doc.body().text()
                val matcher = TEXT_DATE_PATTERN.matcher(bodyText)
                if (matcher.find()) {
                    val rawDate = matcher.group(2) // DD/MM/YYYY
                    try {
                        val date = LocalDate.parse(rawDate, DateTimeFormatter.ofPattern("dd/MM/yyyy"))
                        val expiry = date.atTime(23, 59).atZone(ZoneId.of("Europe/Paris")).toInstant()
                        return Optional.of(buildInfo(expiry))
                    } catch (ex: DateTimeParseException) {
                        LOG.debug("Unable to parse ARTE text expiry date '{}' from '{}'", rawDate, url, ex)
                    }
                }
            }
        } catch (ex: IOException) {
            LOG.debug("Failed to fetch ARTE expiry info from '{}'", url, ex)
        } catch (ex: IllegalArgumentException) {
            LOG.debug("Invalid ARTE URL '{}'", url, ex)
        }

        return Optional.empty()
    }

    private fun parseExpiryInfo(rawJson: String): ExpiryInfo? {
        val root = runCatching { JSON.parseToJsonElement(rawJson) }.getOrNull() ?: return null
        val expiry = findExpiryInstant(root) ?: return null
        return buildInfo(expiry)
    }

    private fun parseAvailabilityEnds(rawJson: String): Instant? {
        val root = runCatching { JSON.parseToJsonElement(rawJson) }.getOrNull() ?: return null
        val availability = (root as? JsonObject)?.get("availability") as? JsonObject ?: return null
        val value = asStringValue(availability["availabilityEnds"]) ?: return null
        return parseInstantOrNull(value)
    }

    private fun fetchJsonBody(url: String): String? {
        val request = Request.Builder()
            .url(url)
            .get()
            .header("User-Agent", Konstanten.JSOUP_USER_AGENT)
            .build()

        return try {
            MVHttpClient.getInstance().httpClient.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    LOG.debug("Could not fetch ARTE config JSON from {}", url)
                    null
                } else {
                    response.body.string()
                }
            }
        } catch (ex: IOException) {
            LOG.debug("Failed to fetch ARTE config JSON from '{}'", url, ex)
            null
        } catch (ex: IllegalArgumentException) {
            LOG.debug("Invalid ARTE config URL '{}'", url, ex)
            null
        }
    }

    private fun resolveConfigUrl(pageUrl: String, cfgUrl: String): String? {
        val resolved = runCatching { URI(pageUrl).resolve(cfgUrl).normalize() }.getOrNull() ?: return null
        val scheme = resolved.scheme?.lowercase() ?: return null
        if (scheme != "http" && scheme != "https") {
            return null
        }
        if (resolved.host.isNullOrBlank()) {
            return null
        }
        return resolved.toString()
    }

    // Recursively search for "availabilityEnds" or "expires" in arbitrary JSON-LD structures.
    private fun findExpiryInstant(node: JsonElement): Instant? {
        return when (node) {
            is JsonObject -> {
                parseInstantOrNull(asStringValue(node["availabilityEnds"]))
                    ?: parseInstantOrNull(asStringValue(node["expires"]))
                    ?: run {
                        for (child in node.values) {
                            val found = findExpiryInstant(child)
                            if (found != null) return found
                        }
                        null
                    }
            }

            is JsonArray -> {
                for (child in node) {
                    val found = findExpiryInstant(child)
                    if (found != null) return found
                }
                null
            }

            else -> null
        }
    }

    private fun parseInstantOrNull(value: String?): Instant? =
        value?.let { runCatching { Instant.parse(it) }.getOrNull() }

    private fun asStringValue(element: JsonElement?): String? =
        (element as? JsonPrimitive)?.content

    private fun buildInfo(expiry: Instant): ExpiryInfo =
        ExpiryInfo(LocalDate.ofInstant(expiry, DateUtil.MV_DEFAULT_TIMEZONE))
}
