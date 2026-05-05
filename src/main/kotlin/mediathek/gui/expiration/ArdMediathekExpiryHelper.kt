/*
 * Copyright (c) 2025-2026 derreisende77.
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

import mediathek.config.Konstanten
import mediathek.tool.http.MVHttpClient
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import org.jsoup.Jsoup
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.*
import java.util.regex.Pattern

object ArdMediathekExpiryHelper {
    // Pattern fuer "Video verfuegbar: bis DD.MM.YYYY ∙ HH:MM Uhr"
    private val AVAILABLE_UNTIL_PATTERN: Pattern = Pattern.compile(
        "Video verfügbar:.*?bis\\s*(\\d{2}\\.\\d{2}\\.\\d{4})\\s*∙\\s*(\\d{2}:\\d{2})\\s*Uhr",
        Pattern.DOTALL
    )
    private val DATE_FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    private val LOG = LogManager.getLogger()

    fun getExpiryInfo(url: String): Optional<ExpiryInfo> {
        val request = Request.Builder()
            .url(url)
            .get()
            .header("User-Agent", Konstanten.JSOUP_USER_AGENT)
            .build()

        try {
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                if (response.isSuccessful) {
                    val doc = Jsoup.parse(response.body.string())
                    val body = doc.body().text()
                    val matcher = AVAILABLE_UNTIL_PATTERN.matcher(body)
                    if (matcher.find()) {
                        val date = matcher.group(1)
                        val expiryDate = LocalDate.parse(date, DATE_FORMATTER)
                        return Optional.of(ExpiryInfo(expiryDate))
                    }
                } else {
                    LOG.error("Could not fetch expiry data from {}", url)
                }
            }
        } catch (_: Exception) {
        }

        return Optional.empty()
    }
}
