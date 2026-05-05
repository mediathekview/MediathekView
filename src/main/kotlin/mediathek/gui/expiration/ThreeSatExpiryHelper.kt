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

object ThreeSatExpiryHelper {
    // Pattern: "Verfuegbar bis: bis DD.MM.YYYY"
    private val TEXT_DATE_PATTERN: Pattern = Pattern.compile(
        "(Verfügbar bis:?)\\s*(?:bis\\s*)?(\\d{2}\\.\\d{2}\\.\\d{4})"
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

                    val matcher = TEXT_DATE_PATTERN.matcher(body)
                    if (matcher.find()) {
                        val date = LocalDate.parse(matcher.group(2), DATE_FORMATTER)
                        return Optional.of(ExpiryInfo(date))
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
