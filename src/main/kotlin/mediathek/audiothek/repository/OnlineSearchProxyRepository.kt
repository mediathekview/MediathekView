/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.audiothek.repository

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext
import mediathek.audiothek.model.AudioEntry
import mediathek.config.Konstanten
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl.Companion.toHttpUrlOrNull
import okhttp3.OkHttpClient
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import tools.jackson.core.JsonParser
import tools.jackson.core.JsonToken
import tools.jackson.core.ObjectReadContext
import tools.jackson.core.json.JsonFactory
import java.io.InputStream
import java.net.URI
import java.time.LocalDateTime
import java.util.concurrent.TimeUnit

class OnlineSearchProxyRepository(
    private val client: OkHttpClient = MVHttpClient.getInstance().httpClient.newBuilder()
        .connectTimeout(Konstanten.AUDIOTHEK_SEARCH_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        .readTimeout(Konstanten.AUDIOTHEK_SEARCH_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        .writeTimeout(Konstanten.AUDIOTHEK_SEARCH_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        .build()
) {
    private val logger = LogManager.getLogger(OnlineSearchProxyRepository::class.java)
    private val jsonFactory = JsonFactory()

    suspend fun search(query: String): List<AudioEntry> = withContext(Dispatchers.IO) {
        val normalizedQuery = query.trim()
        if (normalizedQuery.isEmpty()) {
            return@withContext emptyList()
        }

        val requestUrl = Konstanten.AUDIOTHEK_ONLINE_SEARCH_PROXY_URL.toHttpUrlOrNull()
            ?.newBuilder()
            ?.addPathSegments("api/audiothek/podcast-search")
            ?.addQueryParameter("q", normalizedQuery)
            ?.build()
            ?: run {
                logger.warn(
                    "Ungültige Proxy-URL für die Audiothek-Onlinesuche: {}",
                    Konstanten.AUDIOTHEK_ONLINE_SEARCH_PROXY_URL
                )
                return@withContext emptyList()
            }

        val request = Request.Builder()
            .url(requestUrl)
            .header("User-Agent", readUserAgent())
            .header(
                Konstanten.AUDIOTHEK_PROXY_CLIENT_TOKEN_HEADER,
                Konstanten.AUDIOTHEK_PROXY_CLIENT_TOKEN
            )
            .get()
            .build()

        runCatching {
            client.newCall(request).execute().use { response ->
                if (!response.isSuccessful) {
                    logger.warn(
                        "Onlinesuche über Proxy fehlgeschlagen für '{}': HTTP {}",
                        normalizedQuery,
                        response.code
                    )
                    return@withContext emptyList()
                }

                val body = response.body

                body.byteStream().use(::parseEntries)
            }
        }.getOrElse { error ->
            logger.warn("Onlinesuche über Proxy fehlgeschlagen für '{}': {}", normalizedQuery, error.message)
            emptyList()
        }
    }

    private fun parseEntries(inputStream: InputStream): List<AudioEntry> {
        val entries = mutableListOf<AudioEntry>()
        jsonFactory.createParser(ObjectReadContext.empty(), inputStream).use { parser ->
            if (parser.nextToken() != JsonToken.START_OBJECT) {
                return emptyList()
            }

            while (parser.nextToken() != JsonToken.END_OBJECT) {
                val fieldName = parser.currentName() ?: continue
                val token = parser.nextToken()
                if (fieldName == "results" && token == JsonToken.START_ARRAY) {
                    while (parser.nextToken() != JsonToken.END_ARRAY) {
                        parseEntry(parser)?.let(entries::add)
                    }
                } else {
                    parser.skipChildren()
                }
            }
        }
        return entries
    }

    private fun parseEntry(parser: JsonParser): AudioEntry? {
        if (parser.currentToken() != JsonToken.START_OBJECT) {
            parser.skipChildren()
            return null
        }

        var channel = ""
        var genre = ""
        var theme = ""
        var title = ""
        var description = ""
        var audioUrl: URI? = null
        var websiteUrl: URI? = null
        var publishedAt: LocalDateTime? = null
        var durationMinutes: Int? = null
        var sizeMb: Int? = null
        var isPodcast = true

        while (parser.nextToken() != JsonToken.END_OBJECT) {
            val fieldName = parser.currentName() ?: continue
            parser.nextToken()
            when (fieldName) {
                "channel" -> channel = parser.getValueAsString("").orEmpty()
                "genre" -> genre = parser.getValueAsString("").orEmpty()
                "theme" -> theme = parser.getValueAsString("").orEmpty()
                "title" -> title = parser.getValueAsString("").orEmpty()
                "description" -> description = parser.getValueAsString("").orEmpty()
                "audioUrl" -> audioUrl = parseUri(parser.getValueAsString("").orEmpty())
                "websiteUrl" -> websiteUrl = parseUri(parser.getValueAsString("").orEmpty())
                "publishedAt" -> publishedAt = parseLocalDateTime(parser.getValueAsString("").orEmpty())
                "durationMinutes" -> durationMinutes = parser.getValueAsInt(0).takeIf { it >= 0 }
                "sizeMb" -> sizeMb = parser.getValueAsInt(0).takeIf { it >= 0 }
                "isPodcast" -> isPodcast = parser.getValueAsBoolean(true)
                else -> parser.skipChildren()
            }
        }

        if (title.isBlank()) {
            return null
        }

        return AudioEntry(
            channel = channel.ifBlank { "Podcastindex" },
            genre = genre,
            theme = theme,
            title = title,
            durationMinutes = durationMinutes,
            sizeMb = sizeMb,
            description = description,
            audioUrl = audioUrl,
            websiteUrl = websiteUrl,
            isNew = false,
            isPodcast = isPodcast,
            isDuplicate = false,
            publishedAt = publishedAt
        )
    }

    private fun readUserAgent(): String {
        return ApplicationConfiguration.getConfiguration()
            .getString(ApplicationConfiguration.APPLICATION_USER_AGENT, "MediathekView")
            .ifBlank { "MediathekView" }
    }

    private fun parseUri(value: String): URI? {
        if (value.isBlank()) {
            return null
        }

        return runCatching { URI(value) }.getOrNull()
    }

    private fun parseLocalDateTime(value: String): LocalDateTime? {
        if (value.isBlank()) {
            return null
        }

        return runCatching { LocalDateTime.parse(value) }.getOrNull()
    }
}
