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

package mediathek.audiothek.ui.main

import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.builtins.serializer
import kotlinx.serialization.json.Json
import mediathek.tool.ApplicationConfiguration
import org.apache.commons.configuration2.Configuration

private const val MAX_HISTORY_ENTRIES = 50

internal class AudiothekSearchHistory(
    private val configuration: Configuration = ApplicationConfiguration.getConfiguration(),
) {
    fun load(): List<String> {
        val rawValue = configuration.getString(ApplicationConfiguration.APPLICATION_UI_AUDIOTHEK_SEARCH_HISTORY, "[]")
        return AudiothekSearchHistoryCodec.decode(rawValue)
    }

    fun save(entries: Collection<String>) {
        val normalized = AudiothekSearchHistoryCodec.normalize(entries)
        configuration.setProperty(
            ApplicationConfiguration.APPLICATION_UI_AUDIOTHEK_SEARCH_HISTORY,
            AudiothekSearchHistoryCodec.encode(normalized)
        )
    }
}

internal object AudiothekSearchHistoryCodec {
    fun normalize(entries: Collection<String>): List<String> {
        val seen = linkedSetOf<String>()
        entries.forEach { entry ->
            val normalized = entry.trim()
            if (normalized.isNotEmpty()) {
                seen += normalized
            }
        }
        return seen.take(MAX_HISTORY_ENTRIES)
    }

    fun encode(entries: Collection<String>): String =
        json.encodeToString(stringListSerializer, normalize(entries))

    fun decode(json: String?): List<String> {
        return runCatching {
            normalize(this.json.decodeFromString(stringListSerializer, json.orEmpty()))
        }.getOrDefault(emptyList())
    }

    private val json = Json {
        ignoreUnknownKeys = false
        isLenient = false
    }
    private val stringListSerializer = ListSerializer(String.serializer())
}
