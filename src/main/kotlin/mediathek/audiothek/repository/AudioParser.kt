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

import mediathek.audiothek.model.AudioDataset
import mediathek.audiothek.model.AudioEntry
import tools.jackson.core.JsonParser
import tools.jackson.core.JsonToken
import tools.jackson.core.ObjectReadContext
import tools.jackson.core.json.JsonFactory
import java.io.InputStream
import java.net.URI
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.format.DateTimeFormatter

class AudioParser {
    private var lastChannel = ""
    private var lastGenre = ""
    private var lastTheme = ""
    private val jsonFactory = JsonFactory()

    fun parse(inputStream: InputStream, sourceUrl: String): AudioDataset {
        jsonFactory.createParser(ObjectReadContext.empty(), inputStream).use { parser ->
            require(parser.nextToken() == JsonToken.START_OBJECT) {
                "Expected audio payload to start with an object"
            }

            var metaLocal: LocalDateTime? = null
            val entries = mutableListOf<AudioEntry>()
            var skippedHeaderRow = false
            resetCompressionState()

            while (parser.nextToken() != JsonToken.END_OBJECT) {
                val fieldName = parser.currentName()
                require(!fieldName.isNullOrBlank()) {
                    "Expected audio payload object field but got ${parser.currentToken()}"
                }
                val token = parser.nextToken()
                when (fieldName) {
                    AUDIO_META_TAG if token == JsonToken.START_ARRAY -> {
                        metaLocal = parseDatasetMetadata(parser)
                    }
                    AUDIO_ROWS_TAG if token == JsonToken.START_ARRAY -> {
                        parseAudioRowsField(parser) { row ->
                            if (!skippedHeaderRow && row == AUDIO_HEADER_ROW) {
                                skippedHeaderRow = true
                            } else {
                                entries += mapRowToEntry(row)
                            }
                        }
                    }
                    else -> parser.skipChildren()
                }
            }

            return AudioDataset(
                metaLocal = metaLocal,
                sqliteMetaLocal = null,
                sourceUrl = sourceUrl,
                entries = entries
            )
        }
    }

    private fun parseNestedAudioRow(parser: JsonParser): List<String> {
        val fields = mutableListOf<String>()
        var index = 0
        while (parser.nextToken() != JsonToken.END_ARRAY) {
            if (index < JSON_MAX_ELEM) {
                fields += parser.getValueAsString("").orEmpty()
            }
            index++
        }
        return fields
    }

    private fun parseFlatAudioRow(parser: JsonParser): List<String> {
        val fields = mutableListOf<String>()
        var index = 0
        do {
            if (index < JSON_MAX_ELEM) {
                fields += parser.getValueAsString("").orEmpty()
            }
            index++
        } while (parser.nextToken() != JsonToken.END_ARRAY)
        return fields
    }

    private fun parseAudioRowsField(parser: JsonParser, consumer: (List<String>) -> Unit) {
        val firstToken = parser.nextToken()
        when (firstToken) {
            JsonToken.END_ARRAY -> return
            JsonToken.START_ARRAY -> {
                consumer(parseNestedAudioRow(parser))
                while (parser.nextToken() != JsonToken.END_ARRAY) {
                    require(parser.currentToken() == JsonToken.START_ARRAY) {
                        "Expected audio row array but got ${parser.currentToken()}"
                    }
                    consumer(parseNestedAudioRow(parser))
                }
            }
            else -> consumer(parseFlatAudioRow(parser))
        }
    }

    private fun parseDatasetMetadata(parser: JsonParser): LocalDateTime? {
        var index = 0
        var datasetTimestamp: LocalDateTime? = null
        while (parser.nextToken() != JsonToken.END_ARRAY) {
            val value = parser.getValueAsString("").orEmpty()
            if (index == 1) {
                datasetTimestamp = parseDatasetTimestamp(value)
            }
            index++
        }
        return datasetTimestamp
    }

    private fun mapRowToEntry(row: List<String>): AudioEntry {
        val fields = Array(JSON_MAX_ELEM) { index -> row.getOrElse(index) { "" } }
        val channel = if (fields[0].isNotBlank()) {
            lastChannel = fields[0]
            lastChannel
        } else {
            lastChannel
        }
        val theme = if (fields[1].isNotBlank()) {
            lastTheme = fields[1]
            lastTheme
        } else {
            lastTheme
        }
        val genre = if (fields[2].isNotBlank()) {
            lastGenre = fields[2]
            lastGenre
        } else {
            lastGenre
        }
        val title = fields[3]
        val date = fields[4]
        val time = fields[5]
        val duration = parseDurationMinutes(fields[6])
        val sizeMb = fields[7].toIntOrNull()
        val description = fields[8]
        val audioUrl = parseUri(fields[9])
        val websiteUrl = parseUri(fields[10])

        return AudioEntry(
            sourceLabel = AudioSourceLabels.P2TOOLS,
            channel = channel,
            genre = genre,
            theme = when {
                theme.isNotBlank() -> theme
                genre.isNotBlank() -> genre
                else -> ""
            },
            title = title,
            durationMinutes = duration,
            sizeMb = sizeMb,
            description = description,
            audioUrl = audioUrl,
            websiteUrl = websiteUrl,
            isNew = parseBooleanFlag(fields[11]),
            isPodcast = parseBooleanFlag(fields[12]),
            isDuplicate = parseBooleanFlag(fields[13]),
            publishedAt = parseDateTime(date, time)
        )
    }

    private fun resetCompressionState() {
        lastChannel = ""
        lastGenre = ""
        lastTheme = ""
    }

    private fun parseBooleanFlag(value: String): Boolean {
        return value.equals("true", ignoreCase = true) || value == "1"
    }

    private fun parseUri(value: String): URI? {
        if (value.isBlank()) {
            return null
        }
        return runCatching { URI(value) }.getOrNull()
    }

    private fun parseDurationMinutes(value: String): Int? {
        if (value.isBlank()) {
            return null
        }

        return if (value.contains(':')) {
            val totalSeconds = runCatching {
                value.split(':')
                    .fold(0L) { acc, part -> acc * 60 + part.toLong() }
            }.getOrNull() ?: return null
            secondsToMinutes(totalSeconds)
        } else {
            val totalSeconds = value.toLongOrNull() ?: return null
            secondsToMinutes(totalSeconds)
        }
    }

    private fun secondsToMinutes(totalSeconds: Long): Int {
        if (totalSeconds <= 0L) {
            return 0
        }
        val minutes = (totalSeconds / 60L).toInt()
        return if (minutes <= 0) 1 else minutes
    }

    private fun parseDateTime(date: String, time: String): LocalDateTime? {
        val parsedDate = runCatching { LocalDate.parse(date, DATE_FORMAT) }.getOrNull() ?: return null
        val normalizedTime = when {
            time.isBlank() -> "00:00"
            time.length == 5 -> time
            time.length >= 5 -> time.take(5)
            else -> return null
        }
        val parsedTime = runCatching { LocalTime.parse(normalizedTime, TIME_FORMAT) }.getOrNull() ?: return null
        return LocalDateTime.of(parsedDate, parsedTime)
    }

    private fun parseDatasetTimestamp(value: String): LocalDateTime? {
        return runCatching { LocalDateTime.parse(value, DATASET_TIMESTAMP_FORMAT) }.getOrNull()
    }

    companion object {
        private const val AUDIO_META_TAG = "AudioList"
        private const val AUDIO_ROWS_TAG = "Audios"
        private const val JSON_MAX_ELEM = 14
        private val AUDIO_HEADER_ROW = listOf(
            "Sender",
            "Genre",
            "Thema",
            "Titel",
            "Datum",
            "Zeit",
            "Dauer",
            "Größe",
            "Beschreibung",
            "Url",
            "Website",
            "Neu",
            "Podcast",
            "Doppelt"
        )
        private val DATE_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        private val TIME_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")
        private val DATASET_TIMESTAMP_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm:ss")
    }
}
