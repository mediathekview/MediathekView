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

import mediathek.audiothek.model.AudioEntry
import java.util.Locale

internal object AudiothekOnlineSearchQuery {
    class Request private constructor(
        val query: String,
        private val fieldFilters: List<FieldFilter> = emptyList(),
    ) {
        fun filter(entries: List<AudioEntry>): List<AudioEntry> {
            if (fieldFilters.isEmpty()) {
                return entries
            }
            return entries.filter { entry ->
                fieldFilters.all { it.matches(entry) }
            }
        }

        companion object {
            fun of(query: String, fieldFilters: List<FieldFilter> = emptyList()): Request =
                Request(query, fieldFilters)
        }
    }

    data class FieldFilter(
        val field: OnlineField,
        val value: String,
    ) {
        fun matches(entry: AudioEntry): Boolean {
            val fieldValue = when (field) {
                OnlineField.GENRE -> entry.genre
                OnlineField.THEME -> entry.theme
                OnlineField.TITLE -> entry.title
            }
            return fieldValue.lowercase(Locale.ROOT).contains(value.lowercase(Locale.ROOT))
        }
    }

    enum class OnlineField(
        val includeInRemoteQuery: Boolean,
    ) {
        GENRE(false),
        THEME(true),
        TITLE(true)
    }

    fun from(searchText: String): Request? {
        val normalized = searchText.trim()
        if (normalized.isEmpty()) {
            return null
        }

        val tokens = tokenize(normalized)
        var hasKnownFieldToken = false
        val onlineParts = mutableListOf<String>()
        val fieldFilters = mutableListOf<FieldFilter>()

        tokens.forEach { token ->
            val fieldToken = token.toFieldToken()
            if (fieldToken == null) {
                onlineParts += token.unquote()
                return@forEach
            }

            val (field, value) = fieldToken
            val onlineField = ONLINE_FIELD_KEYS[field]
            when {
                onlineField != null -> {
                    val onlineValue = value.toOnlineValue() ?: return@forEach
                    hasKnownFieldToken = true
                    if (onlineField.includeInRemoteQuery) {
                        onlineParts += onlineValue
                    }
                    fieldFilters += FieldFilter(onlineField, onlineValue)
                }
                field in LOCAL_ONLY_FIELD_KEYS -> return null
                else -> onlineParts += token
            }
        }

        if (!hasKnownFieldToken) {
            return Request.of(normalized)
        }

        return onlineParts.joinToString(" ")
            .trim()
            .takeIf(String::isNotEmpty)
            ?.let { Request.of(it, fieldFilters) }
    }

    private fun tokenize(query: String): List<String> {
        return TOKEN_REGEX.findAll(query)
            .map { it.value.trim() }
            .filter { it.isNotEmpty() }
            .toList()
    }

    private fun String.toFieldToken(): Pair<String, String>? {
        val separatorIndex = indexOf(':')
        if (separatorIndex <= 0 || separatorIndex == lastIndex) {
            return null
        }
        return substring(0, separatorIndex).lowercase() to substring(separatorIndex + 1)
    }

    private fun String.toOnlineValue(): String? {
        val trimmed = trim()
        val value = if (trimmed.isQuoted()) {
            trimmed.unquote()
        } else {
            trimmed
                .replace("*", "")
                .replace("?", "")
        }

        return value
            .trim()
            .takeIf(String::isNotEmpty)
    }

    private fun String.unquote(): String {
        val trimmed = trim()
        if (!trimmed.isQuoted()) {
            return trimmed
        }
        return trimmed.substring(1, trimmed.length - 1).trim()
    }

    private fun String.isQuoted(): Boolean =
        length >= 2 && startsWith('"') && endsWith('"')

    private val ONLINE_FIELD_KEYS = mapOf(
        "genre" to OnlineField.GENRE,
        "thema" to OnlineField.THEME,
        "theme" to OnlineField.THEME,
        "titel" to OnlineField.TITLE,
        "title" to OnlineField.TITLE
    )
    private val LOCAL_ONLY_FIELD_KEYS = setOf(
        "sender",
        "datum",
        "date",
        "zeit",
        "time",
        "dauer",
        "duration",
        "groesse",
        "größe",
        "size"
    )
    private val TOKEN_REGEX = """[^\s:]+:"[^"]*"|"[^"]*"|\S+""".toRegex()
}
