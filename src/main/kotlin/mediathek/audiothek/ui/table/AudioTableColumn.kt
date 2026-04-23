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

package mediathek.audiothek.ui.table

import mediathek.audiothek.model.AudioEntry
import java.time.format.DateTimeFormatter

enum class AudioTableColumn(
    val title: String,
    val preferredWidth: Int? = null,
    val centered: Boolean = false,
    val searchField: String? = null,
    val editable: Boolean = false,
    val valueProvider: (AudioEntry) -> Any
) {
    SENDER(
        title = "Sender",
        preferredWidth = 90,
        centered = true,
        searchField = AudiothekLuceneIndex.FIELD_SENDER,
        valueProvider = { it.channel }
    ),
    GENRE(
        title = "Genre",
        preferredWidth = 150,
        searchField = AudiothekLuceneIndex.FIELD_GENRE,
        valueProvider = { it.genre }
    ),
    THEME(
        title = "Thema",
        preferredWidth = 210,
        searchField = AudiothekLuceneIndex.FIELD_THEME,
        valueProvider = { it.theme }
    ),
    TITLE(
        title = "Titel",
        preferredWidth = 430,
        searchField = AudiothekLuceneIndex.FIELD_TITLE,
        valueProvider = { it.title }
    ),
    PLAY(
        title = "",
        preferredWidth = 32,
        editable = true,
        valueProvider = { "A" }
    ),
    DOWNLOAD(
        title = "",
        preferredWidth = 32,
        editable = true,
        valueProvider = { "D" }
    ),
    DATE(
        title = "Datum",
        preferredWidth = 110,
        centered = true,
        searchField = AudiothekLuceneIndex.FIELD_DATE,
        valueProvider = { it.publishedAt?.format(DATE_FORMAT).orEmpty() }
    ),
    TIME(
        title = "Zeit",
        preferredWidth = 80,
        centered = true,
        searchField = AudiothekLuceneIndex.FIELD_TIME,
        valueProvider = { it.publishedAt?.format(TIME_FORMAT).orEmpty() }
    ),
    DURATION(
        title = "Dauer",
        preferredWidth = 80,
        centered = true,
        searchField = AudiothekLuceneIndex.FIELD_DURATION,
        valueProvider = { it.durationMinutes?.toString().orEmpty() }
    ),
    SIZE(
        title = "Größe",
        preferredWidth = 80,
        centered = true,
        searchField = AudiothekLuceneIndex.FIELD_SIZE,
        valueProvider = { it.sizeMb?.toString().orEmpty() }
    );

    val modelIndex: Int
        get() = ordinal

    val toggleable: Boolean
        get() = searchField != null

    companion object {
        val searchableColumns: List<AudioTableColumn>
            get() = entries.filter(AudioTableColumn::toggleable)

        private val DATE_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        private val TIME_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")
    }
}
