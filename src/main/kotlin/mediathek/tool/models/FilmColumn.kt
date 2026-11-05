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

package mediathek.tool.models

import mediathek.daten.DatenFilm
import mediathek.i18n.UiMessages
import mediathek.tool.datum.DatumFilm

internal enum class FilmColumn(
    val index: Int,
    private val titleKey: String?,
    val valueType: Class<*>,
    private val valueProvider: ((DatenFilm) -> Any)? = null,
) {
    NUMBER(0, "film.column.number", Int::class.javaObjectType, DatenFilm::filmNr),
    SENDER(1, "film.column.channel", String::class.java, DatenFilm::sender),
    TOPIC(2, "film.column.topic", String::class.java, DatenFilm::thema),
    TITLE(3, "film.column.title", String::class.java, DatenFilm::title),
    PLAY(4, "", String::class.java, { "" }),
    SAVE(5, "", String::class.java, { "" }),
    BOOKMARK(6, "", String::class.java, { "" }),
    DATE(7, "film.column.date", DatumFilm::class.java, DatenFilm::datumFilm),
    TIME(8, "film.column.time", String::class.java, DatenFilm::sendeZeit),
    DURATION(9, "film.column.duration", Int::class.javaObjectType, DatenFilm::filmLength),
    SIZE(10, "film.column.size", Int::class.javaObjectType, DatenFilm::fileSizeInMegabytes),
    HIGH_QUALITY(11, "film.column.hq", Boolean::class.javaObjectType, DatenFilm::isHighQuality),
    SUBTITLE(12, "film.column.subtitles", Boolean::class.javaObjectType, { it.hasSubtitle() }),
    GEO(13, "film.column.geo", String::class.java, DatenFilm::countriesAsString),
    URL(14, "film.column.url", String::class.java, DatenFilm::urlNormalQuality),
    ;

    /** Returns the localized heading without changing the stable column index or value type. */
    fun title(): String =
        titleKey?.let { if (it.isEmpty()) "" else UiMessages.text(it) }
            ?: throw IndexOutOfBoundsException("UNKNOWN COLUMN NAME: $index")

    fun valueFrom(film: DatenFilm): Any =
        valueProvider?.invoke(film) ?: throw IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: $index")

    companion object {
        private val byIndex = entries.associateBy(FilmColumn::index)

        fun fromIndex(index: Int): FilmColumn =
            byIndex[index] ?: throw IndexOutOfBoundsException("UNKNOWN FILM COLUMN: $index")
    }
}
