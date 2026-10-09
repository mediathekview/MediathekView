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

package mediathek.gui.bookmark

import mediathek.daten.DatenFilm
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeSupport
import java.time.LocalDate
import java.util.*

/**
 * Bookmark data definition used to store movies
 */
class BookmarkData() {
    private val support = PropertyChangeSupport(this)

    constructor(film: DatenFilm) : this() {
        url = film.urlNormalQuality
        datenFilm = film
        originalSender = film.sender
        originalTitle = film.title
        originalThema = film.thema
    }

    var seen: Boolean = false
        set(value) {
            val oldSeen = field
            field = value
            support.firePropertyChange("seen", oldSeen, value)
        }

    var url: String? = null
        set(value) {
            val oldUrl = field
            field = value
            support.firePropertyChange("url", oldUrl, value)
        }

    var datenFilm: DatenFilm? = null
        set(value) {
            val oldFilm = field
            field = value
            support.firePropertyChange("datenFilm", oldFilm, value)
        }

    var note: String? = null
        set(value) {
            val oldNote = field
            field = value
            support.firePropertyChange("note", oldNote, value)
        }

    var availableUntil: LocalDate? = null
        set(value) {
            val oldDate = field
            field = value
            support.firePropertyChange("availableUntil", oldDate, value)
        }

    // will be added from [BookmarkDataList#checkAndBookmarkMovies].
    var bookmarkAdded: LocalDate? = null
        set(value) {
            val oldDate = field
            field = value
            support.firePropertyChange("bookmarkAdded", oldDate, value)
        }

    // The SHA256 hashcode from the film object used to create the bookmark.
    // This is the correct way to store film object info as it will be unique.
    var filmHashCode: String? = null
        set(value) {
            val oldHash = field
            field = value
            url = null // remove URL if we use the hashcode.
            support.firePropertyChange("hashCode", oldHash, value)
        }

    var originalSender: String? = null
    var originalTitle: String? = null
    var originalThema: String? = null

    val sender: String
        get() = datenFilm?.sender ?: originalSender ?: "NO SENDER"

    val thema: String
        get() = datenFilm?.thema ?: originalThema ?: "NO THEMA"

    val title: String
        get() = datenFilm?.title ?: originalTitle ?: "NO TITLE"

    val dauer: Int
        get() = datenFilm?.filmLength ?: -1

    val sendedatum: Date?
        get() = datenFilm?.datumFilm

    val normalQualityUrl: String?
        get() = datenFilm?.urlNormalQuality

    val noteOptional: Optional<String>
        get() = Optional.ofNullable(note)

    val notSeen: Boolean
        get() = !seen

    fun isNotInFilmList(): Boolean = datenFilm == null

    val webUrl: String?
        get() = datenFilm?.websiteUrl

    val datenFilmOptional: Optional<DatenFilm>
        get() = Optional.ofNullable(datenFilm)

    fun addPropertyChangeListener(listener: PropertyChangeListener) {
        support.addPropertyChangeListener(listener)
    }

    fun removePropertyChangeListener(listener: PropertyChangeListener) {
        support.removePropertyChangeListener(listener)
    }
}
