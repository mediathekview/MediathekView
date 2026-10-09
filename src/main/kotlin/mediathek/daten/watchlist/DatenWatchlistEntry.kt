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

package mediathek.daten.watchlist

import mediathek.daten.DatenFilm
import java.util.*

/**
 * A show (sender + thema, optionally narrowed by a title filter) the user wants to be
 * notified about when new episodes appear in the film list.
 *
 * Instances are immutable so snapshots handed to the UI can never mutate service state.
 */
data class DatenWatchlistEntry(
    /** Stable identity linking notifications to this entry; persisted across restarts. */
    val id: String = UUID.randomUUID().toString(),
    val name: String = "",
    val sender: String = "",
    val thema: String = "",
    val title: String = "",
    /**
     * Identities ([DatenFilm.sha256]) of episodes that must not trigger a notification
     * anymore. Prefilled with all matching episodes when the entry is created so only
     * genuinely new episodes are reported. Unlike the compressed URL storage of
     * [DatenFilm], this identity is stable across application runs.
     */
    val seenFilmIds: Set<String> = emptySet(),
) {
    fun matches(film: DatenFilm): Boolean =
        film.sender.equals(sender, ignoreCase = true) &&
            film.thema.equals(thema, ignoreCase = true) &&
            (title.isEmpty() || film.title.contains(title, ignoreCase = true))

    fun hasSameCriteriaAs(other: DatenWatchlistEntry): Boolean =
        sender.equals(other.sender, ignoreCase = true) &&
            thema.equals(other.thema, ignoreCase = true) &&
            title.equals(other.title, ignoreCase = true)
}
