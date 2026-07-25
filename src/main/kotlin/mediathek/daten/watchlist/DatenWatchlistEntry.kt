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
 * A show (sender + thema, optionally restricted to a title filter) the user wants to be
 * notified about when new episodes appear in the film list.
 */
class DatenWatchlistEntry {
    /**
     * Stable identity used to link notifications to this entry. Generated on creation;
     * persisted so the link survives restarts.
     */
    var id: String = UUID.randomUUID().toString()
    var name: String = ""
    var sender: String = ""
    var thema: String = ""
    var title: String = ""

    /**
     * Compressed URL keys ([DatenFilm.storedNormalQualityUrl]) of episodes that must not
     * trigger a notification anymore. Prefilled with all matching episodes when the entry
     * is created so only genuinely new episodes are reported.
     */
    val seenUrlKeys: MutableSet<String> = LinkedHashSet()

    fun copy(): DatenWatchlistEntry {
        val duplicate = DatenWatchlistEntry()
        duplicate.id = id
        duplicate.name = name
        duplicate.sender = sender
        duplicate.thema = thema
        duplicate.title = title
        duplicate.seenUrlKeys.addAll(seenUrlKeys)
        return duplicate
    }

    fun matches(film: DatenFilm): Boolean =
        film.sender.equals(sender, ignoreCase = true) &&
            film.thema.equals(thema, ignoreCase = true) &&
            (title.isEmpty() || film.title.contains(title, ignoreCase = true))
}
