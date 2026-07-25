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

/**
 * A pending "new episode available" notification of a watchlist entry.
 *
 * Notifications stay pending until the user removes them; the red badge state is tracked
 * separately by the service.
 */
data class WatchlistNotification(
    val entryId: String,
    val entryName: String,
    /** Stable film identity, used to deduplicate notifications across matching runs. */
    val filmId: String,
    val sender: String,
    val thema: String,
    val title: String,
    val sendeDatum: String,
    val urlNormalQuality: String,
)
