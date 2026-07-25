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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists

/**
 * A mutable map backed by an [EventList] that may outlive this map.
 *
 * Call [dispose] when this map is no longer useful but its source list remains
 * referenced, allowing the map to be garbage collected independently.
 */
interface DisposableMap<K, V> : MutableMap<K, V> {
    /**
     * Releases resources held by this map.
     *
     * Calling any map method after disposal is an error.
     */
    fun dispose()
}
