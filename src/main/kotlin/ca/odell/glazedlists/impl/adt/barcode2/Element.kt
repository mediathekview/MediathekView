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
package ca.odell.glazedlists.impl.adt.barcode2

/** The exposed interface of a tree node. */
internal interface Element<V> {
    fun get(): V

    fun set(value: V)

    val color: Byte

    var sorted: Int

    fun next(): Element<V>?

    fun previous(): Element<V>?

    companion object {
        /** A node that is greater than its predecessor and less than its successor. */
        const val SORTED = 0

        /** A node whose value is unrelated to those of its predecessor or successor. */
        const val UNSORTED = 1

        /** A node that is in-work; no inserts or deletes should be performed in this state. */
        const val PENDING = 2
    }
}
