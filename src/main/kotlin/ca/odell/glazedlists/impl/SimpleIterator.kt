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
package ca.odell.glazedlists.impl

/** A forward iterator over a mutable list. */
@Suppress("PLATFORM_CLASS_MAPPED_TO_KOTLIN")
internal open class SimpleIterator<E>(
    private val source: MutableList<E>?,
) : java.util.Iterator<E> {
    private var nextIndex = 0

    override fun hasNext(): Boolean = nextIndex < source!!.size

    override fun next(): E {
        val list = source!!
        if (nextIndex == list.size) {
            throw NoSuchElementException("Cannot retrieve element $nextIndex on a list of size ${list.size}")
        }
        return list[nextIndex++]
    }

    override fun remove() {
        check(nextIndex != 0) { "Cannot remove() without a prior call to next() or previous()" }
        source!!.removeAt(--nextIndex)
    }
}
