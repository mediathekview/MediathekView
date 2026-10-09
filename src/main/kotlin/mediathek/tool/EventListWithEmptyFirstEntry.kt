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

package mediathek.tool

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransformedList
import ca.odell.glazedlists.event.ListEvent

/**
 * Read-only event list which also contains an empty entry for a "select all" selection.
 */
class EventListWithEmptyFirstEntry(sourceList: EventList<String>) : TransformedList<String, String>(sourceList) {
    init {
        source.addListEventListener(this)
    }

    override fun isWritable(): Boolean = false

    override fun listChanged(listChanges: ListEvent<String>) {
        updates.forwardEvent(listChanges)
    }

    override fun get(index: Int): String =
        if (index == 0) "" else source[index - 1]

    override val size: Int
        get() = source.size + 1
}
