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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.impl

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransformedList
import ca.odell.glazedlists.event.ListEvent
/** Maps each source-list element through a fixed function. */
internal class SimpleFunctionList<S, E>(
    source: EventList<S>,
    private val function: (S) -> E,
) : TransformedList<S, E>(source) {
    init {
        source.addListEventListener(this)
    }

    override fun get(index: Int): E = function(source!![index])

    override fun listChanged(listChanges: ListEvent<S>) {
        updates.forwardEvent(listChanges)
    }

    override fun isWritable(): Boolean = false
}
