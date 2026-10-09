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

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener

/** Rejects inserted or updated elements whose runtime types are not configured. */
internal open class TypeSafetyListener<E>(
    source: EventList<E>,
    types: Set<Class<*>?>,
) : ListEventListener<E> {
    private val types: Array<Class<*>?> = types.toTypedArray()

    init {
        source.addListEventListener(this)
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        val source = listChanges.sourceList
        while (listChanges.next()) {
            val type = listChanges.type
            if (type == ListEvent.DELETE) continue

            val index = listChanges.index
            val element = source[index]
            if ((type == ListEvent.INSERT || type == ListEvent.UPDATE) && !checkType(element)) {
                val badType = element?.javaClass
                val operation = if (type == ListEvent.INSERT) "inserted" else "updated"
                throw IllegalArgumentException(
                    "Element with illegal type $badType $operation at index $index: $element",
                )
            }
        }
    }

    private fun checkType(element: E): Boolean =
        types.any { type -> if (element == null) type == null else type?.isInstance(element) == true }
}
