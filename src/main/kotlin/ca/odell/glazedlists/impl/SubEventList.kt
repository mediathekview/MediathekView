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

/** A writable event-list view over a range of its source list. */
internal class SubEventList<E>(
    source: EventList<E>,
    private var startIndex: Int,
    private var endIndex: Int,
    automaticallyRemove: Boolean,
) : TransformedList<E, E>(source) {
    init {
        if (startIndex !in 0..endIndex || endIndex > source.size) {
            throw IllegalArgumentException(
                "The range $startIndex-$endIndex is not valid over a list of size ${source.size}",
            )
        }

        if (automaticallyRemove) {
            source.addListEventListener(WeakReferenceProxy(source, this))
        } else {
            source.addListEventListener(this)
        }
    }

    override val size: Int
        get() = endIndex - startIndex

    override fun getSourceIndex(mutationIndex: Int): Int = mutationIndex + startIndex

    override fun isWritable(): Boolean = true

    override fun listChanged(listChanges: ListEvent<E>) {
        updates.beginEvent()

        if (listChanges.isReordering && size == 1) {
            val reorderMap = listChanges.reorderMap
            for (reorderedIndex in reorderMap.indices) {
                if (reorderMap[reorderedIndex] == startIndex) {
                    startIndex = reorderedIndex
                    endIndex = startIndex + 1
                    break
                }
            }
        } else {
            while (listChanges.next()) {
                val changeIndex = listChanges.index
                when (listChanges.type) {
                    ListEvent.INSERT -> {
                        if (changeIndex <= startIndex) {
                            startIndex++
                            endIndex++
                        } else if (changeIndex < endIndex) {
                            endIndex++
                            updates.elementInserted(changeIndex - startIndex, listChanges.newValue)
                        }
                    }

                    ListEvent.UPDATE -> {
                        if (changeIndex in startIndex..<endIndex) {
                            updates.elementUpdated(
                                changeIndex - startIndex,
                                listChanges.oldValue,
                                listChanges.newValue,
                            )
                        }
                    }

                    ListEvent.DELETE -> {
                        if (changeIndex < startIndex) {
                            startIndex--
                            endIndex--
                        } else if (changeIndex < endIndex) {
                            endIndex--
                            updates.elementDeleted(changeIndex - startIndex, listChanges.oldValue)
                        }
                    }
                }
            }
        }

        check(startIndex <= endIndex)
        updates.commitEvent()
    }
}
