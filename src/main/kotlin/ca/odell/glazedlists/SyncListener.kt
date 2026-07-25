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

import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener

/**
 * Synchronizes the specified [EventList] to the specified [MutableList].
 *
 * @author Jesse Wilson
 */
open class SyncListener<E>(
    private var source: EventList<E>?,
    private var target: MutableList<E>?,
) : ListEventListener<E> {
    private var targetSize: Int

    init {
        val targetList = target!!
        targetList.clear()
        val sourceList = source!!
        targetList.addAll(sourceList)
        targetSize = targetList.size
        sourceList.addListEventListener(this)
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        val sourceList = listChanges.sourceList
        val targetList = target!!
        check(targetList.size == targetSize) { "Synchronize EventList target has been modified" }

        while (listChanges.next()) {
            val changeIndex = listChanges.index
            when (listChanges.type) {
                ListEvent.INSERT -> {
                    targetList.add(changeIndex, sourceList[changeIndex])
                    targetSize++
                }

                ListEvent.UPDATE -> targetList[changeIndex] = sourceList[changeIndex]

                ListEvent.DELETE -> {
                    targetList.removeAt(changeIndex)
                    targetSize--
                }
            }
        }
    }

    /**
     * Stops synchronization and clears references to the source and target lists.
     */
    open fun dispose() {
        val sourceList = source ?: return
        sourceList.removeListEventListener(this)
        source = null
        target = null
    }
}
