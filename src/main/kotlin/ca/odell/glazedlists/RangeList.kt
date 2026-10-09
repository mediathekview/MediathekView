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
package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent

/** A writable, continuously updated range view of a source [EventList]. */
@Suppress("INAPPLICABLE_JVM_NAME")
open class RangeList<E>(source: EventList<E>) : TransformedList<E, E>(source) {
    private var desiredStart = 0
    private var desiredEnd = -1
    private var currentStartIndex = 0
    private var currentEndIndex = source.size

    init {
        source.addListEventListener(this)
    }

    final override fun listChanged(listChanges: ListEvent<E>) {
        updates.beginEvent(true)

        while (listChanges.next()) {
            val changeType = listChanges.type
            val changeIndex = listChanges.index
            val oldValue = listChanges.oldValue
            val newValue = listChanges.newValue

            when (changeType) {
                ListEvent.DELETE -> {
                    if (changeIndex < currentStartIndex) {
                        currentStartIndex--
                        currentEndIndex--
                    } else if (changeIndex < currentEndIndex) {
                        currentEndIndex--
                        updates.elementDeleted(changeIndex - currentStartIndex, oldValue)
                    }
                }

                ListEvent.INSERT -> {
                    if (changeIndex < currentStartIndex) {
                        currentStartIndex++
                        currentEndIndex++
                    } else if (changeIndex < currentEndIndex) {
                        currentEndIndex++
                        updates.elementInserted(changeIndex - currentStartIndex, newValue)
                    }
                }

                ListEvent.UPDATE -> {
                    if (changeIndex in currentStartIndex until currentEndIndex) {
                        updates.elementUpdated(changeIndex - currentStartIndex, oldValue, newValue)
                    }
                }
            }
        }

        adjustRange()
        updates.commitEvent()
    }

    open fun setHeadRange(startIndex: Int, endIndex: Int) {
        desiredStart = startIndex
        desiredEnd = endIndex
        adjustRange()
    }

    open fun setMiddleRange(startIndex: Int, endIndex: Int) {
        desiredStart = startIndex
        desiredEnd = -endIndex - 1
        adjustRange()
    }

    open fun setTailRange(startIndex: Int, endIndex: Int) {
        desiredStart = -startIndex - 1
        desiredEnd = -endIndex - 1
        adjustRange()
    }

    protected fun adjustRange() {
        updates.beginEvent(true)

        var desiredStartIndex = startIndex
        var desiredEndIndex = endIndex
        if (desiredEndIndex < desiredStartIndex) {
            val previousEnd = desiredEndIndex
            desiredEndIndex = desiredStartIndex
            desiredStartIndex = previousEnd
        }

        if (desiredStartIndex < currentStartIndex) {
            updates.elementsInserted(0, currentStartIndex - desiredStartIndex - 1)
        } else if (currentStartIndex < desiredStartIndex && currentStartIndex < currentEndIndex) {
            val deleteThru = minOf(desiredStartIndex, currentEndIndex)
            for (index in currentStartIndex until deleteThru) {
                updates.elementDeleted(0, source!![index])
            }
        }
        currentStartIndex = desiredStartIndex

        if (desiredEndIndex < currentEndIndex) {
            for (index in desiredEndIndex until currentEndIndex) {
                updates.elementDeleted(desiredEndIndex - currentStartIndex, source!![index])
            }
        } else if (currentEndIndex < desiredEndIndex && desiredStartIndex < desiredEndIndex) {
            val insertFrom = maxOf(currentEndIndex, currentStartIndex)
            updates.elementsInserted(insertFrom - currentStartIndex, desiredEndIndex - currentStartIndex - 1)
        }
        currentEndIndex = desiredEndIndex

        updates.commitEvent()
    }

    @get:JvmName("size")
    final override val size: Int
        get() = currentEndIndex - currentStartIndex

    final override fun getSourceIndex(mutationIndex: Int): Int = mutationIndex + currentStartIndex

    final override fun isWritable(): Boolean = true

    open val startIndex: Int
        get() {
            val sourceSize = source!!.size
            val desiredStartIndex = if (desiredStart >= 0) desiredStart else sourceSize + desiredStart + 1
            return desiredStartIndex.coerceIn(0, sourceSize)
        }

    open val endIndex: Int
        get() {
            val sourceSize = source!!.size
            val desiredEndIndex = if (desiredEnd >= 0) desiredEnd else sourceSize + desiredEnd + 1
            return desiredEndIndex.coerceIn(startIndex, sourceSize)
        }
}
