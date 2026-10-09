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
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener

/** A list iterator that remains consistent while its source list changes. */
@Suppress("PLATFORM_CLASS_MAPPED_TO_KOTLIN")
internal open class EventListIterator<E> : java.util.ListIterator<E>, ListEventListener<E> {
    private val source: EventList<E>
    private var nextIndex: Int
    private var lastIndex = -1

    constructor(source: EventList<E>) : this(source, 0, true)

    constructor(source: EventList<E>, nextIndex: Int) : this(source, nextIndex, true)

    constructor(source: EventList<E>, nextIndex: Int, automaticallyRemove: Boolean) {
        this.source = source
        this.nextIndex = nextIndex

        if (automaticallyRemove) {
            val gcProxy: ListEventListener<E> = WeakReferenceProxy(source, this)
            source.addListEventListener(gcProxy)
            source.publisher.clearRelatedSubject(gcProxy)
        } else {
            source.addListEventListener(this)
            source.publisher.clearRelatedSubject(this)
        }
    }

    override fun hasNext(): Boolean = nextIndex < source.size

    override fun next(): E {
        val list = source
        if (nextIndex == list.size) {
            throw NoSuchElementException("Cannot retrieve element $nextIndex on a list of size ${list.size}")
        }
        lastIndex = nextIndex
        nextIndex++
        return list[lastIndex]
    }

    override fun nextIndex(): Int = nextIndex

    override fun hasPrevious(): Boolean = nextIndex > 0

    @Suppress("KotlinConstantConditions")
    override fun previous(): E {
        val list = source
        if (nextIndex == 0) {
            throw NoSuchElementException("Cannot retrieve element $nextIndex on a list of size ${list.size}")
        }
        nextIndex--
        lastIndex = nextIndex
        return list[nextIndex]
    }

    override fun previousIndex(): Int = nextIndex - 1

    override fun add(element: E) {
        source.add(nextIndex, element)
    }

    override fun remove() {
        check(lastIndex != -1) { "Cannot remove() without a prior call to next() or previous()" }
        source.removeAt(lastIndex)
    }

    override fun set(element: E) {
        check(lastIndex != -1) { "Cannot set() without a prior call to next() or previous()" }
        source[lastIndex] = element
    }

    override fun listChanged(listChanges: ListEvent<E>) {
        while (listChanges.next()) {
            val changeIndex = listChanges.index
            when (listChanges.type) {
                ListEvent.INSERT -> {
                    if (changeIndex <= nextIndex) nextIndex++
                    if (lastIndex != -1 && changeIndex <= lastIndex) lastIndex++
                }

                ListEvent.DELETE -> {
                    if (changeIndex < nextIndex) nextIndex--
                    if (lastIndex != -1 && changeIndex < lastIndex) {
                        lastIndex--
                    } else if (lastIndex != -1 && changeIndex == lastIndex) {
                        lastIndex = -1
                    }
                }
            }
        }
    }
}
