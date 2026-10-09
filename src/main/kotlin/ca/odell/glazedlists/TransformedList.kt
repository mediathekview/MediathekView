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
import ca.odell.glazedlists.event.ListEventListener

/** A base class for event lists that transform another [EventList]. */
@Suppress("INAPPLICABLE_JVM_NAME", "UNCHECKED_CAST")
abstract class TransformedList<S, E> protected constructor(source: EventList<S>) :
    AbstractEventList<E>(source.publisher),
    ListEventListener<S> {
    /** The event list being transformed. It becomes `null` when a subclass uses null to mark disposal. */
    @JvmField
    protected var source: EventList<S>? = source

    init {
        readWriteLock = source.readWriteLock
    }

    protected open fun getSourceIndex(mutationIndex: Int): Int = mutationIndex

    protected abstract fun isWritable(): Boolean

    abstract override fun listChanged(listChanges: ListEvent<S>)

    override fun add(index: Int, element: E) {
        check(isWritable()) { "Non-writable List cannot be modified" }
        if (index !in 0..size) {
            throw IndexOutOfBoundsException("Cannot add at $index on list of size $size")
        }

        val currentSource = source!!
        val sourceIndex = if (index < size) getSourceIndex(index) else currentSource.size
        currentSource.add(sourceIndex, element as S)
    }

    override fun get(index: Int): E {
        if (index !in indices) {
            throw IndexOutOfBoundsException("Cannot get at $index on list of size $size")
        }
        return source!![getSourceIndex(index)] as E
    }

    @JvmName("remove")
    override fun removeAt(index: Int): E {
        check(isWritable()) { "Non-writable List cannot be modified" }
        if (index !in indices) {
            throw IndexOutOfBoundsException("Cannot remove at $index on list of size $size")
        }
        return source!!.removeAt(getSourceIndex(index)) as E
    }

    override fun set(index: Int, element: E): E {
        check(isWritable()) { "List ${javaClass.name} cannot be modified in the current state" }
        if (index !in indices) {
            throw IndexOutOfBoundsException("Cannot set at $index on list of size $size")
        }
        return source!!.set(getSourceIndex(index), element as S) as E
    }

    @get:JvmName("size")
    override val size: Int
        get() = source!!.size

    override fun dispose() {
        source!!.removeListEventListener(this)
    }
}
