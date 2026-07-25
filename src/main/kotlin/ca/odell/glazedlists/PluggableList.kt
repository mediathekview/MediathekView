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
import ca.odell.glazedlists.event.ListEventPublisher
import java.util.concurrent.locks.ReadWriteLock

/**
 * An event list that delegates to a source [EventList] which can be replaced at runtime.
 */
@Suppress("INAPPLICABLE_JVM_NAME")
open class PluggableList<E>(source: EventList<E>) : TransformedList<E, E>(source) {
    constructor(publisher: ListEventPublisher?, lock: ReadWriteLock?) : this(BasicEventList(publisher, lock))

    init {
        source.addListEventListener(this)
    }

    open fun createSourceList(): EventList<E> = BasicEventList(publisher, readWriteLock)

    @get:JvmName("size")
    override val size: Int
        get() = source!!.size

    @JvmName("remove")
    override fun removeAt(index: Int): E = super.removeAt(index)

    open fun setSource(source: EventList<E>) {
        readWriteLock.writeLock().lock()
        try {
            val currentSource = checkNotNull(this.source) {
                "setSource may not be called on a disposed PluggableList"
            }
            require(readWriteLock == source.readWriteLock) {
                "source list must share lock with PluggableList"
            }
            require(publisher == source.publisher) {
                "source list must share publisher with PluggableList"
            }
            if (currentSource === source) return

            updates.beginEvent()
            for (element in this) updates.elementDeleted(0, element)

            currentSource.removeListEventListener(this)
            this.source = source
            source.addListEventListener(this)

            repeat(size) { index -> updates.elementInserted(index, this[index]) }
            updates.commitEvent()
        } finally {
            readWriteLock.writeLock().unlock()
        }
    }

    override fun isWritable(): Boolean = true

    override fun listChanged(listChanges: ListEvent<E>) {
        updates.forwardEvent(listChanges)
    }

    override fun dispose() {
        source?.removeListEventListener(this)
        source = null
    }
}
