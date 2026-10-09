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
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import java.awt.EventQueue
import java.util.concurrent.CopyOnWriteArrayList
import javax.swing.ListModel
import javax.swing.event.ListDataEvent
import javax.swing.event.ListDataListener

/** Adapts an [EventList] for use as a Swing [ListModel]. */
open class DefaultEventListModel<E>(
    source: EventList<E>,
    private val disposeSource: Boolean,
) : ListEventListener<E>, ListModel<E> {
    /** The source event list. Set to `null` after disposal to reject later access. */
    @JvmField
    protected var source: EventList<E>? = source

    private val listeners: MutableList<ListDataListener> = CopyOnWriteArrayList()

    /** Reusable event retained to avoid allocating for every source change. */
    @JvmField
    protected val listDataEvent = MutableListDataEvent(this)

    init {
        source.addListEventListener(this)
    }

    constructor(source: EventList<E>) : this(source, false)

    override fun listChanged(listChanges: ListEvent<E>) {
        check(EventQueue.isDispatchThread()) {
            "Events to ${javaClass.simpleName} must arrive on the EDT - " +
                "consider adding source.swingThreadProxyList() somewhere in your list pipeline"
        }

        listChanges.nextBlock()
        listDataEvent.setRange(listChanges.blockStartIndex, listChanges.blockEndIndex)
        when (listChanges.type) {
            ListEvent.INSERT -> listDataEvent.setType(ListDataEvent.INTERVAL_ADDED)
            ListEvent.DELETE -> listDataEvent.setType(ListDataEvent.INTERVAL_REMOVED)
            ListEvent.UPDATE -> listDataEvent.setType(ListDataEvent.CONTENTS_CHANGED)
        }

        if (listChanges.nextBlock()) {
            listDataEvent.setRange(0, Int.MAX_VALUE)
            listDataEvent.setType(ListDataEvent.CONTENTS_CHANGED)
        }

        fireListDataEvent(listDataEvent)
    }

    override fun getElementAt(index: Int): E {
        val currentSource = checkNotNull(source)
        currentSource.readWriteLock.readLock().lock()
        return try {
            currentSource[index]
        } finally {
            currentSource.readWriteLock.readLock().unlock()
        }
    }

    override fun getSize(): Int {
        val currentSource = checkNotNull(source)
        currentSource.readWriteLock.readLock().lock()
        return try {
            currentSource.size
        } finally {
            currentSource.readWriteLock.readLock().unlock()
        }
    }

    override fun addListDataListener(listDataListener: ListDataListener) {
        listeners.add(listDataListener)
    }

    override fun removeListDataListener(listDataListener: ListDataListener) {
        listeners.remove(listDataListener)
    }

    /** Notifies the current listener snapshot about one block of list changes. */
    protected open fun fireListDataEvent(listDataEvent: ListDataEvent) {
        listeners.forEach { listener ->
            when (listDataEvent.type) {
                ListDataEvent.CONTENTS_CHANGED -> listener.contentsChanged(listDataEvent)
                ListDataEvent.INTERVAL_ADDED -> listener.intervalAdded(listDataEvent)
                ListDataEvent.INTERVAL_REMOVED -> listener.intervalRemoved(listDataEvent)
            }
        }
    }

    /** Unregisters this model and optionally disposes its source list. */
    open fun dispose() {
        val currentSource = checkNotNull(source)
        currentSource.removeListEventListener(this)
        if (disposeSource) currentSource.dispose()
        source = null
    }
}
