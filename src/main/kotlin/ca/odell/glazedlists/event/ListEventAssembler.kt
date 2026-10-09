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
package ca.odell.glazedlists.event

import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.impl.WeakReferenceProxy
import ca.odell.glazedlists.impl.event.BlockSequence
import ca.odell.glazedlists.impl.event.Tree4Deltas

/**
 * Models a continuous stream of changes on a list. Changes of the same type
 * that occur on a continuous set of rows are grouped into blocks
 * automatically for performance benefits.
 *
 * Atomic sets of changes may involve many lines of changes and many blocks
 * of changes. They are committed to the queue in one action. No other threads
 * should be creating a change on the same list change queue when an atomic
 * change is being created.
 *
 * @author Jesse Wilson
 */
class ListEventAssembler<E>(
    private val sourceList: EventList<E>,
    publisherArg: ListEventPublisher,
) {
    /** non-null if an event is currently pending */
    private var eventThread: Thread? = null

    /** the event level is the number of nested events */
    private var eventLevel: Int = 0

    /** whether to allow nested events */
    private var allowNestedEvents: Boolean = true

    /** the current reordering array if this change is a reorder */
    private var reorderMap: IntArray? = null

    /** prefer to use the linear blocks, which are more performant but handle only a subset of all cases */
    private val blockSequence = BlockSequence<E>()

    private var useListBlocksLinear = false

    /** fall back to list tree4deltas, which are capable of all list changes */
    private val listDeltas = Tree4Deltas<E>()

    private val publisher = publisherArg as SequenceDependenciesEventPublisher

    private val listEvent: ListEvent<E> = createTree4DeltasListEvent(this, sourceList)

    private val eventFormat = ListEventFormat()

    private val state = ListEventAssemblerState()

    /** true if we're waiting on the publisher to distribute our event */
    private var eventIsBeingPublished = false

    /**
     * Indicate whether or not an event is in progress. Intended for testing purposes.
     */
    fun isEventInProgress(): Boolean = eventLevel != 0

    /**
     * Starts a new atomic change to this list change queue.
     *
     * This simple change event does not support change events nested within.
     * To allow other methods to nest change events within a change event, use
     * beginEvent(true).
     */
    fun beginEvent() {
        beginEvent(false)
    }

    /**
     * Starts a new atomic change to this list change queue. This signature
     * allows you to specify allowing nested changes. This simply means that
     * you can call other methods that contain a beginEvent(), commitEvent()
     * block and their changes will be recorded but not fired. This allows
     * the creation of list modification methods to call simpler list modification
     * methods while still firing a single ListEvent to listeners.
     *
     * @param allowNestedEvents false to throw an exception
     * if another call to beginEvent() is made before
     * the next call to commitEvent(). Nested events allow
     * multiple method's events to be composed into a single
     * event.
     */
    @Synchronized
    fun beginEvent(allowNestedEvents: Boolean) {
        if (!this.allowNestedEvents) {
            throw ConcurrentModificationException(
                "Cannot begin a new event while another event is in progress by thread, ${eventThread!!.name}",
            )
        }
        this.allowNestedEvents = allowNestedEvents
        if (allowNestedEvents || (eventLevel == 0 && eventThread != null)) {
            listDeltas.setAllowContradictingEvents(true)
        }

        if (eventThread == null) {
            eventThread = Thread.currentThread()
            useListBlocksLinear = true
        }

        eventLevel++
    }

    /**
     * Add to the current ListEvent the insert of the element at
     * the specified index, with the specified previous value.
     */
    fun elementInserted(index: Int, newValue: E) {
        addChange(ListEvent.INSERT, index, index, ListEvent.unknownValue(), newValue)
    }

    /**
     * Add to the current ListEvent the update of the element at the specified
     * index, with the specified previous value.
     */
    fun elementUpdated(index: Int, oldValue: E, newValue: E) {
        addChange(ListEvent.UPDATE, index, index, oldValue, newValue)
    }

    /**
     * Add to the current ListEvent the removal of the element at the specified
     * index, with the specified previous value.
     */
    fun elementDeleted(index: Int, oldValue: E) {
        addChange(ListEvent.DELETE, index, index, oldValue, ListEvent.unknownValue())
    }

    /**
     * Add a contiguous range of inserts whose values are unavailable.
     */
    fun elementsInserted(startIndex: Int, endIndex: Int) {
        addChange(ListEvent.INSERT, startIndex, endIndex, ListEvent.unknownValue(), ListEvent.unknownValue())
    }

    /**
     * Add a contiguous range of updates whose old and new values are unavailable.
     */
    fun elementsUpdated(startIndex: Int, endIndex: Int) {
        addChange(ListEvent.UPDATE, startIndex, endIndex, ListEvent.unknownValue(), ListEvent.unknownValue())
    }

    /**
     * Add a contiguous range of deletes whose values are unavailable.
     */
    fun elementsDeleted(startIndex: Int, endIndex: Int) {
        addChange(ListEvent.DELETE, startIndex, endIndex, ListEvent.unknownValue(), ListEvent.unknownValue())
    }

    /**
     * Adds a block of changes to the set of list changes. The change block
     * allows a range of changes to be grouped together for efficiency.
     *
     * @param endIndex the inclusive end index
     */
    private fun addChange(type: Int, startIndex: Int, endIndex: Int, oldValue: E, newValue: E) {
        if (useListBlocksLinear) {
            val success = blockSequence.addChange(type, startIndex, endIndex + 1, oldValue, newValue)
            if (success) {
                return
            }

            listDeltas.addAll(blockSequence)
            useListBlocksLinear = false
        }

        when (type) {
            ListEvent.INSERT -> listDeltas.targetInsert(startIndex, endIndex + 1, newValue)
            ListEvent.UPDATE -> listDeltas.targetUpdate(startIndex, endIndex + 1, oldValue, newValue)
            ListEvent.DELETE -> listDeltas.targetDelete(startIndex, endIndex + 1, oldValue)
        }
    }

    /**
     * Sets the current event as a reordering. Reordering events cannot be
     * combined with other events.
     */
    fun reorder(reorderMap: IntArray) {
        check(isEventEmpty()) { "Cannot combine reorder with other change events" }
        if (reorderMap.isEmpty()) {
            return
        }
        addChange(ListEvent.DELETE, 0, reorderMap.size - 1, ListEvent.unknownValue(), ListEvent.unknownValue())
        addChange(ListEvent.INSERT, 0, reorderMap.size - 1, ListEvent.unknownValue(), ListEvent.unknownValue())
        this.reorderMap = reorderMap
    }

    /**
     * Forwards the event. This is a convenience method that does the following:
     * 1. Snapshot all remaining changes in sourceEvent
     * 2. beginEvent()
     * 3. Apply the snapshotted changes to this
     * 4. commitEvent()
     *
     * Note that this method should be preferred to manually forwarding events
     * because it is heavily optimized.
     *
     * Note that currently this implementation does a best effort to preserve
     * reorderings. This means that a reordering is lost if it is combined with
     * any other ListEvent.
     */
    @Suppress("UNCHECKED_CAST")
    fun forwardEvent(listChanges: ListEvent<*>) {
        val preserveReorder = listChanges.isReordering && isEventEmpty()
        val forwardedReorderMap: IntArray?
        val forwardedChanges: List<ForwardedChange<E>>
        if (preserveReorder) {
            forwardedReorderMap = listChanges.reorderMap
            forwardedChanges = emptyList()
        } else {
            forwardedReorderMap = null
            forwardedChanges = ArrayList()
            while (listChanges.next()) {
                forwardedChanges += ForwardedChange(
                    listChanges.type,
                    listChanges.index,
                    listChanges.oldValue as E,
                    listChanges.newValue as E,
                )
            }
            listChanges.reset()
        }

        beginEvent(false)
        reorderMap = null
        if (forwardedReorderMap != null) {
            reorder(forwardedReorderMap)
        } else {
            for ((type, index, oldValue, newValue) in forwardedChanges) {
                addChange(type, index, index, oldValue, newValue)
            }
        }
        commitEvent()
    }

    /**
     * Commits the current atomic change to this list change queue. This will
     * notify all listeners about the change.
     *
     * If the current event is nested within a greater event, this will simply
     * change the nesting level so that further changes are applied directly to the
     * parent change.
     */
    @Synchronized
    fun commitEvent() {
        check(eventLevel != 0) { "Cannot commit without an event in progress" }

        eventLevel--
        allowNestedEvents = true

        if (eventLevel != 0) {
            return
        }

        if (isEventEmpty()) {
            cleanup()
            return
        }

        if (eventIsBeingPublished) {
            return
        }

        eventIsBeingPublished = true
        publisher.fireEvent(sourceList, listEvent, eventFormat)
    }

    /**
     * Discards the current atomic change to this list change queue. This does
     * not notify any listeners about any changes.
     *
     * The caller of this method is responsible for returning the EventList
     * to its state before the event began. If they fail to do so, the EventList
     * pipeline may be in an inconsistent state.
     *
     * If the current event is nested within a greater event, this will
     * discard changes at the current nesting level and that further changes
     * are still applied directly to the parent change.
     */
    @Synchronized
    fun discardEvent() {
        check(eventLevel != 0) { "Cannot discard without an event in progress" }

        eventLevel--
        allowNestedEvents = true

        if (eventLevel == 0) {
            cleanup()
        }
    }

    /**
     * Returns true if the current atomic change to this list change
     * queue is empty; false otherwise.
     */
    fun isEventEmpty(): Boolean = if (useListBlocksLinear) blockSequence.isEmpty else listDeltas.isEmpty

    /**
     * Registers the specified listener to be notified whenever new changes
     * are appended to this list change sequence.
     *
     * For each listener, a ListEvent is created, which provides
     * a read-only view to the list changes in the list. The same
     * ListChangeView object is used for all notifications to the specified
     * listener, so if a listener does not process a set of changes, those
     * changes will persist in the next notification.
     *
     * @param listChangeListener event listener != null
     * @throws NullPointerException if the specified listener is null
     */
    @Synchronized
    fun addListEventListener(listChangeListener: ListEventListener<in E>) {
        publisher.addListener(sourceList, listChangeListener, eventFormat)
    }

    /**
     * Removes the specified listener from receiving notification when new
     * changes are appended to this list change sequence.
     *
     * This uses the == identity comparison to find the listener
     * instead of equals(). This is because multiple Lists may be
     * listening and therefore equals() may be ambiguous.
     *
     * @param listChangeListener event listener != null
     * @throws NullPointerException if the specified listener is null
     * @throws IllegalArgumentException if the specified listener wasn't added before
     */
    @Synchronized
    fun removeListEventListener(listChangeListener: ListEventListener<in E>) {
        publisher.removeListener(sourceList, listChangeListener)
    }

    /**
     * Get all ListEventListeners observing the EventList.
     */
    fun getListEventListeners(): List<ListEventListener<E>> = publisher.getListeners(sourceList)

    internal fun eventState(): ListEventAssemblerState = state

    /**
     * Cleanup all temporary variables necessary while events are being fired.
     */
    private fun cleanup() {
        eventThread = null
        blockSequence.reset()
        listDeltas.reset(sourceList.size)
        reorderMap = null
        listDeltas.setAllowContradictingEvents(false)
        listEvent.reset()
    }

    /**
     * Adapt SequenceDependenciesEventPublisher.EventFormat for use with ListEvents.
     */
    private inner class ListEventFormat :
        SequenceDependenciesEventPublisher.EventFormat<EventList<E>, ListEventListener<in E>, ListEvent<E>> {
        override fun fire(subject: EventList<E>, event: ListEvent<E>, listener: ListEventListener<in E>) {
            event.reset()
            @Suppress("UNCHECKED_CAST")
            (listener as ListEventListener<E>).listChanged(event)
        }

        override fun postEvent(subject: EventList<E>) {
            cleanup()
            eventIsBeingPublished = false
        }

        override fun isStale(subject: EventList<E>, listener: ListEventListener<in E>): Boolean {
            if (listener is WeakReferenceProxy<*> && listener.referent == null) {
                listener.dispose()
                return true
            }
            return false
        }
    }

    companion object {
        /**
         * Create a new ListEventPublisher for an EventList not attached
         * to any other EventLists.
         */
        fun createListEventPublisher(): ListEventPublisher = SequenceDependenciesEventPublisher()
    }

    internal inner class ListEventAssemblerState {
        val useListBlocksLinear: Boolean
            get() = this@ListEventAssembler.useListBlocksLinear

        val listBlocksLinear: BlockSequence<E>
            get() = blockSequence

        val listDeltas: Tree4Deltas<E>
            get() = this@ListEventAssembler.listDeltas

        val reorderMap: IntArray?
            get() = this@ListEventAssembler.reorderMap
    }

    private data class ForwardedChange<E>(
        val type: Int,
        val index: Int,
        val oldValue: E,
        val newValue: E,
    )
}
