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

import java.util.*

/**
 * Manage listeners, firing events, and making sure that events arrive in order.
 *
 * This manages listeners across multiple objects in a pipeline of observables
 * and their listeners. It implements Martin Fowler's EventAggregator design.
 *
 * To guarantee a safe notification order, this class makes sure that all an
 * object's dependencies have been notified of a particular event before that
 * object is itself notified. This is tricky because it requires us to interrupt
 * the event flow and control its flow. In this class, event flow is controlled
 * by queueing events and not necessarily firing them during the [fireEvent]
 * method.
 */
internal class SequenceDependenciesEventPublisher : ListEventPublisher {
    /** keep track of how many times the fireEvent() method is on the stack */
    private var reentrantFireEventCount = 0

    /** subject to cleanup when this event is completely distributed */
    private val subjectsToCleanUp = IdentityHashMap<Any, EventFormat<*, *, *>>()

    /** for proper dependency management, when a listener and subject aren't the same identity */
    private val listenersToRelatedSubjects = IdentityHashMap<Any, Any>()

    /** the last listener notified, the next one will be beyond it in the list */
    private var nextToNotify = 0

    /**
     * A mix of different subjects and listeners pairs in a deliberate order.
     * We should be careful not to make changes to this list directly and instead
     * create a copy as necessary.
     */
    private var subjectAndListeners: List<SubjectAndListener<*, *, *>> = emptyList()

    /**
     * We use copy-on-write on the listeners list. This is a copy of the
     * listeners list as it looked immediately before the current change
     * started. If there is no change going on (reentrantFireEventCount == 0),
     * then this should be null.
     */
    private var subjectsAndListenersForCurrentEvent: List<SubjectAndListener<*, *, *>>? = null

    /**
     * Rebuild the subject and listeners list so that all required invariants
     * are met with respect to notification order. That is, for any listener
     * T, all of the subjects S that T listens to have been updated before T
     * receives a change event from any S.
     */
    private fun orderSubjectsAndListeners(
        subjectsAndListeners: List<SubjectAndListener<*, *, *>>,
    ): List<SubjectAndListener<*, *, *>> {
        val result = ArrayList<SubjectAndListener<*, *, *>>()
        val sourceToPairs = IdentityHashMap<Any, MutableList<SubjectAndListener<*, *, *>>>()
        val targetToPairs = IdentityHashMap<Any, MutableList<SubjectAndListener<*, *, *>>>()
        val satisfied = Collections.newSetFromMap(IdentityHashMap<Any, Boolean>())

        for (subjectAndListener in subjectsAndListeners) {
            val source = subjectAndListener.subject
            val target = getRelatedSubject(subjectAndListener.listener)
            sourceToPairs.computeIfAbsent(source) { ArrayList(2) }.add(subjectAndListener)
            targetToPairs.computeIfAbsent(target) { ArrayList(2) }.add(subjectAndListener)

            satisfied.remove(target)
            if (!targetToPairs.containsKey(source)) {
                satisfied.add(source)
            }
        }

        val satisfiedToDo = ArrayDeque(satisfied)
        while (!satisfiedToDo.isEmpty()) {
            val subject = satisfiedToDo.removeFirst()
            val sourceTargets = sourceToPairs.getOrDefault(subject, emptyList())

            tryEachTarget@ for (target in sourceTargets) {
                val sourceTarget = getRelatedSubject(target.listener)
                val allSourcesForSourceTarget = targetToPairs.getOrDefault(sourceTarget, emptyList())
                if (allSourcesForSourceTarget.isEmpty()) {
                    continue
                }
                for (sourceAndTarget in allSourcesForSourceTarget) {
                    if (!satisfied.contains(sourceAndTarget.subject)) {
                        continue@tryEachTarget
                    }
                }

                result.addAll(allSourcesForSourceTarget)
                targetToPairs.remove(sourceTarget)
                satisfiedToDo.addLast(sourceTarget)
                satisfied.add(sourceTarget)
            }
        }

        if (!targetToPairs.isEmpty()) {
            throw IllegalStateException("Listener cycle detected, ${targetToPairs.values}")
        }

        return result
    }

    private fun getRelatedSubject(listener: Any): Any = listenersToRelatedSubjects[listener] ?: listener

    /**
     * Register the specified listener to receive events from the specified
     * subject whenever they are fired.
     */
    @Synchronized
    fun <Subject : Any, Listener : Any, Event : Any> addListener(
        subject: Subject,
        listener: Listener,
        eventFormat: EventFormat<Subject, Listener, Event>,
    ) {
        val unordered = updateListEventListeners(subject, listener, null, eventFormat)
        subjectAndListeners = orderSubjectsAndListeners(unordered)
    }

    /**
     * Deregister the specified listener from recieving events from the specified
     * subject.
     */
    @Synchronized
    fun removeListener(subject: Any, listener: Any) {
        subjectAndListeners = updateListEventListeners<Any, Any, Any>(subject, null, listener, null)
    }

    /**
     * Support method for adding and removing listeners, that also cleans up
     * stale listeners, such as those from weak references.
     *
     * @param listenerToAdd a listener to be added, or `null`
     * @param listenerToRemove a listener to be removed, or `null`
     */
    @Suppress("UNCHECKED_CAST")
    private fun <Subject : Any, Listener : Any, Event : Any> updateListEventListeners(
        subject: Subject,
        listenerToAdd: Listener?,
        listenerToRemove: Listener?,
        eventFormat: EventFormat<Subject, Listener, Event>?,
    ): List<SubjectAndListener<*, *, *>> {
        var listenerToRemoveVar = listenerToRemove
        var anticipatedSize = subjectAndListeners.size + if (listenerToAdd == null) -1 else 1
        if (anticipatedSize < 0) {
            anticipatedSize = 0
        }

        val result = ArrayList<SubjectAndListener<*, *, *>>(anticipatedSize)
        for (originalSubjectAndListener in subjectAndListeners) {
            if (
                originalSubjectAndListener.listener === listenerToRemoveVar &&
                    originalSubjectAndListener.subject === subject
            ) {
                listenerToRemoveVar = null
                continue
            }
            if (originalSubjectAndListener.isStale()) {
                continue
            }
            result.add(originalSubjectAndListener)
        }

        if (listenerToAdd != null) {
            result.add(
                SubjectAndListener(
                    subject,
                    listenerToAdd,
                    requireNotNull(eventFormat),
                ),
            )
        }

        return result
    }

    override fun setRelatedListener(subject: Any, relatedListener: Any) {
        addListener(relatedListener, subject, NoOpEventFormat)
    }

    override fun clearRelatedListener(subject: Any, relatedListener: Any) {
        removeListener(relatedListener, subject)
    }

    override fun setRelatedSubject(listener: Any, relatedSubject: Any) {
        listenersToRelatedSubjects[listener] = relatedSubject
    }

    override fun clearRelatedSubject(listener: Any) {
        listenersToRelatedSubjects.remove(listener)
    }

    /**
     * Get all listeners of the specified object.
     */
    @Synchronized
    @Suppress("UNCHECKED_CAST")
    fun <Listener : Any> getListeners(subject: Any): MutableList<Listener> {
        val result = ArrayList<Listener>()
        for (subjectAndListener in subjectAndListeners) {
            if (subjectAndListener.subject !== subject) {
                continue
            }
            result.add(subjectAndListener.listener as Listener)
        }
        return result
    }

    /**
     * Notify all listeners of the specified subject of the specified event.
     *
     * @param subject the event's source
     * @param event the event to send to all listeners
     * @param eventFormat the mechanism to notify listeners of the event, also
     *   used for a callback when this event is complete
     */
    fun <Subject : Any, Listener : Any, Event : Any> fireEvent(
        subject: Subject,
        event: Event,
        eventFormat: EventFormat<Subject, Listener, Event>,
    ) {
        if (reentrantFireEventCount == 0) {
            subjectsAndListenersForCurrentEvent = subjectAndListeners
            nextToNotify = Int.MAX_VALUE
        }

        reentrantFireEventCount++
        try {
            val previous = subjectsToCleanUp.put(subject, eventFormat)
            check(previous == null) { "Reentrant fireEvent() by \"$subject\"" }

            val currentListeners = subjectsAndListenersForCurrentEvent!!
            val subjectAndListenersSize = currentListeners.size
            for (i in 0 until subjectAndListenersSize) {
                val subjectAndListener = currentListeners[i]
                if (subjectAndListener.subject !== subject) continue
                if (i < nextToNotify) nextToNotify = i
                subjectAndListener.addPendingEventObject(event)
            }

            if (reentrantFireEventCount != 1) return

            var toRethrow: RuntimeException? = null
            while (true) {
                var nextToFire: SubjectAndListener<*, *, *>? = null
                for (i in nextToNotify until subjectAndListenersSize) {
                    val subjectAndListener = currentListeners[i]
                    if (subjectAndListener.hasPendingEvent()) {
                        nextToFire = subjectAndListener
                        nextToNotify = i + 1
                        break
                    }
                }

                if (nextToFire == null) break

                try {
                    nextToFire.firePendingEvent()
                } catch (e: RuntimeException) {
                    toRethrow = recordException(toRethrow, e)
                }
            }

            for ((cleanupSubject, cleanupEventFormat) in subjectsToCleanUp.entries) {
                try {
                    postEvent(cleanupEventFormat, cleanupSubject)
                } catch (e: RuntimeException) {
                    toRethrow = recordException(toRethrow, e)
                }
            }
            if (toRethrow != null) throw toRethrow
        } finally {
            reentrantFireEventCount--
            if (reentrantFireEventCount == 0) {
                subjectsAndListenersForCurrentEvent?.forEach { it.discardPendingEvent() }
                subjectsToCleanUp.clear()
                subjectsAndListenersForCurrentEvent = null
            }
        }
    }

    /** Adapt any observer-style interface to a common format. */
    interface EventFormat<Subject : Any, Listener : Any, Event : Any> {
        /** Fire the specified event to the specified listener. */
        fun fire(subject: Subject, event: Event, listener: Listener)

        /**
         * A callback made only after all listeners of the specified subject
         * have been notified of the specified event. This can be used as
         * a hook to clean up temporary datastructures for that event.
         */
        fun postEvent(subject: Subject)

        /**
         * Whether the listener is still valid. Usually a listener becomes stale
         * when a weak reference goes out of scope. If this method returns true,
         * the listener will be silently removed and no longer receive events.
         */
        fun isStale(subject: Subject, listener: Listener): Boolean
    }

    private object NoOpEventFormat : EventFormat<Any, Any, Any> {
        override fun fire(subject: Any, event: Any, listener: Any) {
            throw UnsupportedOperationException()
        }

        override fun postEvent(subject: Any) {
            throw UnsupportedOperationException()
        }

        override fun isStale(subject: Any, listener: Any): Boolean = false
    }

    /**
     * Manage a subject/listener pair, plus a possible event that is queued to
     * be fired to the listener from the subject.
     */
    private class SubjectAndListener<Subject : Any, Listener : Any, Event : Any>(
        val subject: Subject,
        val listener: Listener,
        private val eventFormat: EventFormat<Subject, Listener, Event>,
    ) {
        private var pendingEvent: Any? = null

        fun hasPendingEvent(): Boolean = pendingEvent != null

        fun isStale(): Boolean = eventFormat.isStale(subject, listener)

        fun addPendingEvent(pendingEvent: Event) {
            check(this.pendingEvent == null) { "Pending event already exists" }
            this.pendingEvent = pendingEvent
        }

        fun discardPendingEvent() {
            pendingEvent = null
        }

        @Suppress("UNCHECKED_CAST")
        fun addPendingEventObject(pendingEvent: Any) {
            addPendingEvent(pendingEvent as Event)
        }

        @Suppress("UNCHECKED_CAST")
        fun firePendingEvent() {
            assert(pendingEvent != null)
            try {
                eventFormat.fire(subject, pendingEvent as Event, listener)
            } finally {
                pendingEvent = null
            }
        }

        override fun toString(): String {
            val separator = if (hasPendingEvent()) ">>>" else "-->"
            return "$subject$separator$listener"
        }
    }

}

private fun recordException(
    primary: RuntimeException?,
    additional: RuntimeException,
): RuntimeException {
    if (primary == null) {
        return additional
    }
    if (primary !== additional) {
        primary.addSuppressed(additional)
    }
    return primary
}

private fun postEvent(
    eventFormat: SequenceDependenciesEventPublisher.EventFormat<*, *, *>,
    subject: Any,
) {
    postEventTyped(eventFormat, subject)
}

@Suppress("UNCHECKED_CAST")
private fun <Subject : Any> postEventTyped(
    eventFormat: SequenceDependenciesEventPublisher.EventFormat<Subject, *, *>,
    subject: Any,
) {
    eventFormat.postEvent(subject as Subject)
}
