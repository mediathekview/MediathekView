/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

import java.util.concurrent.CopyOnWriteArrayList

/**
 * Base implementation for matcher editors that manages listeners and creates
 * consistently sourced matcher events.
 */
abstract class AbstractMatcherEditorListenerSupport<E> : MatcherEditor<E> {
    private val listenerList = CopyOnWriteArrayList<MatcherEditor.Listener<E>>()

    final override fun addMatcherEditorListener(listener: MatcherEditor.Listener<E>) {
        listenerList += listener
    }

    final override fun removeMatcherEditorListener(listener: MatcherEditor.Listener<E>) {
        listenerList -= listener
    }

    /** Delivers an event to a stable listener snapshot in LIFO order. */
    protected fun fireChangedMatcher(event: MatcherEditor.Event<E>) {
        val listeners = listenerList.toList()
        for (index in listeners.lastIndex downTo 0) {
            listeners[index].changedMatcher(event)
        }
    }

    protected fun createChangedEvent(matcher: Matcher<E>): MatcherEditor.Event<E> =
        createEvent(MatcherEditor.Event.CHANGED, matcher)

    protected fun createConstrainedEvent(matcher: Matcher<E>): MatcherEditor.Event<E> =
        createEvent(MatcherEditor.Event.CONSTRAINED, matcher)

    protected fun createRelaxedEvent(matcher: Matcher<E>): MatcherEditor.Event<E> =
        createEvent(MatcherEditor.Event.RELAXED, matcher)

    protected fun createMatchNoneEvent(matcher: Matcher<E>): MatcherEditor.Event<E> =
        createEvent(MatcherEditor.Event.MATCH_NONE, matcher)

    protected fun createMatchAllEvent(matcher: Matcher<E>): MatcherEditor.Event<E> =
        createEvent(MatcherEditor.Event.MATCH_ALL, matcher)

    private fun createEvent(eventType: Int, matcher: Matcher<E>): MatcherEditor.Event<E> =
        MatcherEditor.Event(this, eventType, matcher)
}
