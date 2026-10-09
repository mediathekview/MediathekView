/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.FilterList
import java.util.*

/** Produces immutable matchers and publishes events when the active matcher changes. */
interface MatcherEditor<E> {
    fun addMatcherEditorListener(listener: Listener<E>)

    fun removeMatcherEditorListener(listener: Listener<E>)

    val matcher: Matcher<E>

    fun interface Listener<E> : EventListener {
        fun changedMatcher(matcherEvent: Event<E>)
    }

    /** Describes the relationship between a new matcher and its predecessor. */
    open class Event<E> private constructor(
        source: Any,
        open val matcherEditor: MatcherEditor<E>?,
        open val type: Int,
        open val matcher: Matcher<E>,
    ) : EventObject(source) {
        constructor(matcherEditor: MatcherEditor<E>, changeType: Int, matcher: Matcher<E>) : this(
            source = matcherEditor,
            matcherEditor = matcherEditor,
            type = changeType,
            matcher = matcher,
        )

        constructor(eventSource: FilterList<E>, changeType: Int, matcher: Matcher<E>) : this(
            source = eventSource,
            matcherEditor = null,
            type = changeType,
            matcher = matcher,
        )

        companion object {
            const val MATCH_ALL = 0
            const val MATCH_NONE = 1
            const val CONSTRAINED = 2
            const val RELAXED = 3
            const val CHANGED = 4
        }
    }

    companion object {
        fun <E> fromMatcher(matcher: Matcher<E>): MatcherEditor<E> = FixedMatcherEditor(matcher)
    }
}
