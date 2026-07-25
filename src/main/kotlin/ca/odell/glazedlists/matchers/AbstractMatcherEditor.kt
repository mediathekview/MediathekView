/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

/** Base matcher editor that owns the current matcher and its event transitions. */
abstract class AbstractMatcherEditor<E> : AbstractMatcherEditorListenerSupport<E>() {
    private var currentMatcher: Matcher<E> = Matchers.trueMatcher()

    final override val matcher: Matcher<E>
        get() = currentMatcher

    protected fun fireMatchAll() {
        currentMatcher = Matchers.trueMatcher()
        fireChangedMatcher(createMatchAllEvent(currentMatcher))
    }

    protected fun fireChanged(matcher: Matcher<E>) {
        currentMatcher = matcher
        fireChangedMatcher(createChangedEvent(currentMatcher))
    }

    protected fun fireConstrained(matcher: Matcher<E>) {
        currentMatcher = matcher
        fireChangedMatcher(createConstrainedEvent(currentMatcher))
    }

    protected fun fireRelaxed(matcher: Matcher<E>) {
        currentMatcher = matcher
        fireChangedMatcher(createRelaxedEvent(currentMatcher))
    }

    protected fun fireMatchNone() {
        currentMatcher = Matchers.falseMatcher()
        fireChangedMatcher(createMatchNoneEvent(currentMatcher))
    }

    protected val isCurrentlyMatchingAll: Boolean
        get() = currentMatcher === Matchers.trueMatcher<E>()

    protected val isCurrentlyMatchingNone: Boolean
        get() = currentMatcher === Matchers.falseMatcher<E>()
}
