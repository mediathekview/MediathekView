package ca.odell.glazedlists.matchers

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class AbstractMatcherEditorMigrationBehaviorTest {
    @Test
    fun matcherTransitionsKeepStateAndEventTypes() {
        val editor = ExposedMatcherEditor<String>()
        val events = mutableListOf<MatcherEditor.Event<String>>()
        editor.addMatcherEditorListener { event -> events += event }
        val changed = Matcher<String> { value -> value.startsWith('a') }
        val constrained = Matcher<String> { value -> value.startsWith("ab") }
        val relaxed = Matcher<String> { value -> value.isNotEmpty() }

        assertTrue(editor.matchingAll)
        assertFalse(editor.matchingNone)

        editor.changeTo(changed)
        editor.constrainTo(constrained)
        editor.relaxTo(relaxed)
        editor.matchNone()

        assertTrue(editor.matchingNone)
        assertSame(Matchers.falseMatcher<String>(), editor.matcher)

        editor.matchAll()

        assertTrue(editor.matchingAll)
        assertSame(Matchers.trueMatcher<String>(), editor.matcher)
        assertEquals(
            listOf(
                MatcherEditor.Event.CHANGED,
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.RELAXED,
                MatcherEditor.Event.MATCH_NONE,
                MatcherEditor.Event.MATCH_ALL,
            ),
            events.map { event -> event.type },
        )
        assertTrue(events.all { event -> event.matcherEditor === editor })
        assertEquals(listOf(changed, constrained, relaxed), events.take(3).map { event -> event.matcher })
    }

    @Test
    fun listenersUseLifoSnapshotWhenRegistrationChangesDuringDispatch() {
        val editor = ExposedMatcherEditor<String>()
        val calls = mutableListOf<String>()
        val first = MatcherEditor.Listener<String> { calls += "first" }
        val removing = MatcherEditor.Listener<String> {
            calls += "removing"
            editor.removeMatcherEditorListener(first)
        }
        editor.addMatcherEditorListener(first)
        editor.addMatcherEditorListener(removing)

        editor.changeTo { true }

        assertEquals(listOf("removing", "first"), calls)

        calls.clear()
        editor.changeTo { false }

        assertEquals(listOf("removing"), calls)
    }

    private class ExposedMatcherEditor<E> : AbstractMatcherEditor<E>() {
        val matchingAll: Boolean
            get() = isCurrentlyMatchingAll

        val matchingNone: Boolean
            get() = isCurrentlyMatchingNone

        fun changeTo(matcher: Matcher<E>) = fireChanged(matcher)

        fun constrainTo(matcher: Matcher<E>) = fireConstrained(matcher)

        fun relaxTo(matcher: Matcher<E>) = fireRelaxed(matcher)

        fun matchAll() = fireMatchAll()

        fun matchNone() = fireMatchNone()
    }
}
