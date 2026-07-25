package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.BasicEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class CompositeMatcherEditorMigrationBehaviorTest {
    @Test
    fun delegatesCombineInAndAndOrModes() {
        val startsWithA = MutableMatcherEditor(Matcher<String> { value -> value.startsWith('a') })
        val endsWithZ = MutableMatcherEditor(Matcher<String> { value -> value.endsWith('z') })
        val delegates = BasicEventList<MatcherEditor<String>>()
        delegates += startsWithA
        delegates += endsWithZ
        val composite = CompositeMatcherEditor(delegates)
        val eventTypes = mutableListOf<Int>()
        composite.addMatcherEditorListener { event -> eventTypes += event.type }

        assertTrue(composite.matcher.matches("abz"))
        assertFalse(composite.matcher.matches("abc"))
        assertFalse(composite.matcher.matches("zzz"))

        composite.mode = CompositeMatcherEditor.OR

        assertTrue(composite.matcher.matches("abc"))
        assertTrue(composite.matcher.matches("zzz"))
        assertFalse(composite.matcher.matches("xxx"))

        composite.mode = CompositeMatcherEditor.AND

        assertEquals(
            listOf(MatcherEditor.Event.RELAXED, MatcherEditor.Event.CONSTRAINED),
            eventTypes,
        )
    }

    @Test
    fun andModeClassifiesDelegateListChanges() {
        val delegates = BasicEventList<MatcherEditor<String>>()
        val composite = CompositeMatcherEditor(delegates)
        val eventTypes = mutableListOf<Int>()
        composite.addMatcherEditorListener { event -> eventTypes += event.type }
        val first = MutableMatcherEditor(Matcher<String> { value -> value.isNotEmpty() })
        val second = MutableMatcherEditor(Matcher<String> { value -> value.length > 1 })

        delegates += first
        delegates += second
        delegates.removeAt(1)
        delegates.clear()

        assertEquals(
            listOf(
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.RELAXED,
                MatcherEditor.Event.MATCH_ALL,
            ),
            eventTypes,
        )
        assertSame(Matchers.trueMatcher<String>(), composite.matcher)
    }

    @Test
    fun replacingDelegateMovesListenerToTheNewEditor() {
        val original = MutableMatcherEditor(Matcher<String> { true })
        val replacement = MutableMatcherEditor(Matcher<String> { false })
        val delegates = BasicEventList<MatcherEditor<String>>()
        delegates += original
        val composite = CompositeMatcherEditor(delegates)
        val eventTypes = mutableListOf<Int>()
        composite.addMatcherEditorListener { event -> eventTypes += event.type }

        delegates[0] = replacement
        original.changeTo { false }
        replacement.changeTo { true }

        assertEquals(
            listOf(MatcherEditor.Event.CHANGED, MatcherEditor.Event.CHANGED),
            eventTypes,
        )
        assertTrue(composite.matcher.matches("anything"))
    }

    private class MutableMatcherEditor<E>(initialMatcher: Matcher<E>) : AbstractMatcherEditor<E>() {
        init {
            fireChanged(initialMatcher)
        }

        fun changeTo(matcher: Matcher<E>) {
            fireChanged(matcher)
        }
    }
}
