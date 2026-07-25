package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.FilterList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.lang.reflect.Modifier

internal class MatcherContractMigrationBehaviorTest {
    @Test
    fun matcherRemainsASamWithItsCanonicalMethod() {
        val matcher = Matcher<String> { value -> value.length > 2 }

        assertFalse(matcher.matches("ab"))
        assertTrue(matcher.matches("abc"))
    }

    @Test
    fun listenerRemainsASamWithItsCanonicalMethod() {
        val matcher = Matcher<String> { true }
        val editor = MatcherEditor.fromMatcher(matcher)
        val event = MatcherEditor.Event(editor, MatcherEditor.Event.CHANGED, matcher)
        var received: MatcherEditor.Event<String>? = null
        val listener = MatcherEditor.Listener { event: MatcherEditor.Event<String> -> received = event }

        listener.changedMatcher(event)

        assertSame(event, received)
    }

    @Test
    fun companionFactoryKeepsMatcherIdentityWithoutAStaticBridge() {
        val matcher = Matcher<String> { value -> value.isNotEmpty() }

        val editor = MatcherEditor.fromMatcher(matcher)

        assertSame(matcher, editor.matcher)
        assertFalse(
            Modifier.isStatic(
                MatcherEditor.Companion::class.java.getMethod("fromMatcher", Matcher::class.java).modifiers,
            ),
        )
    }

    @Test
    fun eventsKeepBothSourceShapesAndConstants() {
        val matcher = Matcher<String> { true }
        val editor = MatcherEditor.fromMatcher(matcher)
        val editorEvent = MatcherEditor.Event(editor, MatcherEditor.Event.CONSTRAINED, matcher)
        val source = BasicEventList<String>()
        val filterList = FilterList(source)

        try {
            val filterEvent = MatcherEditor.Event(filterList, MatcherEditor.Event.RELAXED, matcher)

            assertSame(editor, editorEvent.matcherEditor)
            assertSame(editor, editorEvent.source)
            assertSame(matcher, editorEvent.matcher)
            assertEquals(MatcherEditor.Event.CONSTRAINED, editorEvent.type)
            assertNull(filterEvent.matcherEditor)
            assertSame(filterList, filterEvent.source)
            assertEquals(
                listOf(0, 1, 2, 3, 4),
                listOf(
                    MatcherEditor.Event.MATCH_ALL,
                    MatcherEditor.Event.MATCH_NONE,
                    MatcherEditor.Event.CONSTRAINED,
                    MatcherEditor.Event.RELAXED,
                    MatcherEditor.Event.CHANGED,
                ),
            )
        } finally {
            filterList.dispose()
        }
    }
}
