package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.AbstractMatcherEditor
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.MatcherEditor
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.beans.PropertyChangeEvent

internal class MatcherUtilityMigrationBehaviorTest {
    @Test
    fun propertyNamesCanBeIncludedOrExcluded() {
        val includedNames = PropertyEventNameMatcher(true, "name", "age", "name")
        val excludedNames = PropertyEventNameMatcher(false, listOf("internal"))

        assertTrue(includedNames.isMatchPropertyNames)
        assertTrue(includedNames.matches(event("name")))
        assertFalse(includedNames.matches(event("other")))
        assertFalse(excludedNames.isMatchPropertyNames)
        assertFalse(excludedNames.matches(event("internal")))
        assertTrue(excludedNames.matches(event("name")))
    }

    @Test
    fun weakProxyRewritesEventSourceAndSupportsIdentityRemoval() {
        val source = MutableMatcherEditor<String>()
        val proxy = WeakReferenceMatcherEditor(source)
        val events = mutableListOf<MatcherEditor.Event<String>>()
        val listener = MatcherEditor.Listener { event: MatcherEditor.Event<String> -> events += event }
        val longerThanThree = Matcher<String> { value -> value.length > 3 }

        proxy.addMatcherEditorListener(listener)
        source.changeTo(longerThanThree)

        assertSame(source.matcher, proxy.matcher)
        assertEquals(1, events.size)
        assertSame(proxy, events.single().matcherEditor)
        assertSame(longerThanThree, events.single().matcher)

        proxy.removeMatcherEditorListener(listener)
        source.changeTo { true }

        assertEquals(1, events.size)
    }

    private fun event(propertyName: String): PropertyChangeEvent =
        PropertyChangeEvent(this, propertyName, null, null)

    private class MutableMatcherEditor<E> : AbstractMatcherEditor<E>() {
        fun changeTo(newMatcher: Matcher<E>) {
            fireChanged(newMatcher)
        }
    }
}
