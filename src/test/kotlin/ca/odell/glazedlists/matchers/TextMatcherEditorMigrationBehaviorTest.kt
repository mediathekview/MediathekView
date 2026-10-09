package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.TextFilterator
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class TextMatcherEditorMigrationBehaviorTest {
    private val valueFilterator = TextFilterator<TestEntry> { values, entry -> values += entry.value }

    @Test
    fun modesKeepContainsStartsWithExactAndRegularExpressionBehavior() {
        val editor = TextMatcherEditor(valueFilterator)
        editor.setFilterText(arrayOf("news"))

        assertTrue(editor.matcher.matches(TestEntry("evening news")))

        editor.mode = TextMatcherEditor.STARTS_WITH
        assertFalse(editor.matcher.matches(TestEntry("evening news")))
        assertTrue(editor.matcher.matches(TestEntry("newspaper")))

        editor.mode = TextMatcherEditor.EXACT
        assertFalse(editor.matcher.matches(TestEntry("newspaper")))
        assertTrue(editor.matcher.matches(TestEntry("NEWS")))

        editor.mode = TextMatcherEditor.REGULAR_EXPRESSION
        editor.setFilterText(arrayOf("n.*s"))
        assertTrue(editor.matcher.matches(TestEntry("news")))
        assertFalse(editor.matcher.matches(TestEntry("evening news")))
    }

    @Test
    fun normalizedStrategyKeepsDiacriticMatchingAndStrategyValidation() {
        val editor = TextMatcherEditor(valueFilterator)
        editor.setFilterText(arrayOf("resume"))
        assertFalse(editor.matcher.matches(TestEntry("résumé")))

        editor.strategy = TextMatcherEditor.NORMALIZED_STRATEGY
        assertTrue(editor.matcher.matches(TestEntry("résumé")))

        assertThrows(IllegalArgumentException::class.java) { editor.strategy = Any() }
        assertThrows(IllegalArgumentException::class.java) { editor.strategy = null }
        assertSame(TextMatcherEditor.NORMALIZED_STRATEGY, editor.strategy)
    }

    @Test
    fun modeAndFilterChangesKeepSpecificMatcherEventTypes() {
        val editor = TextMatcherEditor(valueFilterator)
        val eventTypes = mutableListOf<Int>()
        editor.addMatcherEditorListener { event -> eventTypes += event.type }

        editor.setFilterText(arrayOf("news"))
        editor.mode = TextMatcherEditor.STARTS_WITH
        editor.mode = TextMatcherEditor.CONTAINS
        editor.mode = TextMatcherEditor.EXACT
        editor.setFilterText(emptyArray())
        editor.setFilterText(emptyArray())

        assertEquals(
            listOf(
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.RELAXED,
                MatcherEditor.Event.CHANGED,
                MatcherEditor.Event.MATCH_ALL,
            ),
            eventTypes,
        )
    }

    @Test
    fun changingFilteratorRebuildsAnActiveMatcherOnlyOnce() {
        val editor = TextMatcherEditor(valueFilterator)
        editor.setFilterText(arrayOf("Berlin"))
        val eventTypes = mutableListOf<Int>()
        editor.addMatcherEditorListener { event -> eventTypes += event.type }

        val alternateFilterator = TextFilterator<TestEntry> { values, entry -> values += entry.alternate }
        editor.filterator = alternateFilterator

        assertEquals(listOf(MatcherEditor.Event.CHANGED), eventTypes)
        assertFalse(editor.matcher.matches(TestEntry("Berlin", "Hamburg")))
        assertTrue(editor.matcher.matches(TestEntry("Hamburg", "Berlin")))

        editor.filterator = alternateFilterator
        assertEquals(1, eventTypes.size)
    }

    @Test
    fun invalidModeIsRejectedWithoutChangingTheCurrentMode() {
        val editor = TextMatcherEditor<String>()

        assertThrows(IllegalArgumentException::class.java) { editor.mode = -1 }
        assertEquals(TextMatcherEditor.CONTAINS, editor.mode)
    }

    private data class TestEntry(val value: String, val alternate: String = value)
}
