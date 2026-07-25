package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.Filterator
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class MatcherEditorMigrationBehaviorTest {
    @Test
    fun fixedMatcherEditorKeepsTheSuppliedMatcher() {
        val fixedMatcher = Matcher<String> { value -> value.startsWith('a') }

        val editor = MatcherEditor.fromMatcher(fixedMatcher)

        assertSame(fixedMatcher, editor.matcher)
        assertTrue(editor.matcher.matches("apple"))
        assertFalse(editor.matcher.matches("pear"))
    }

    @Test
    fun rangeMatcherEditorUsesInclusiveNormalizedBounds() {
        val editor = RangeMatcherEditor<Int, Int>()

        assertNull(editor.filterator)
        assertTrue(editor.matcher.matches(Int.MIN_VALUE))

        editor.setRange(8, 3)

        assertFalse(editor.matcher.matches(2))
        assertTrue(editor.matcher.matches(3))
        assertTrue(editor.matcher.matches(8))
        assertFalse(editor.matcher.matches(9))
    }

    @Test
    fun rangeChangesKeepMatcherEventClassifications() {
        val editor = RangeMatcherEditor<Int, Int>()
        val eventTypes = mutableListOf<Int>()
        editor.addMatcherEditorListener { event -> eventTypes += event.type }

        editor.setRange(2, 8)
        editor.setRange(3, 7)
        editor.setRange(2, 8)
        editor.setRange(4, 9)
        editor.setRange(9, 4)
        editor.setRange(null, null)
        editor.setRange(null, null)

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
    fun rangeMatcherEditorUsesFilteratorValues() {
        val scoreFilterator = Filterator<Int, ScoredElement> { values, element ->
            values.addAll(element.scores)
        }
        val editor = RangeMatcherEditor(scoreFilterator)

        assertSame(scoreFilterator, editor.filterator)
        editor.setRange(2, 4)

        assertFalse(editor.matcher.matches(ScoredElement(emptyList())))
        assertFalse(editor.matcher.matches(ScoredElement(listOf(1, 5))))
        assertTrue(editor.matcher.matches(ScoredElement(listOf(1, 3, 5))))
    }

    private data class ScoredElement(val scores: List<Int>)
}
