package ca.odell.glazedlists.matchers

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class ThresholdMatcherEditorMigrationBehaviorTest {
    @Test
    fun comparisonOperationsKeepTheirBoundaryBehavior() {
        val expectedMatches = mapOf(
            ThresholdMatcherEditor.GREATER_THAN to listOf(false, false, true),
            ThresholdMatcherEditor.GREATER_THAN_OR_EQUAL to listOf(false, true, true),
            ThresholdMatcherEditor.LESS_THAN to listOf(true, false, false),
            ThresholdMatcherEditor.LESS_THAN_OR_EQUAL to listOf(true, true, false),
            ThresholdMatcherEditor.EQUAL to listOf(false, true, false),
            ThresholdMatcherEditor.NOT_EQUAL to listOf(true, false, true),
        )

        for ((operation, matches) in expectedMatches) {
            val editor = ThresholdMatcherEditor<Int, Int>(5, operation)
            assertEquals(matches, listOf(4, 5, 6).map(editor.matcher::matches))
        }
    }

    @Test
    fun thresholdAndOperationChangesKeepEventClassifications() {
        val editor = ThresholdMatcherEditor<Int, Int>(5)
        val eventTypes = mutableListOf<Int>()
        editor.addMatcherEditorListener { event -> eventTypes += event.type }

        editor.threshold = 6
        editor.threshold = 4
        editor.matchOperation = ThresholdMatcherEditor.GREATER_THAN_OR_EQUAL
        editor.matchOperation = ThresholdMatcherEditor.LESS_THAN

        assertEquals(
            listOf(
                MatcherEditor.Event.CONSTRAINED,
                MatcherEditor.Event.RELAXED,
                MatcherEditor.Event.RELAXED,
                MatcherEditor.Event.CHANGED,
            ),
            eventTypes,
        )
    }

    @Test
    fun nullComparatorRestoresNaturalOrdering() {
        val reverseOrder = Comparator.reverseOrder<Int>()
        val editor = ThresholdMatcherEditor<Int, Int>(5, comparator = reverseOrder)

        assertTrue(editor.matcher.matches(4))
        assertSame(reverseOrder, editor.comparator)

        editor.comparator = null

        assertFalse(editor.matcher.matches(4))
        assertTrue(editor.matcher.matches(6))
    }

    @Test
    fun matchOperationRejectsNull() {
        val editor = ThresholdMatcherEditor<Int, Int>()

        assertThrows(IllegalArgumentException::class.java) {
            editor.matchOperation = null
        }
    }
}
