package ca.odell.glazedlists.impl.filter

import ca.odell.glazedlists.TextFilterable
import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.matchers.TextMatcherEditor
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class TextMatcherMigrationBehaviorTest {
    private val valueFilterator = TextFilterator<TestEntry> { values, entry -> values += entry.value }

    @Test
    fun constructionKeepsNormalizationMatchingAndSearchTermAccessors() {
        val matcher = createMatcher("new", "news", "")

        assertArrayEquals(arrayOf("news"), matcher.searchTermStrings)
        assertEquals(listOf("news"), matcher.searchTerms.map(SearchTerm<TestEntry>::text))
        assertTrue(matcher.matches(TestEntry("evening news")))
        assertFalse(matcher.matches(TestEntry("newly")))
    }

    @Test
    fun equalityKeepsIgnoringOrderDuplicatesAndFilterator() {
        val first = createMatcher("evening", "news")
        val alternateFilterator = TextFilterator<TestEntry> { values, entry -> values += entry.alternate }
        val second = TextMatcher(
            arrayOf(SearchTerm("news"), SearchTerm("evening"), SearchTerm("news")),
            alternateFilterator,
            TextMatcherEditor.CONTAINS,
            TextMatcherEditor.IDENTICAL_STRATEGY,
        )

        assertEquals(first, second)
        assertEquals(first.hashCode(), second.hashCode())

        val changedMode = first.newMode(TextMatcherEditor.STARTS_WITH)
        assertNotEquals(first, changedMode)
        assertEquals(TextMatcherEditor.STARTS_WITH, changedMode.mode)

        val changedFilterator = first.newFilterator(alternateFilterator)
        assertEquals(first, changedFilterator)
        assertFalse(changedFilterator.matches(TestEntry("evening news", "other")))
        assertTrue(changedFilterator.matches(TestEntry("other", "evening news")))

        val changedStrategy = first.newStrategy(TextMatcherEditor.NORMALIZED_STRATEGY)
        assertNotEquals(first, changedStrategy)
        assertSame(TextMatcherEditor.NORMALIZED_STRATEGY, changedStrategy.strategy)
    }

    @Test
    fun nullFilteratorKeepsTextFilterableFallback() {
        val matcher = TextMatcher(
            arrayOf(SearchTerm<FilterableEntry>("news")),
            null,
            TextMatcherEditor.CONTAINS,
            TextMatcherEditor.IDENTICAL_STRATEGY,
        )

        assertTrue(matcher.matches(FilterableEntry("evening news")))
        assertFalse(matcher.matches(FilterableEntry("weather")))
    }

    @Test
    fun normalizedRegularExpressionCombinationRemainsRejected() {
        assertThrows(IllegalArgumentException::class.java) {
            TextMatcher(
                arrayOf(SearchTerm(".*")),
                valueFilterator,
                TextMatcherEditor.REGULAR_EXPRESSION,
                TextMatcherEditor.NORMALIZED_STRATEGY,
            )
        }
    }

    private fun createMatcher(vararg terms: String): TextMatcher<TestEntry> =
        TextMatcher(
            Array(terms.size) { index -> SearchTerm(terms[index]) },
            valueFilterator,
            TextMatcherEditor.CONTAINS,
            TextMatcherEditor.IDENTICAL_STRATEGY,
        )

    private data class TestEntry(val value: String, val alternate: String = value)

    private data class FilterableEntry(val value: String) : TextFilterable {
        override fun getFilterStrings(baseList: MutableList<String>) {
            baseList += value
        }
    }
}
