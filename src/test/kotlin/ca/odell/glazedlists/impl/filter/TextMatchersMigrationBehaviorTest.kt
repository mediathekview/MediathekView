package ca.odell.glazedlists.impl.filter

import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.matchers.SearchEngineTextMatcherEditor
import ca.odell.glazedlists.matchers.TextMatcherEditor
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class TextMatchersMigrationBehaviorTest {
    private val valueFilterator = TextFilterator<TestEntry> { values, entry -> values += entry.value }

    @Test
    fun parseKeepsOperatorsPhrasesFieldsAndUnknownFieldText() {
        val cityFilterator = TextFilterator<TestEntry> { values, entry -> values += entry.city }
        val cityField = SearchEngineTextMatcherEditor.Field("city", cityFilterator)

        val terms = TextMatchers.parse(
            "alpha +\"two words\" -omit city:Berlin unknown:value \"unfinished",
            setOf(cityField),
        )

        assertEquals(
            listOf("alpha", "two words", "omit", "Berlin", "unknown:value", "unfinished"),
            terms.map(SearchTerm<TestEntry>::text),
        )
        assertTrue(terms[1].isRequired)
        assertTrue(terms[2].isNegated)
        assertSame(cityField, terms[3].field)
        assertNull(terms[4].field)
    }

    @Test
    fun normalizationKeepsFieldTermsAndRequiredTermsWhileRemovingRedundancy() {
        val cityField = SearchEngineTextMatcherEditor.Field("city", valueFilterator)
        val filters = arrayOf(
            SearchTerm("new"),
            SearchTerm("news"),
            SearchTerm("new", isNegated = false, isRequired = true, field = null),
            SearchTerm("black", isNegated = true, isRequired = false, field = null),
            SearchTerm("blackened", isNegated = true, isRequired = false, field = null),
            SearchTerm("city", isNegated = false, isRequired = false, field = cityField),
        )

        val normalized = TextMatchers.normalizeSearchTerms(
            filters,
            TextMatcherEditor.IDENTICAL_STRATEGY as TextSearchStrategy.Factory,
        )

        assertEquals(listOf("city", "black", "news", "new"), normalized.map(SearchTerm<TestEntry>::text))
        assertSame(cityField, normalized.first().field)
        assertTrue(normalized.last().isRequired)
    }

    @Test
    fun normalizedStrategyMapsLatinDiacritics() {
        val normalized = TextMatchers.normalizeSearchTerms(
            arrayOf(SearchTerm<String>("résumé")),
            TextMatcherEditor.NORMALIZED_STRATEGY as TextSearchStrategy.Factory,
        )

        assertArrayEquals(arrayOf("resume"), normalized.map(SearchTerm<String>::text).toTypedArray())
    }

    @Test
    fun matchesKeepsLegacyArbitraryFilterStringObjects() {
        val strategyFactory = TextMatcherEditor.IDENTICAL_STRATEGY as TextSearchStrategy.Factory
        val searchStrategy = strategyFactory.create(TextMatcherEditor.CONTAINS, "123").apply {
            setSubtext("123")
        }
        val arbitraryValueFilterator = TextFilterator<TestEntry> { values, _ ->
            @Suppress("UNCHECKED_CAST")
            (values as MutableList<Any?>) += 12345
        }

        assertTrue(
            TextMatchers.matches(
                mutableListOf(),
                arbitraryValueFilterator,
                arrayOf(SearchTerm("123")),
                arrayOf(searchStrategy),
                TestEntry("ignored"),
            ),
        )
    }

    @Test
    fun matcherComparisonKeepsConstrainedRelaxedAndUnclassifiableCases() {
        val broad = createMatcher("new")
        val narrow = createMatcher("news")

        assertTrue(TextMatchers.isMatcherConstrained(broad, narrow))
        assertTrue(TextMatchers.isMatcherRelaxed(narrow, broad))
        assertFalse(TextMatchers.isMatcherConstrained(broad, broad))

        assertFalse(
            TextMatchers.isMatcherConstrained(
                broad.newMode(TextMatcherEditor.STARTS_WITH),
                narrow.newMode(TextMatcherEditor.CONTAINS),
            ),
        )
        assertFalse(
            TextMatchers.isMatcherConstrained(
                broad,
                narrow.newStrategy(TextMatcherEditor.NORMALIZED_STRATEGY),
            ),
        )
        assertFalse(
            TextMatchers.isMatcherConstrained(
                broad.newMode(TextMatcherEditor.EXACT),
                narrow.newMode(TextMatcherEditor.EXACT),
            ),
        )
    }

    private fun createMatcher(term: String): TextMatcher<TestEntry> =
        TextMatcher(
            arrayOf(SearchTerm(term)),
            valueFilterator,
            TextMatcherEditor.CONTAINS,
            TextMatcherEditor.IDENTICAL_STRATEGY,
        )

    private data class TestEntry(val value: String, val city: String = value)
}
