package ca.odell.glazedlists

import ca.odell.glazedlists.impl.filter.SearchTerm
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class SearchTermTest {
    @Test
    fun replacementPreservesMetadataButNotScratchState() {
        val original = SearchTerm<Any>("old", isNegated = true, isRequired = true, field = null)
        original.fieldFilterStrings += "temporary"

        val replacement = original.newSearchTerm("new")

        assertEquals("new", replacement.text)
        assertTrue(replacement.isNegated)
        assertTrue(replacement.isRequired)
        assertTrue(replacement.fieldFilterStrings.isEmpty())
    }

    @Test
    fun equalityIgnoresReusableFilterStrings() {
        val first = SearchTerm<Any>("term")
        val second = SearchTerm<Any>("term")
        first.fieldFilterStrings += "extracted"

        assertEquals(first, second)
        assertEquals(first.hashCode(), second.hashCode())
        assertNotEquals(first, SearchTerm<Any>("other"))
    }

    @Test
    fun constrainmentAndRelaxationPreservePositiveAndNegativeSemantics() {
        val broad = SearchTerm<Any>("cat")
        val narrow = SearchTerm<Any>("catalog")
        assertTrue(narrow.isConstrainment(broad))
        assertTrue(broad.isRelaxation(narrow))

        val negatedBroad = SearchTerm<Any>("cat", isNegated = true, isRequired = false, field = null)
        val negatedNarrow = SearchTerm<Any>("catalog", isNegated = true, isRequired = false, field = null)
        assertTrue(negatedBroad.isConstrainment(negatedNarrow))
        assertFalse(broad.isConstrainment(negatedBroad))
        assertFalse(broad.isConstrainment(SearchTerm<Any>("cat")))
    }
}
