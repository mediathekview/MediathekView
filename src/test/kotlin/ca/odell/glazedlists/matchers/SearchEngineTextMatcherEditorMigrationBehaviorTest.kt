package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.TextFilterator
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class SearchEngineTextMatcherEditorMigrationBehaviorTest {
    private val fullText = TextFilterator<TestEntry> { values, entry ->
        values += entry.title
        values += entry.city
    }
    private val city = TextFilterator<TestEntry> { values, entry -> values += entry.city }

    @Test
    fun fieldsRemainDefensiveAndFieldEqualityDependsOnlyOnName() {
        val editor = SearchEngineTextMatcherEditor(fullText)
        val cityField = SearchEngineTextMatcherEditor.Field("city", city)
        val suppliedFields = mutableSetOf(cityField)

        editor.fields = suppliedFields
        suppliedFields.clear()
        assertEquals(setOf(cityField), editor.fields)

        val returnedFields = editor.fields.toMutableSet()
        returnedFields.clear()
        assertEquals(setOf(cityField), editor.fields)

        val alternateFilterator = TextFilterator<TestEntry> { values, entry -> values += entry.title }
        assertEquals(cityField, SearchEngineTextMatcherEditor.Field("city", alternateFilterator))
        assertEquals(cityField.hashCode(), SearchEngineTextMatcherEditor.Field("city", alternateFilterator).hashCode())
    }

    @Test
    fun refilterKeepsFieldPhraseRequiredAndExcludedTermBehavior() {
        val editor = SearchEngineTextMatcherEditor(fullText)
        editor.fields = setOf(SearchEngineTextMatcherEditor.Field("city", city))

        editor.refilter("+city:Berlin \"Evening News\" -sports")

        assertTrue(editor.matcher.matches(TestEntry("Evening News", "Berlin")))
        assertFalse(editor.matcher.matches(TestEntry("Evening News sports", "Berlin")))
        assertFalse(editor.matcher.matches(TestEntry("Evening News", "Hamburg")))
        assertFalse(editor.matcher.matches(TestEntry("Morning News", "Berlin")))
    }

    private data class TestEntry(val title: String, val city: String)
}
