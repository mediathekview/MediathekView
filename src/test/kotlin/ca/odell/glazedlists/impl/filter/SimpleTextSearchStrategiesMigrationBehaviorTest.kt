package ca.odell.glazedlists.impl.filter

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test
import java.util.regex.PatternSyntaxException

internal class SimpleTextSearchStrategiesMigrationBehaviorTest {
    @Test
    fun startsWithStrategyKeepsInitializationPrefixCaseAndMappingBehavior() {
        val strategy = StartsWithCaseInsensitiveTextSearchStrategy()

        assertThrows(IllegalStateException::class.java) { strategy.indexOf("text") }

        strategy.setSubtext("")
        assertEquals(0, strategy.indexOf(""))
        assertEquals(0, strategy.indexOf("anything"))

        strategy.setSubtext("N")
        assertEquals(0, strategy.indexOf("news"))
        assertEquals(-1, strategy.indexOf(""))
        assertEquals(-1, strategy.indexOf("weather"))

        strategy.setSubtext("Ne")
        assertEquals(0, strategy.indexOf("news"))
        assertEquals(0, strategy.indexOf("NEWS"))
        assertEquals(-1, strategy.indexOf("evening news"))
        assertEquals(-1, strategy.indexOf("n"))

        val characterMap = CharArray('é'.code + 1) { index -> index.toChar() }
        characterMap['é'.code] = 'e'
        strategy.setCharacterMap(characterMap)
        strategy.setSubtext("re")
        assertEquals(0, strategy.indexOf("résumé"))
    }

    @Test
    fun exactStrategyKeepsLengthCaseEmptyAndMappingBehavior() {
        val strategy = ExactCaseInsensitiveTextSearchStrategy()

        strategy.setSubtext("News")
        assertEquals(0, strategy.indexOf("news"))
        assertEquals(0, strategy.indexOf("NEWS"))
        assertEquals(-1, strategy.indexOf("new"))
        assertEquals(-1, strategy.indexOf("newsletter"))

        strategy.setSubtext("")
        assertEquals(0, strategy.indexOf(""))
        assertEquals(-1, strategy.indexOf("anything"))

        val characterMap = CharArray('é'.code + 1) { index -> index.toChar() }
        characterMap['é'.code] = 'e'
        strategy.setCharacterMap(characterMap)
        strategy.setSubtext("resume")
        assertEquals(0, strategy.indexOf("résumé"))
    }

    @Test
    fun singleCharacterStrategyKeepsValidationCaseAndMappingBehavior() {
        val strategy = SingleCharacterCaseInsensitiveTextSearchStrategy()

        assertThrows(IllegalStateException::class.java) { strategy.indexOf("text") }
        assertThrows(IllegalArgumentException::class.java) { strategy.setSubtext("") }
        assertThrows(IllegalArgumentException::class.java) { strategy.setSubtext("ab") }

        strategy.setSubtext("E")
        assertEquals(1, strategy.indexOf("text"))
        assertEquals(1, strategy.indexOf("TEXT"))
        assertEquals(-1, strategy.indexOf("alpha"))

        val characterMap = CharArray('é'.code + 1) { index -> index.toChar() }
        characterMap['é'.code] = 'e'
        strategy.setCharacterMap(characterMap)
        assertEquals(1, strategy.indexOf("résumé"))
    }

    @Test
    fun regularExpressionStrategyKeepsFullMatchAndFailureBehavior() {
        val strategy = RegularExpressionTextSearchStrategy()

        assertThrows(NullPointerException::class.java) { strategy.indexOf("news") }
        assertThrows(PatternSyntaxException::class.java) { strategy.setSubtext("[") }

        strategy.setSubtext("n.*s")
        assertEquals(0, strategy.indexOf("news"))
        assertEquals(-1, strategy.indexOf("evening news"))
        assertEquals(-1, strategy.indexOf("weather"))
    }
}
