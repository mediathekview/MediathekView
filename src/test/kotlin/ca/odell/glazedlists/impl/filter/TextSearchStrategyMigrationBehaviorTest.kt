package ca.odell.glazedlists.impl.filter

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class TextSearchStrategyMigrationBehaviorTest {
    @Test
    fun nestedFactoryRemainsAJavaFunctionalInterface() {
        val factory = TextSearchStrategy.Factory { _, _ -> MappingStrategy() }

        assertInstanceOf(MappingStrategy::class.java, factory.create(0, "filter"))
        assertTrue(TextSearchStrategy.Factory::class.java.isAnnotationPresent(FunctionalInterface::class.java))
    }

    @Test
    fun characterMappingKeepsRawMappedResetAndOutOfRangeBehavior() {
        val strategy = MappingStrategy()
        assertEquals('é', strategy.mapForTest('é'))

        val characterMap = CharArray('é'.code + 1) { index -> index.toChar() }
        characterMap['é'.code] = 'e'
        strategy.setCharacterMap(characterMap)
        assertEquals('e', strategy.mapForTest('é'))
        assertEquals('x', strategy.mapForTest('x'))
        assertEquals('漢', strategy.mapForTest('漢'))

        characterMap['é'.code] = 'a'
        assertEquals('a', strategy.mapForTest('é'))

        strategy.setCharacterMap(null)
        assertEquals('é', strategy.mapForTest('é'))
    }

    private class MappingStrategy : AbstractTextSearchStrategy() {
        override fun setSubtext(subtext: String) = Unit

        override fun indexOf(text: String): Int = -1

        fun mapForTest(character: Char): Char = map(character)
    }
}
