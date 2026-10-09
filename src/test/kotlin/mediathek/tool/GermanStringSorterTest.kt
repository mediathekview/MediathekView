package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.MethodSource
import java.text.Collator
import java.util.*
import java.util.stream.Stream

class GermanStringSorterTest {

    @ParameterizedTest
    @MethodSource("comparisonCases")
    fun `matches legacy Java collator comparison`(left: String, right: String) {
        assertEquals(legacyJavaCompare(left, right), GermanStringSorter.compare(left, right))
    }

    @Test
    fun `sorts like legacy Java collator`() {
        val values = listOf("ZDF", "ARD", "arte", "Ärger", "Österreich", "3sat", "ß", "ss", "ä", "a")

        assertEquals(
            values.sortedWith(::legacyJavaCompare),
            values.sortedWith(GermanStringSorter),
        )
    }

    private companion object {
        private val legacyJavaCollator: Collator = Collator.getInstance(Locale.GERMANY).apply {
            strength = Collator.SECONDARY
        }

        @JvmStatic
        fun comparisonCases(): Stream<Arguments> =
            Stream.of(
                Arguments.of("ARD", "ZDF"),
                Arguments.of("arte", "ARTE"),
                Arguments.of("ä", "a"),
                Arguments.of("Ärger", "Aachen"),
                Arguments.of("Österreich", "ZDF"),
                Arguments.of("ß", "ss"),
                Arguments.of("3sat", "ARD"),
            )

        private fun legacyJavaCompare(left: String, right: String): Int =
            legacyJavaCollator.compare(left, right)
    }
}
