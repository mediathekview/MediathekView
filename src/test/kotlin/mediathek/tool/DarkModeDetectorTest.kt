package mediathek.tool

import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.ValueSource

internal class DarkModeDetectorTest {
    @ParameterizedTest
    @ValueSource(
        strings = [
            "1",
            "uint32 1",
            "variant uint32 1",
            "variant\n  uint32 1",
        ],
    )
    fun parseKdePortalColorSchemeTreatsOneAsDark(result: String) {
        assertTrue(DarkModeDetector.parseKdePortalColorScheme(result))
    }

    @ParameterizedTest
    @ValueSource(
        strings = [
            "0",
            "2",
            "uint32 0",
            "uint32 2",
            "",
            "unexpected",
        ],
    )
    fun parseKdePortalColorSchemeRejectsNonDarkValues(result: String) {
        assertFalse(DarkModeDetector.parseKdePortalColorScheme(result))
    }
}
