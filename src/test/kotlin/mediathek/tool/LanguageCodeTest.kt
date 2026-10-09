package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Test
import java.util.*

internal class LanguageCodeTest {

    @Test
    fun testConversion() {
        for (code in EnumSet.allOf(LanguageCode::class.java)) {
            val output = code.getISO3Language()
            assertFalse(output.isEmpty())
        }
    }

    @Test
    fun testSelectedIso3Mappings() {
        assertEquals("deu", LanguageCode.de.getISO3Language())
        assertEquals("eng", LanguageCode.en.getISO3Language())
        assertEquals("fra", LanguageCode.fr.getISO3Language())
        assertEquals("ita", LanguageCode.it.getISO3Language())
        assertEquals("spa", LanguageCode.es.getISO3Language())
    }
}
