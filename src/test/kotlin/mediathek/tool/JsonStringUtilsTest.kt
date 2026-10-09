package mediathek.tool

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertNull
import org.junit.jupiter.api.Test

internal class JsonStringUtilsTest {
    @Test
    fun parseQuotedJsonStringParsesEscapedContentAndTracksEndIndex() {
        val prefix = """{"id":"""
        val escaped = JsonStringUtils.escapeJsonString("quote \" and slash \\ and tab\t")
        val json = prefix + "\"$escaped\",\"visible\":true}"

        val parsed = JsonStringUtils.parseQuotedJsonString(json, prefix.length)

        assertEquals("quote \" and slash \\ and tab\t", parsed?.value)
        assertEquals(prefix.length + escaped.length + 1, parsed?.endIndex)
    }

    @Test
    fun parseQuotedJsonStringReturnsNullForNonStringsAndUnterminatedStrings() {
        assertNull(JsonStringUtils.parseQuotedJsonString("""{"id":123}""", 6))
        assertNull(JsonStringUtils.parseQuotedJsonString(""""unterminated""", 0))
    }
}
