package mediathek.gui.bookmark

import mediathek.tool.JsonStringUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.lang.reflect.Method
import java.util.regex.Pattern
import javax.swing.JTable

internal class BookmarkTableColumnSettingsManagerTest {
    private val legacyIdPattern = Pattern.compile("\"id\"\\s*:\\s*\"((?:\\\\.|[^\"])*)\"")
    private val manager = BookmarkTableColumnSettingsManager<Any>(JTable(0, 1), "test", null)
    private val extractMethod: Method = manager.javaClass.getDeclaredMethod(
        "extractJsonStringValue",
        String::class.java,
        String::class.java
    ).apply { isAccessible = true }

    @Test
    fun iterativeIdExtractionMatchesLegacyRegex() {
        val validIds = listOf(
            "",
            "simple",
            "mit leerzeichen",
            "quote \" inside",
            """backslash \ path""",
            "slash / value",
            "line\nbreak",
            "tab\tvalue",
            "umlaut äöü",
            "\u0001control"
        )

        validIds.forEach { id ->
            assertMatchesLegacyRegex(
                """
                    {
                      "width": 120,
                      "id": "${JsonStringUtils.escapeJsonString(id)}",
                      "position": 3,
                      "visible": true
                    }
                """.trimIndent()
            )
        }

        listOf(
            """{"width":120,"position":3,"visible":true}""",
            """{"id":123,"position":3,"visible":true}""",
            """{"id":"unterminated,\"position":3,"visible":true}"""
        ).forEach(::assertMatchesLegacyRegex)
    }

    private fun assertMatchesLegacyRegex(objectJson: String) {
        assertEquals(extractWithLegacyRegex(objectJson), extractWithIterativeParser(objectJson))
    }

    private fun extractWithIterativeParser(objectJson: String): String? =
        extractMethod.invoke(manager, objectJson, "id") as String?

    private fun extractWithLegacyRegex(objectJson: String): String? {
        val matcher = legacyIdPattern.matcher(objectJson)
        if (!matcher.find()) {
            return null
        }
        return JsonStringUtils.unescapeJsonString(matcher.group(1))
    }
}
