package mediathek.gui.bookmark

import mediathek.config.application.ApplicationConfiguration
import mediathek.tool.JsonStringUtils
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.lang.reflect.Method
import java.util.regex.Pattern
import javax.swing.JTable

internal class BookmarkTableColumnSettingsManagerTest {
    private val legacyIdPattern = Pattern.compile("\"id\"\\s*:\\s*\"((?:\\\\.|[^\"])*)\"")
    private val manager = BookmarkTableColumnSettingsManager<Any>(JTable(0, 1), "test", null)
    private val extractMethod: Method = manager.javaClass.getDeclaredMethod(
        "extractId",
        String::class.java,
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

    @Test
    fun allHiddenPersistedColumnsAreRecovered() {
        val config = ApplicationConfiguration.getInstance()
        val prefix = "test-bookmark-all-hidden"
        val originalSettings = config.getTableColumnSettings(prefix)
        val table = JTable(0, 2)
        val settings = table.columnModel.columns.asIterator().asSequence()
            .mapIndexed { index, column ->
                """{"id":"${column.identifier}","position":$index,"width":75,"visible":false}"""
            }
            .joinToString(prefix = "[", postfix = "]")

        try {
            config.setTableColumnSettings(prefix, settings)

            BookmarkTableColumnSettingsManager<Any>(table, prefix, null).load()

            assertEquals(2, table.columnCount)
            assertTrue(config.getTableColumnSettings(prefix).contains("\"visible\":true"))
        } finally {
            config.setTableColumnSettings(prefix, originalSettings)
        }
    }

    private fun assertMatchesLegacyRegex(objectJson: String) {
        assertEquals(extractWithLegacyRegex(objectJson), extractWithIterativeParser(objectJson))
    }

    private fun extractWithIterativeParser(objectJson: String): String? =
        extractMethod.invoke(manager, objectJson) as String?

    private fun extractWithLegacyRegex(objectJson: String): String? {
        val matcher = legacyIdPattern.matcher(objectJson)
        if (!matcher.find()) {
            return null
        }
        return JsonStringUtils.unescapeJsonString(matcher.group(1))
    }
}
