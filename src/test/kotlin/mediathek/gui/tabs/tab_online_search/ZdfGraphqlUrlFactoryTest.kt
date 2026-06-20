package mediathek.gui.tabs.tab_online_search

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.net.URI
import java.net.URLDecoder
import java.nio.charset.StandardCharsets

class ZdfGraphqlUrlFactoryTest {
    @Test
    fun `escapes JSON special characters in query variables`() {
        val url = ZdfGraphqlUrlFactory.build("heute \"journal\" \\ test", cursor = null)
        val variables = URI(url).queryParameter("variables")!!
        val parsed = Json.parseToJsonElement(variables).jsonObject

        assertEquals("heute \"journal\" \\ test", parsed["query"]!!.jsonPrimitive.content)
    }

    @Test
    fun `builds explicit search query instead of stale persisted query`() {
        val url = ZdfGraphqlUrlFactory.build("1,2 oder 3", cursor = null)
        val uri = URI(url)
        val query = uri.queryParameter("query")

        assertEquals("getSearchResults", uri.queryParameter("operationName"))
        assertNotNull(query)
        assertTrue(query!!.contains("searchDocuments"))
        assertNull(uri.queryParameter("extensions"))
    }

    private fun URI.queryParameter(name: String): String? {
        val rawValue = rawQuery
            .split('&')
            .singleOrNull { it.startsWith("$name=") }
            ?: return null
        return rawValue
            .substringAfter('=')
            .let { URLDecoder.decode(it, StandardCharsets.UTF_8) }
    }
}
