package mediathek.gui.tabs.tab_online_search

import kotlinx.serialization.json.Json
import kotlinx.serialization.json.jsonObject
import kotlinx.serialization.json.jsonPrimitive
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.net.URI
import java.net.URLDecoder
import java.nio.charset.StandardCharsets

class ZdfGraphqlUrlFactoryTest {
    @Test
    fun `escapes JSON special characters in query variables`() {
        val url = ZdfGraphqlUrlFactory.build("heute \"journal\" \\ test", cursor = null)
        val variables = URI(url).rawQuery
            .split('&')
            .single { it.startsWith("variables=") }
            .substringAfter('=')
            .let { URLDecoder.decode(it, StandardCharsets.UTF_8) }
        val parsed = Json.parseToJsonElement(variables).jsonObject

        assertEquals("heute \"journal\" \\ test", parsed["query"]!!.jsonPrimitive.content)
    }
}
