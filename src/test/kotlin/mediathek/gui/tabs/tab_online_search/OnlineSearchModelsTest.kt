package mediathek.gui.tabs.tab_online_search

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class OnlineSearchModelsTest {
    @Test
    fun `empty page has no next page`() {
        val page = OnlineSearchPage(results = emptyList(), nextToken = null, totalResults = 0)

        assertFalse(page.hasNextPage)
    }

    @Test
    fun `page with nonblank token has next page`() {
        val page = OnlineSearchPage(results = emptyList(), nextToken = "MjQ=", totalResults = 50)

        assertTrue(page.hasNextPage)
    }

    @Test
    fun `display topic falls back to title when topic is blank`() {
        val result = OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = "",
            title = "Tagesschau",
            normalQualityUrl = "https://cdn.example/video.mp4",
        )

        assertEquals("Tagesschau", result.displayTopic)
    }
}
