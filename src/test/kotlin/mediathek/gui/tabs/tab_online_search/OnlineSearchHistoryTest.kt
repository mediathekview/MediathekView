package mediathek.gui.tabs.tab_online_search

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class OnlineSearchHistoryTest {
    @Test
    fun `adds newest entry first and limits size`() {
        val history = OnlineSearchHistory.decode("[\"old\"]")
            .withEntry("new", maxSize = 2)
            .withEntry("third", maxSize = 2)

        assertEquals(listOf("third", "new"), history.entries)
    }

    @Test
    fun `deduplicates case insensitively`() {
        val history = OnlineSearchHistory.decode("[\"Tatort\",\"News\"]")
            .withEntry("tatort", maxSize = 5)

        assertEquals(listOf("tatort", "News"), history.entries)
    }
}
