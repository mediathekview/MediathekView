package mediathek.gui.tabs.tab_online_search

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import javax.swing.JMenuItem

class OnlineSearchContextMenuTest {
    @Test
    fun `menu shows disabled no-selection item when no result is selected`() {
        val menu = OnlineSearchContextMenu(null, emptyList(), StubHost())

        val items = menu.components.filterIsInstance<JMenuItem>()
        assertEquals(
            listOf(
                "Abspielen",
                "Filminformation anzeigen",
                "Download anlegen",
                "Website öffnen",
                "Website kopieren",
                "URL kopieren",
                "HD-URL kopieren",
                "Kleine URL kopieren",
            ),
            items.map { it.text },
        )
        assertTrue(items.all { !it.isEnabled })
    }

    private class StubHost : OnlineSearchHost {
        override fun updateCurrentResult(result: OnlineSearchResult?) = Unit
        override fun showFilmInfo(result: OnlineSearchResult) = Unit
        override fun startDownload(results: List<OnlineSearchResult>) = Unit
        override fun playResult(result: OnlineSearchResult) = Unit
    }
}
