package mediathek.gui.tabs.tab_online_search

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import javax.swing.JMenu
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
                "Webseite im Browser öffnen",
                "Website-URL kopieren",
                "Film-URL kopieren",
            ),
            items.map { it.text },
        )
        assertTrue(items.all { !it.isEnabled })
        val filmUrlMenu = items.filterIsInstance<JMenu>().single()
        val qualityItems = filmUrlMenu.menuComponents.filterIsInstance<JMenuItem>()
        assertEquals(
            listOf("höchste/hohe Qualität", "mittlere Qualität", "niedrige Qualität"),
            qualityItems.map { it.text },
        )
        assertTrue(qualityItems.all { !it.isEnabled })
    }

    private class StubHost : OnlineSearchHost {
        override fun updateCurrentResult(result: OnlineSearchResult?) = Unit
        override fun showFilmInfo(result: OnlineSearchResult) = Unit
        override fun startDownload(results: List<OnlineSearchResult>) = Unit
        override fun playResult(result: OnlineSearchResult) = Unit
    }
}
