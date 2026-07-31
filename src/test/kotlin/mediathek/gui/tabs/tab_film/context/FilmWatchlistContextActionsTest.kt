package mediathek.gui.tabs.tab_film.context

import kotlinx.coroutines.runBlocking
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.daten.watchlist.WatchlistDatabaseStorage
import mediathek.daten.watchlist.WatchlistServices
import mediathek.tool.notification.NotificationPublisher
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import java.nio.file.Path
import java.util.*
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

internal class FilmWatchlistContextActionsTest {
    @TempDir
    lateinit var tempDir: Path

    private lateinit var allFilms: ListeFilme
    private lateinit var watchlist: WatchlistServices
    private lateinit var contextActions: FilmWatchlistContextActions

    @BeforeEach
    fun setUp() {
        allFilms = ListeFilme()
        watchlist = WatchlistServices(
            allFilms,
            NotificationPublisher { },
            tempDir.resolve("watchlist.db"),
            WatchlistDatabaseStorage,
        )
        contextActions = FilmWatchlistContextActions(watchlist)
    }

    @AfterEach
    fun tearDown() {
        watchlist.close()
    }

    private fun film(title: String = "Tagesschau 20:00 Uhr"): DatenFilm =
        DatenFilm().apply {
            sender = "ARD"
            thema = "Tagesschau"
            this.title = title
            urlNormalQuality = "https://example.org/film.mp4"
        }

    private fun watchlistItems(selectedFilm: Optional<DatenFilm>): List<JMenuItem> {
        val popupMenu = JPopupMenu()
        contextActions.addWatchlistMenu(popupMenu, selectedFilm)
        val submenu = popupMenu.subElements
            .mapNotNull { element -> element.component as? JMenu }
            .single { menu -> menu.text == "Watchlist" }
        return submenu.menuComponents.filterIsInstance<JMenuItem>()
    }

    @Test
    fun `offers adding the show with and without title`() {
        val items = watchlistItems(Optional.of(film()))

        assertEquals(
            listOf("Sendung auf Watchlist setzen", "Sendung mit Titel auf Watchlist setzen"),
            items.map(JMenuItem::getText),
        )
        assertTrue(items.all(JMenuItem::isEnabled))
    }

    @Test
    fun `items are disabled without a selected film`() {
        val items = watchlistItems(Optional.empty())

        assertTrue(items.none(JMenuItem::isEnabled))
    }

    @Test
    fun `adding uses the film the menu was built for`() = runBlocking {
        val menuFilm = film()
        allFilms.add(menuFilm)
        val items = watchlistItems(Optional.of(menuFilm))

        // A table update between opening and clicking must not change the target.
        allFilms.clear()
        allFilms.add(
            DatenFilm().apply {
                sender = "ZDF"
                thema = "heute"
                title = "heute 19:00 Uhr"
                urlNormalQuality = "https://example.org/other.mp4"
            }
        )
        items.first().doClick()
        watchlist.awaitIdle()

        val entry = watchlist.entriesSnapshot().single()
        assertEquals("ARD", entry.sender)
        assertEquals("Tagesschau", entry.thema)
        assertEquals("", entry.title)
    }

    @Test
    fun `existing entry switches the item to removal`() = runBlocking {
        val selectedFilm = film()
        allFilms.add(selectedFilm)
        watchlist.addEntryFromFilm(selectedFilm, withTitle = false).join()

        val items = watchlistItems(Optional.of(selectedFilm))
        assertEquals(
            listOf("Sendung von Watchlist entfernen", "Sendung mit Titel auf Watchlist setzen"),
            items.map(JMenuItem::getText),
        )

        items.first().doClick()
        watchlist.awaitIdle()

        assertTrue(watchlist.entriesSnapshot().isEmpty())
    }
}
