package mediathek.gui.watchlist

import mediathek.daten.watchlist.WatchlistNotification
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.awt.Insets
import java.awt.Rectangle
import javax.swing.JLabel
import javax.swing.JMenuItem

internal class WatchlistNotificationPanelTest {
    private fun notification(title: String = "Tagesschau 20:00 Uhr", filmId: String = "film-1") =
        WatchlistNotification(
            entryId = "entry-1",
            entryName = "Tagesschau",
            filmId = filmId,
            sender = "ARD",
            thema = "Tagesschau",
            title = title,
            sendeDatum = "25.07.2026",
            urlNormalQuality = "https://example.org/$filmId.mp4",
        )

    @Test
    fun `row offers a context menu with all watchlist actions`() {
        val panel = WatchlistNotificationPanel()
        val notification = notification()

        panel.setNotifications(listOf(notification))

        val popupMenu = checkNotNull(panel.rowPanelFor(notification)?.componentPopupMenu)
        val labels = popupMenu.subElements
            .mapNotNull { element -> (element.component as? JMenuItem)?.text }
        assertEquals(
            listOf("In Filmliste anzeigen", "Film aufzeichnen...", "Sendung von Watchlist entfernen"),
            labels,
        )
    }

    @Test
    fun `row text inherits the context menu so right clicking the labels works`() {
        val panel = WatchlistNotificationPanel()
        val notification = notification()

        panel.setNotifications(listOf(notification))

        val row = checkNotNull(panel.rowPanelFor(notification))
        val labels = row.components.filterIsInstance<JLabel>()
        assertTrue(labels.isNotEmpty())
        assertTrue(labels.all { label -> label.inheritsPopupMenu }, "labels must inherit the row popup menu")
        assertTrue(row.removeButton.inheritsPopupMenu)
    }

    @Test
    fun `remove button is an accessible button that reports the notification`() {
        val panel = WatchlistNotificationPanel()
        val notification = notification()
        val removed = mutableListOf<WatchlistNotification>()
        panel.addRemoveNotificationListener(removed::add)

        panel.setNotifications(listOf(notification))
        val removeButton = checkNotNull(panel.rowPanelFor(notification)).removeButton

        assertTrue(removeButton.isFocusable, "keyboard users must be able to reach the remove button")
        removeButton.doClick()

        assertEquals(listOf(notification), removed)
    }

    @Test
    fun `context menu actions report the row notification`() {
        val panel = WatchlistNotificationPanel()
        val notification = notification()
        val shown = mutableListOf<WatchlistNotification>()
        val recorded = mutableListOf<WatchlistNotification>()
        val removedEntries = mutableListOf<WatchlistNotification>()
        panel.addShowInFilmTableListener(shown::add)
        panel.addRecordFilmListener(recorded::add)
        panel.addRemoveEntryListener(removedEntries::add)

        panel.setNotifications(listOf(notification))
        val popupMenu = checkNotNull(panel.rowPanelFor(notification)?.componentPopupMenu)
        popupMenu.subElements
            .mapNotNull { element -> element.component as? JMenuItem }
            .forEach(JMenuItem::doClick)

        assertEquals(listOf(notification), shown)
        assertEquals(listOf(notification), recorded)
        assertEquals(listOf(notification), removedEntries)
    }

    @Test
    fun `empty list shows a hint and notifies only after rows disappeared`() {
        val panel = WatchlistNotificationPanel()
        var emptyCallbacks = 0
        panel.addEmptyListener { emptyCallbacks++ }

        panel.setNotifications(emptyList())
        assertEquals(0, emptyCallbacks, "opening an empty panel must not close the popup")

        panel.setNotifications(listOf(notification()))
        panel.setNotifications(emptyList())

        assertEquals(1, emptyCallbacks)
        assertTrue(hintLabels(panel).any { text -> text == "Keine neuen Folgen" })
    }

    @Test
    fun `popup is shifted left to retain its width and screen margin`() {
        val panel = WatchlistNotificationPanel()

        val bounds = panel.calculatePopupBounds(
            ownerBounds = Rectangle(1880, 100, 30, 30),
            screenBounds = Rectangle(0, 0, 1920, 1080),
            screenInsets = Insets(0, 0, 0, 0),
        )

        assertEquals(Rectangle(1300, 130, 610, 320), bounds)
    }

    @Test
    fun `popup opens above the owner when more space is available there`() {
        val panel = WatchlistNotificationPanel()

        val bounds = panel.calculatePopupBounds(
            ownerBounds = Rectangle(100, 900, 30, 30),
            screenBounds = Rectangle(0, 0, 1920, 1080),
            screenInsets = Insets(0, 0, 0, 0),
        )

        assertEquals(Rectangle(100, 580, 610, 320), bounds)
    }

    @Test
    fun `popup size is recalculated from its target size for every screen`() {
        val panel = WatchlistNotificationPanel()
        val ownerBounds = Rectangle(100, 100, 30, 30)

        val constrained = panel.calculatePopupBounds(
            ownerBounds,
            Rectangle(0, 0, 400, 300),
            Insets(0, 0, 0, 0),
        )
        val unconstrained = panel.calculatePopupBounds(
            ownerBounds,
            Rectangle(0, 0, 1920, 1080),
            Insets(0, 0, 0, 0),
        )

        assertEquals(380, constrained.width)
        assertEquals(160, constrained.height)
        assertEquals(610, unconstrained.width)
        assertEquals(320, unconstrained.height)
    }

    private fun hintLabels(panel: WatchlistNotificationPanel): List<String> =
        collectLabels(panel).map(JLabel::getText)

    private fun collectLabels(component: java.awt.Container): List<JLabel> =
        component.components.flatMap { child ->
            when (child) {
                is JLabel -> listOf(child)
                is java.awt.Container -> collectLabels(child)
                else -> emptyList()
            }
        }
}
