package mediathek.gui.watchlist

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class WatchlistBellButtonTest {
    @Test
    fun `button starts without badge and reports the idle state`() {
        val button = WatchlistBellButton { }

        assertNotNull(button.icon)
        assertEquals("Watchlist: keine neuen Folgen", button.toolTipText)
    }

    @Test
    fun `unseen notifications are announced in the tooltip`() {
        val button = WatchlistBellButton { }

        button.setNotificationState(hasUnseen = true, pendingCount = 3)

        assertEquals("Neue Folgen auf der Watchlist eingetroffen", button.toolTipText)
    }

    @Test
    fun `acknowledged notifications are not called unread`() {
        val button = WatchlistBellButton { }

        button.setNotificationState(hasUnseen = false, pendingCount = 2)

        assertEquals("Watchlist: 2 gespeicherte Benachrichtigungen", button.toolTipText)
    }

    @Test
    fun `clicking the bell triggers the action`() {
        var clicks = 0
        val button = WatchlistBellButton { clicks++ }

        button.doClick()

        assertEquals(1, clicks)
    }
}
