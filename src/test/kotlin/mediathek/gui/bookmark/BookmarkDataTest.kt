package mediathek.gui.bookmark

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.beans.PropertyChangeListener

internal class BookmarkDataTest {
    @Test
    fun propertyChangeListenerCanBeDetached() {
        val bookmark = BookmarkData()
        var notifications = 0
        val listener = PropertyChangeListener { notifications++ }

        bookmark.addPropertyChangeListener(listener)
        bookmark.note = "attached"
        bookmark.removePropertyChangeListener(listener)
        bookmark.note = "detached"

        assertEquals(1, notifications)
    }
}
