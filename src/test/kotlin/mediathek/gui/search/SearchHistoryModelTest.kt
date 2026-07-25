package mediathek.gui.search

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test

internal class SearchHistoryModelTest {
    @Test
    fun `moving an existing entry persists one aggregate change`() {
        val savedSnapshots = mutableListOf<List<String>>()
        val history = SearchHistoryModel(listOf("first", "second"), savedSnapshots::add)
        var publishedEvents = 0
        history.entries.addListEventListener { publishedEvents++ }

        history.addMostRecent("second")

        assertEquals(listOf("second", "first"), history.entries.toList())
        assertEquals(1, publishedEvents)
        assertEquals(listOf(listOf("second", "first")), savedSnapshots)
    }

    @Test
    fun `initialization does not persist and clear persists once`() {
        val savedSnapshots = mutableListOf<List<String>>()
        val history = SearchHistoryModel(listOf("first", "second"), savedSnapshots::add)

        assertTrue(savedSnapshots.isEmpty())

        history.clear()

        assertEquals(listOf(emptyList<String>()), savedSnapshots)
    }
}
