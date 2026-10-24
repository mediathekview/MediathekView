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

        history.addEntry("second", useLru = true)

        assertEquals(listOf("second", "first"), history.entries.toList())
        assertEquals(1, publishedEvents)
        assertEquals(listOf(listOf("second", "first")), savedSnapshots)
    }

    @Test
    fun `default mode preserves manually arranged entries without publishing duplicates`() {
        val savedSnapshots = mutableListOf<List<String>>()
        val history = SearchHistoryModel(listOf("first", "second", "third"), savedSnapshots::add)
        history.entries.add(0, history.entries.removeAt(2))
        savedSnapshots.clear()
        var publishedEvents = 0
        history.entries.addListEventListener { publishedEvents++ }

        history.addEntry("second")

        assertEquals(listOf("third", "first", "second"), history.entries.toList())
        assertTrue(savedSnapshots.isEmpty())
        assertEquals(0, publishedEvents)
    }

    @Test
    fun `new entries are added once at the top in either mode`() {
        for (useLru in listOf(false, true)) {
            val savedSnapshots = mutableListOf<List<String>>()
            val history = SearchHistoryModel(listOf("first", "second"), savedSnapshots::add)

            history.addEntry("new", useLru)

            assertEquals(listOf("new", "first", "second"), history.entries.toList())
            assertEquals(listOf(listOf("new", "first", "second")), savedSnapshots)
        }
    }

    @Test
    fun `mode can change without rebuilding the history`() {
        val history = SearchHistoryModel(listOf("first", "second")) {}

        history.addEntry("second", useLru = false)
        assertEquals(listOf("first", "second"), history.entries.toList())
        history.addEntry("second", useLru = true)
        assertEquals(listOf("second", "first"), history.entries.toList())
        history.addEntry("first", useLru = false)
        assertEquals(listOf("second", "first"), history.entries.toList())
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
