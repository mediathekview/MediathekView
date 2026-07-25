package mediathek.gui.tabs.tab_online_search

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.TransactionList
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertThrows
import org.junit.jupiter.api.Test

class OnlineSearchTransactionTest {
    @Test
    fun `result update publishes one aggregate event`() {
        val results = TransactionList<OnlineSearchResult>(BasicEventList())
        var eventCount = 0
        results.addListEventListener { eventCount++ }

        results.updateResults {
            add(result("first"))
            add(result("second"))
        }

        assertEquals(listOf("first", "second"), results.map(OnlineSearchResult::title))
        assertEquals(1, eventCount)
    }

    @Test
    fun `result update rolls back on failure`() {
        val original = result("original")
        val results = TransactionList<OnlineSearchResult>(BasicEventList()).apply { add(original) }
        var eventCount = 0
        results.addListEventListener { eventCount++ }

        assertThrows(IllegalArgumentException::class.java) {
            results.updateResults {
                clear()
                add(result("partial"))
                throw IllegalArgumentException("abort")
            }
        }

        assertEquals(listOf(original), results)
        assertEquals(0, eventCount)
    }

    private fun result(title: String) = OnlineSearchResult(
        provider = OnlineSearchProvider.ARD,
        sender = "ARD",
        topic = "Topic",
        title = title,
        normalQualityUrl = "https://example.invalid/$title.mp4",
    )
}
