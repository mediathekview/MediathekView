package mediathek.gui.tabs.tab_online_search

import ca.odell.glazedlists.BasicEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class OnlineSearchResultTableTest {
    @Test
    fun `selectedResult returns model object for selected view row`() {
        val source = BasicEventList<OnlineSearchResult>()
        val table = OnlineSearchResultTable(source)
        val result = OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = "Tatort",
            title = "Test",
            normalQualityUrl = "https://cdn.example/test.mp4",
        )

        javax.swing.SwingUtilities.invokeAndWait {
            source.add(result)
        }
        javax.swing.SwingUtilities.invokeAndWait {
            table.setRowSelectionInterval(0, 0)
        }

        assertSame(result, table.selectedResult())
    }

    @Test
    fun `selectedResult tolerates stale selection after rows are cleared`() {
        val source = BasicEventList<OnlineSearchResult>()
        val table = OnlineSearchResultTable(source)
        val result = OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = "Tatort",
            title = "Test",
            normalQualityUrl = "https://cdn.example/test.mp4",
        )

        javax.swing.SwingUtilities.invokeAndWait {
            source.add(result)
            table.setRowSelectionInterval(0, 0)
            source.clear()
        }

        assertNull(table.selectedResult())
        assertEquals(emptyList<OnlineSearchResult>(), table.selectedResults())
    }

    @Test
    fun `column state saves and restores widths and positions`() {
        val store = InMemoryOnlineSearchTableStateStore()
        val firstTable = OnlineSearchResultTable(BasicEventList(), store)

        javax.swing.SwingUtilities.invokeAndWait {
            firstTable.columnModel.getColumn(OnlineSearchResultTableFormat.SENDER).preferredWidth = 123
            firstTable.columnModel.getColumn(OnlineSearchResultTableFormat.SENDER).width = 123
            firstTable.columnModel.moveColumn(OnlineSearchResultTableFormat.SENDER, OnlineSearchResultTableFormat.TITLE)
            firstTable.saveColumnState()
        }

        val restoredTable = OnlineSearchResultTable(BasicEventList(), store)

        assertEquals(OnlineSearchResultTableFormat.TOPIC, restoredTable.columnModel.getColumn(0).modelIndex)
        assertEquals(OnlineSearchResultTableFormat.TITLE, restoredTable.columnModel.getColumn(1).modelIndex)
        assertEquals(OnlineSearchResultTableFormat.SENDER, restoredTable.columnModel.getColumn(2).modelIndex)
        assertEquals(123, restoredTable.columnModel.getColumn(2).preferredWidth)
    }
}

private class InMemoryOnlineSearchTableStateStore : OnlineSearchTableStateStore {
    private var state = ""

    override fun read(): String = state

    override fun write(state: String) {
        this.state = state
    }
}
