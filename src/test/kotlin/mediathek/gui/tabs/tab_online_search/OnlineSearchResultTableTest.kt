package mediathek.gui.tabs.tab_online_search

import ca.odell.glazedlists.BasicEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import javax.swing.SwingUtilities

class OnlineSearchResultTableTest {
    @Test
    fun `selectedResult returns model object for selected view row`() {
        val result = OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = "Tatort",
            title = "Test",
            normalQualityUrl = "https://cdn.example/test.mp4",
        )

        withTable { source, table ->
            source.add(result)
            assertEquals(1, table.rowCount)
            table.setRowSelectionInterval(0, 0)
            assertSame(result, table.selectedResult())
        }
    }

    @Test
    fun `selectedResult tolerates stale selection after rows are cleared`() {
        val result = OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = "Tatort",
            title = "Test",
            normalQualityUrl = "https://cdn.example/test.mp4",
        )

        withTable { source, table ->
            source.add(result)
            table.setRowSelectionInterval(0, 0)
            source.clear()

            assertNull(table.selectedResult())
            assertEquals(emptyList<OnlineSearchResult>(), table.selectedResults())
        }
    }

    @Test
    fun `column state saves and restores widths and positions`() {
        val store = InMemoryOnlineSearchTableStateStore()
        withTable(store) { _, firstTable ->
            firstTable.columnModel.getColumn(OnlineSearchResultTableFormat.SENDER).preferredWidth = 123
            firstTable.columnModel.getColumn(OnlineSearchResultTableFormat.SENDER).width = 123
            firstTable.columnModel.moveColumn(
                OnlineSearchResultTableFormat.SENDER,
                OnlineSearchResultTableFormat.TITLE,
            )
            firstTable.saveColumnState()

            withTable(store) { _, restoredTable ->
                assertEquals(OnlineSearchResultTableFormat.TOPIC, restoredTable.columnModel.getColumn(0).modelIndex)
                assertEquals(OnlineSearchResultTableFormat.TITLE, restoredTable.columnModel.getColumn(1).modelIndex)
                assertEquals(OnlineSearchResultTableFormat.SENDER, restoredTable.columnModel.getColumn(2).modelIndex)
                assertEquals(123, restoredTable.columnModel.getColumn(2).preferredWidth)
            }
        }
    }

    @Test
    fun `dispose detaches the table pipeline from its source`() {
        withTable { source, table ->
            table.dispose()
            source.add(
                OnlineSearchResult(
                    provider = OnlineSearchProvider.ARD,
                    sender = "ARD",
                    topic = "Topic",
                    title = "After disposal",
                    normalQualityUrl = "https://example.invalid/disposed.mp4",
                ),
            )

            assertEquals(0, table.rowCount)
        }
    }

    private fun withTable(
        stateStore: OnlineSearchTableStateStore = InMemoryOnlineSearchTableStateStore(),
        action: (BasicEventList<OnlineSearchResult>, OnlineSearchResultTable) -> Unit,
    ) {
        BasicEventList<OnlineSearchResult>().use { source ->
            onEdt {
                val table = OnlineSearchResultTable(source, stateStore)
                try {
                    action(source, table)
                } finally {
                    table.dispose()
                }
            }
        }
    }

    private fun onEdt(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) {
            action()
        } else {
            SwingUtilities.invokeAndWait(action)
        }
    }
}

private class InMemoryOnlineSearchTableStateStore : OnlineSearchTableStateStore {
    private var state = ""

    override fun read(): String = state

    override fun write(state: String) {
        this.state = state
    }
}
