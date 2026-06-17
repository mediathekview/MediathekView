package mediathek.gui.tabs.tab_online_search

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.awaitCancellation
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertFalse
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.awt.Container
import java.awt.IllegalComponentStateException
import java.awt.event.MouseEvent
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.JLabel
import javax.swing.SwingUtilities

class OnlineSearchPanelTest {
    @Test
    fun `panel contains sender selector and empty GlazedLists table`() {
        val panel = OnlineSearchPanel(TestOnlineSearchHost())

        assertEquals(3, panel.senderComboBox.itemCount)
        assertEquals(OnlineSearchProvider.ARD, panel.senderComboBox.getItemAt(0))
        assertEquals(OnlineSearchProvider.ZDF, panel.senderComboBox.getItemAt(1))
        assertEquals(OnlineSearchProvider.ARTE, panel.senderComboBox.getItemAt(2))
        assertEquals(150, panel.senderComboBox.maximumSize.width)
        assertEquals(0, panel.table.rowCount)
    }

    @Test
    fun `sender label aligns with search field labels`() {
        val panel = OnlineSearchPanel(TestOnlineSearchHost())
        SwingUtilities.invokeAndWait {
            panel.setSize(500, 300)
            panel.doLayoutRecursively()
        }

        val senderLabel = panel.findLabel("Sender")
        val liveSearchLabel = panel.findLabel("Livesuche")

        assertEquals(liveSearchLabel.xIn(panel), senderLabel.xIn(panel))
    }

    @Test
    fun `cancel button cancels running search`() = runBlocking {
        val service = BlockingOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        searchPanel.queryText = "tatort"

        SwingUtilities.invokeAndWait { searchPanel.searchButton.doClick() }
        withTimeout(2_000) { service.started.await() }

        SwingUtilities.invokeAndWait { searchPanel.cancelButton.doClick() }

        withTimeout(2_000) { service.cancelled.await() }
    }

    @Test
    fun `cancelled stale search does not mark newer search idle`() = runBlocking {
        val service = DelayedCancellationOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        searchPanel.setQueryHistory(listOf("zweite suche"))
        searchPanel.queryText = "erste suche"

        SwingUtilities.invokeAndWait { searchPanel.searchButton.doClick() }
        withTimeout(2_000) { service.firstSearchStarted.await() }

        SwingUtilities.invokeAndWait { searchPanel.queryField.selectHistoryEntry("zweite suche") }
        withTimeout(2_000) { service.firstSearchCancellationStarted.await() }
        withTimeout(2_000) { service.secondSearchStarted.await() }

        service.allowFirstSearchCancellationToComplete.complete(Unit)
        SwingUtilities.invokeAndWait { }

        assertFalse(searchPanel.queryField.isEnabled)
        assertFalse(searchPanel.urlField.isEnabled)
        assertFalse(panel.senderComboBox.isEnabled)
        assertTrue(searchPanel.cancelButton.isEnabled)
        assertTrue(searchPanel.progressBar.isVisible)

        SwingUtilities.invokeAndWait { panel.removeNotify() }
    }

    @Test
    fun `clearing query field after text search clears table`() = runBlocking {
        val service = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        SwingUtilities.invokeAndWait {
            searchPanel.queryText = "tatort"
            searchPanel.searchButton.doClick()
        }
        waitForTableRows(panel, 1)

        SwingUtilities.invokeAndWait { searchPanel.queryText = "" }

        waitForTableRows(panel, 0)
    }

    @Test
    fun `clearing query field after multi-result text search clears all table rows and selection`() = runBlocking {
        val service = MultiResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        SwingUtilities.invokeAndWait {
            searchPanel.queryText = "tatort"
            searchPanel.searchButton.doClick()
        }
        waitForTableRows(panel, 2)
        SwingUtilities.invokeAndWait { panel.table.setRowSelectionInterval(0, 1) }

        SwingUtilities.invokeAndWait { searchPanel.queryText = "" }

        waitForTableRows(panel, 0)
        assertEquals(emptyList<Int>(), panel.table.selectedRows.toList())
    }

    @Test
    fun `selected sender chooses search service`() = runBlocking {
        val ardService = ResultOnlineSearchService()
        val zdfService = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = ardService,
            zdfService = zdfService,
        )
        val searchPanel = panel.searchPanel

        SwingUtilities.invokeAndWait {
            panel.senderComboBox.selectedItem = OnlineSearchProvider.ZDF
            searchPanel.queryText = "heute"
            searchPanel.searchButton.doClick()
        }

        waitForTableRows(panel, 1)
        assertEquals(null, ardService.lastSearchQuery)
        assertEquals("heute", zdfService.lastSearchQuery)
    }

    @Test
    fun `selected ARTE sender chooses ARTE search service`() = runBlocking {
        val ardService = ResultOnlineSearchService()
        val zdfService = ResultOnlineSearchService()
        val arteService = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = ardService,
            zdfService = zdfService,
            arteService = arteService,
        )

        SwingUtilities.invokeAndWait {
            panel.senderComboBox.selectedItem = OnlineSearchProvider.ARTE
            panel.searchPanel.queryText = "tracks"
            panel.searchPanel.searchButton.doClick()
        }

        waitForTableRows(panel, 1)
        assertEquals(null, ardService.lastSearchQuery)
        assertEquals(null, zdfService.lastSearchQuery)
        assertEquals("tracks", arteService.lastSearchQuery)
    }

    @Test
    fun `selected ARTE sender chooses ARTE url lookup service`() = runBlocking {
        val ardService = ResultOnlineSearchService()
        val zdfService = ResultOnlineSearchService()
        val arteService = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = ardService,
            zdfService = zdfService,
            arteService = arteService,
        )
        val url = "https://www.arte.tv/de/videos/118267-006-A/re-tatort-kirche-betroffene-klagen-an/"

        SwingUtilities.invokeAndWait {
            panel.senderComboBox.selectedItem = OnlineSearchProvider.ARTE
            panel.searchPanel.urlText = url
            panel.searchPanel.urlSearchButton.doClick()
        }

        waitForTableRows(panel, 1)
        assertEquals(null, ardService.lastUrl)
        assertEquals(null, zdfService.lastUrl)
        assertEquals(url, arteService.lastUrl)
    }

    @Test
    fun `text search loads all pages automatically`() = runBlocking {
        val service = PaginatedOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )

        SwingUtilities.invokeAndWait {
            panel.searchPanel.queryText = "tatort"
            panel.searchPanel.searchButton.doClick()
        }

        waitForTableRows(panel, 2)
        assertEquals(listOf(null, "page-2"), service.requestedTokens)
    }

    @Test
    fun `text search publishes first page while later page is still loading`() = runBlocking {
        val service = BlockingSecondPageOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )

        SwingUtilities.invokeAndWait {
            panel.searchPanel.queryText = "tatort"
            panel.searchPanel.searchButton.doClick()
        }
        withTimeout(2_000) { service.secondPageRequested.await() }
        SwingUtilities.invokeAndWait { }

        assertEquals(1, panel.table.rowCount)

        service.allowSecondPage.complete(Unit)
        waitForTableRows(panel, 2)
    }

    @Test
    fun `switching sender clears search fields`() {
        val panel = OnlineSearchPanel(TestOnlineSearchHost())
        val searchPanel = panel.searchPanel

        SwingUtilities.invokeAndWait {
            searchPanel.queryText = "tatort"
            searchPanel.urlText = "https://example.invalid/film"

            panel.senderComboBox.selectedItem = OnlineSearchProvider.ZDF
        }

        assertEquals("", searchPanel.queryText)
        assertEquals("", searchPanel.urlText)
    }

    @Test
    fun `starting text search clears url field`() = runBlocking {
        val service = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel

        SwingUtilities.invokeAndWait {
            searchPanel.queryText = "tatort"
            searchPanel.urlText = "https://example.invalid/film"
            searchPanel.searchButton.doClick()
        }

        waitForTableRows(panel, 1)
        assertEquals("", searchPanel.urlText)
        assertEquals("tatort", service.lastSearchQuery)
    }

    @Test
    fun `starting url search clears query field`() = runBlocking {
        val service = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        val url = "https://example.invalid/film"

        SwingUtilities.invokeAndWait {
            searchPanel.queryText = "tatort"
            searchPanel.urlText = url
            searchPanel.urlSearchButton.doClick()
        }

        waitForTableRows(panel, 1)
        assertEquals("", searchPanel.queryText)
        assertEquals(url, service.lastUrl)
    }

    @Test
    fun `clearing url field after url search clears table`() = runBlocking {
        val service = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        SwingUtilities.invokeAndWait {
            searchPanel.urlText = "https://example.invalid/film"
            searchPanel.urlSearchButton.doClick()
        }
        waitForTableRows(panel, 1)

        SwingUtilities.invokeAndWait { searchPanel.urlText = "" }

        waitForTableRows(panel, 0)
    }

    @Test
    fun `failed url search clears stale table results immediately`() = runBlocking {
        val service = FailingSecondUrlOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        SwingUtilities.invokeAndWait {
            searchPanel.urlText = "https://example.invalid/first"
            searchPanel.urlSearchButton.doClick()
        }
        waitForTableRows(panel, 1)

        SwingUtilities.invokeAndWait {
            searchPanel.urlSearchButton.doClick()
        }

        waitForTableRows(panel, 0)
    }

    @Test
    fun `selecting query history starts text search`() = runBlocking {
        val service = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        searchPanel.setQueryHistory(listOf("tatort"))

        SwingUtilities.invokeAndWait { searchPanel.queryField.selectHistoryEntry("tatort") }

        waitForTableRows(panel, 1)
        assertEquals("tatort", searchPanel.queryText)
        assertEquals("tatort", service.lastSearchQuery)
    }

    @Test
    fun `selecting url history starts url search`() = runBlocking {
        val service = ResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        val searchPanel = panel.searchPanel
        val url = "https://example.invalid/film"
        searchPanel.setUrlHistory(listOf(url))

        SwingUtilities.invokeAndWait { searchPanel.urlField.selectHistoryEntry(url) }

        waitForTableRows(panel, 1)
        assertEquals(url, searchPanel.urlText)
        assertEquals(url, service.lastUrl)
    }

    @Test
    fun `right-click inside existing table selection preserves multi-selection`() = runBlocking {
        val service = MultiResultOnlineSearchService()
        val panel = OnlineSearchPanel(
            host = TestOnlineSearchHost(),
            ardService = service,
            zdfService = service,
        )
        SwingUtilities.invokeAndWait {
            panel.searchPanel.queryText = "tatort"
            panel.searchPanel.searchButton.doClick()
        }
        waitForTableRows(panel, 2)

        SwingUtilities.invokeAndWait {
            panel.table.setRowSelectionInterval(0, 1)
            triggerPopupOnRow(panel, 1)
        }

        assertEquals(listOf(0, 1), panel.table.selectedRows.toList())
    }

    @Test
    fun `double-click below table rows does not open stale selected result`() = runBlocking {
        val service = MultiResultOnlineSearchService()
        val host = TestOnlineSearchHost()
        val panel = OnlineSearchPanel(
            host = host,
            ardService = service,
            zdfService = service,
        )
        SwingUtilities.invokeAndWait {
            panel.searchPanel.queryText = "tatort"
            panel.searchPanel.searchButton.doClick()
        }
        waitForTableRows(panel, 2)

        SwingUtilities.invokeAndWait {
            panel.table.setSize(500, 500)
            panel.table.setRowSelectionInterval(0, 0)
            triggerDoubleClickBelowRows(panel)
        }

        assertEquals(null, host.filmInfoResult)
    }

    @Test
    fun `double-click selected table row opens clicked row instead of first selected row`() = runBlocking {
        val service = MultiResultOnlineSearchService()
        val host = TestOnlineSearchHost()
        val panel = OnlineSearchPanel(
            host = host,
            ardService = service,
            zdfService = service,
        )
        SwingUtilities.invokeAndWait {
            panel.searchPanel.queryText = "tatort"
            panel.searchPanel.searchButton.doClick()
        }
        waitForTableRows(panel, 2)

        SwingUtilities.invokeAndWait {
            panel.table.setRowSelectionInterval(0, 1)
            triggerDoubleClickOnRow(panel, 1)
        }

        assertEquals("Tatort 2", host.filmInfoResult?.title)
    }
}

private fun Container.doLayoutRecursively() {
    doLayout()
    components.filterIsInstance<Container>().forEach { it.doLayoutRecursively() }
}

private fun Container.findLabel(text: String): JLabel = components.asSequence()
    .mapNotNull { component ->
        when {
            component is JLabel && component.text == text -> component
            component is Container -> component.findLabelOrNull(text)
            else -> null
        }
    }
    .firstOrNull()
    ?: error("Label not found: $text")

private fun Container.findLabelOrNull(text: String): JLabel? = components.asSequence()
    .mapNotNull { component ->
        when {
            component is JLabel && component.text == text -> component
            component is Container -> component.findLabelOrNull(text)
            else -> null
        }
    }
    .firstOrNull()

private fun JLabel.xIn(container: Container): Int = SwingUtilities.convertPoint(parent, x, y, container).x

private fun triggerPopupOnRow(panel: OnlineSearchPanel, row: Int) {
    val bounds = panel.table.getCellRect(row, 0, true)
    val event = MouseEvent(
        panel.table,
        MouseEvent.MOUSE_RELEASED,
        System.currentTimeMillis(),
        0,
        bounds.x + 1,
        bounds.y + 1,
        1,
        true,
        MouseEvent.BUTTON3,
    )
    try {
        panel.table.mouseListeners.forEach { it.mouseReleased(event) }
    } catch (_: IllegalComponentStateException) {
        // The popup cannot be shown for an off-screen test table; selection has already been updated.
    }
}

private fun triggerDoubleClickBelowRows(panel: OnlineSearchPanel) {
    val lastRowBounds = panel.table.getCellRect(panel.table.rowCount - 1, 0, true)
    val event = MouseEvent(
        panel.table,
        MouseEvent.MOUSE_CLICKED,
        System.currentTimeMillis(),
        0,
        lastRowBounds.x + 1,
        lastRowBounds.y + lastRowBounds.height + 10,
        2,
        false,
        MouseEvent.BUTTON1,
    )
    panel.table.mouseListeners.forEach { it.mouseClicked(event) }
}

private fun triggerDoubleClickOnRow(panel: OnlineSearchPanel, row: Int) {
    val bounds = panel.table.getCellRect(row, 0, true)
    val event = MouseEvent(
        panel.table,
        MouseEvent.MOUSE_CLICKED,
        System.currentTimeMillis(),
        0,
        bounds.x + 1,
        bounds.y + 1,
        2,
        false,
        MouseEvent.BUTTON1,
    )
    panel.table.mouseListeners.forEach { it.mouseClicked(event) }
}

private suspend fun waitForTableRows(panel: OnlineSearchPanel, rows: Int) {
    withTimeout(2_000) {
        while (panel.table.rowCount != rows) {
            SwingUtilities.invokeAndWait { }
            delay(10)
        }
    }
}

private class ResultOnlineSearchService : OnlineSearchService {
    var lastSearchQuery: String? = null
        private set
    var lastUrl: String? = null
        private set

    private val result = OnlineSearchResult(
        provider = OnlineSearchProvider.ARD,
        sender = "ARD",
        topic = "Tatort",
        title = "Tatort Folge",
        normalQualityUrl = "https://cdn.example/tatort.mp4",
    )

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        lastSearchQuery = request.query
        return OnlineSearchPage(
            results = listOf(result),
            nextToken = null,
        )
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult {
        lastUrl = request.url
        return result
    }
}

private class FailingSecondUrlOnlineSearchService : OnlineSearchService {
    private var urlRequests = 0

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage = OnlineSearchPage(emptyList(), nextToken = null)

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult {
        urlRequests += 1
        if (urlRequests > 1) error("URL lookup failed")
        return OnlineSearchResult(
            provider = OnlineSearchProvider.ARD,
            sender = "ARD",
            topic = "Tatort",
            title = "Tatort Folge",
            normalQualityUrl = "https://cdn.example/tatort.mp4",
        )
    }
}

private class MultiResultOnlineSearchService : OnlineSearchService {
    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage = OnlineSearchPage(
        results = listOf(
            result("Tatort 1"),
            result("Tatort 2"),
        ),
        nextToken = null,
    )

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? = null

    private fun result(title: String) = OnlineSearchResult(
        provider = OnlineSearchProvider.ARD,
        sender = "ARD",
        topic = "Tatort",
        title = title,
        normalQualityUrl = "https://cdn.example/$title.mp4",
    )
}

private class PaginatedOnlineSearchService : OnlineSearchService {
    val requestedTokens = mutableListOf<String?>()

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        requestedTokens += request.nextToken
        return when (request.nextToken) {
            null -> OnlineSearchPage(listOf(result("Tatort 1")), nextToken = "page-2")
            "page-2" -> OnlineSearchPage(listOf(result("Tatort 2")), nextToken = null)
            else -> OnlineSearchPage(emptyList(), nextToken = null)
        }
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? = null

    private fun result(title: String) = OnlineSearchResult(
        provider = OnlineSearchProvider.ARD,
        sender = "ARD",
        topic = "Tatort",
        title = title,
        normalQualityUrl = "https://cdn.example/$title.mp4",
    )
}

private class BlockingSecondPageOnlineSearchService : OnlineSearchService {
    val secondPageRequested = CompletableDeferred<Unit>()
    val allowSecondPage = CompletableDeferred<Unit>()

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage = when (request.nextToken) {
        null -> OnlineSearchPage(listOf(result("Tatort 1")), nextToken = "page-2")
        "page-2" -> {
            secondPageRequested.complete(Unit)
            allowSecondPage.await()
            OnlineSearchPage(listOf(result("Tatort 2")), nextToken = null)
        }
        else -> OnlineSearchPage(emptyList(), nextToken = null)
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? = null

    private fun result(title: String) = OnlineSearchResult(
        provider = OnlineSearchProvider.ARD,
        sender = "ARD",
        topic = "Tatort",
        title = title,
        normalQualityUrl = "https://cdn.example/$title.mp4",
    )
}

private class BlockingOnlineSearchService : OnlineSearchService {
    val started = CompletableDeferred<Unit>()
    val cancelled = CompletableDeferred<Unit>()

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        started.complete(Unit)
        try {
            awaitCancellation()
        } finally {
            cancelled.complete(Unit)
        }
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? = null
}

private class DelayedCancellationOnlineSearchService : OnlineSearchService {
    val firstSearchStarted = CompletableDeferred<Unit>()
    val firstSearchCancellationStarted = CompletableDeferred<Unit>()
    val allowFirstSearchCancellationToComplete = CompletableDeferred<Unit>()
    val secondSearchStarted = CompletableDeferred<Unit>()
    private val searchCount = AtomicInteger()

    override suspend fun search(request: OnlineSearchRequest): OnlineSearchPage {
        when (searchCount.incrementAndGet()) {
            1 -> {
                firstSearchStarted.complete(Unit)
                try {
                    awaitCancellation()
                } finally {
                    firstSearchCancellationStarted.complete(Unit)
                    allowFirstSearchCancellationToComplete.await()
                }
            }
            2 -> {
                secondSearchStarted.complete(Unit)
                awaitCancellation()
            }
            else -> awaitCancellation()
        }
    }

    override suspend fun loadByUrl(request: OnlineUrlRequest): OnlineSearchResult? = null
}

private class TestOnlineSearchHost : OnlineSearchHost {
    override fun updateCurrentResult(result: OnlineSearchResult?) = Unit

    var filmInfoResult: OnlineSearchResult? = null
        private set

    override fun showFilmInfo(result: OnlineSearchResult) {
        filmInfoResult = result
    }

    override fun startDownload(results: List<OnlineSearchResult>) = Unit
    override fun playResult(result: OnlineSearchResult) = Unit
}
