package mediathek.gui.tabs.tab_online_search

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.TransactionList
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.tool.withWriteLock
import org.apache.logging.log4j.LogManager
import java.awt.BorderLayout
import java.awt.Component
import java.awt.Dimension
import java.awt.FlowLayout
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.*

class OnlineSearchPanel(
    private val host: OnlineSearchHost,
    private val ardService: OnlineSearchService = ArdOnlineSearchService(),
    private val zdfService: OnlineSearchService = ZdfOnlineSearchService(),
    private val arteService: OnlineSearchService = ArteOnlineSearchService(),
    private val historyStore: OnlineSearchHistoryStore = ApplicationOnlineSearchHistoryStore,
) : JPanel(BorderLayout()), AutoCloseable {
    constructor(host: OnlineSearchHost) : this(
        host,
        ArdOnlineSearchService(),
        ZdfOnlineSearchService(),
        ArteOnlineSearchService(),
        ApplicationOnlineSearchHistoryStore,
    )

    val senderComboBox = JComboBox(OnlineSearchProvider.entries.toTypedArray()).apply {
        preferredSize = Dimension(SENDER_COMBO_BOX_MAXIMUM_WIDTH, preferredSize.height)
        maximumSize = Dimension(SENDER_COMBO_BOX_MAXIMUM_WIDTH, maximumSize.height)
        renderer = object : DefaultListCellRenderer() {
            override fun getListCellRendererComponent(
                list: JList<*>?,
                value: Any?,
                index: Int,
                isSelected: Boolean,
                cellHasFocus: Boolean,
            ): Component = super.getListCellRendererComponent(
                list,
                (value as? OnlineSearchProvider)?.displayName ?: value,
                index,
                isSelected,
                cellHasFocus,
            )
        }
    }
    val searchPanel = ProviderSearchPanel()
    private val resultList = TransactionList<OnlineSearchResult>(BasicEventList())
    val table = OnlineSearchResultTable(resultList)
    private val statusLabel = JLabel("Bereit")
    private var panelJob = SupervisorJob()
    private var panelScope = CoroutineScope(panelJob + Dispatchers.Swing)
    private var searchJob: Job? = null
    private var searchGeneration = 0L
    private var closed = false

    init {
        add(createSearchArea(), BorderLayout.NORTH)
        add(JScrollPane(table), BorderLayout.CENTER)
        add(statusLabel, BorderLayout.SOUTH)
        installActions()
        installSelectionSync()
        installMouseActions()
        loadHistories()
    }

    private fun createSearchArea(): JPanel = JPanel(BorderLayout()).apply {
        add(
            JPanel(FlowLayout(FlowLayout.LEFT, 0, 0)).apply {
                border = BorderFactory.createEmptyBorder(8, 8, 0, 8)
                add(JLabel("Sender"))
                add(Box.createHorizontalStrut(8))
                add(senderComboBox)
            },
            BorderLayout.NORTH,
        )
        add(searchPanel, BorderLayout.CENTER)
    }

    override fun addNotify() {
        super.addNotify()
        if (!panelJob.isActive) {
            panelJob = SupervisorJob()
            panelScope = CoroutineScope(panelJob + Dispatchers.Swing)
        }
    }

    override fun removeNotify() {
        table.saveColumnState()
        searchJob?.cancel()
        searchJob = null
        searchGeneration += 1
        panelJob.cancel()
        super.removeNotify()
    }

    override fun close() {
        if (closed) return
        closed = true
        searchJob?.cancel()
        searchJob = null
        panelJob.cancel()
        runCatching(table::dispose)
            .onFailure { failure -> logger.warn("Failed to dispose online search table", failure) }
        runCatching(resultList::dispose)
            .onFailure { failure -> logger.warn("Failed to dispose online search result list", failure) }
    }

    private fun installActions() {
        senderComboBox.addActionListener { switchProvider(selectedProvider()) }
        searchPanel.addSearchListener { startTextSearch(selectedProvider()) }
        searchPanel.addCancelListener { cancelSearch(selectedProvider()) }
        searchPanel.addQueryClearedListener { clearSearchResults() }
        searchPanel.addUrlClearedListener { clearSearchResults() }
        searchPanel.addUrlSearchListener { startUrlSearch(selectedProvider()) }
        searchPanel.addQueryHistorySelectionListener { startTextSearch(selectedProvider()) }
        searchPanel.addUrlHistorySelectionListener { startUrlSearch(selectedProvider()) }
        searchPanel.addQueryHistoryChangeListener { entries ->
            historyStore.writeQueryHistory(selectedProvider(), OnlineSearchHistory.of(entries))
        }
        searchPanel.addUrlHistoryChangeListener { entries ->
            historyStore.writeUrlHistory(selectedProvider(), OnlineSearchHistory.of(entries))
        }
    }

    private fun loadHistories() {
        updateVisibleHistory(selectedProvider())
    }

    private fun updateVisibleHistory(provider: OnlineSearchProvider) {
        searchPanel.setQueryHistory(historyStore.readQueryHistory(provider).entries)
        searchPanel.setUrlHistory(historyStore.readUrlHistory(provider).entries)
    }

    private fun switchProvider(provider: OnlineSearchProvider) {
        searchJob?.cancel()
        searchJob = null
        searchGeneration += 1
        updateVisibleHistory(provider)
        searchPanel.clearSearchFields()
        clearDisplayedResults()
        host.updateCurrentResult(null)
        searchPanel.updateRunningState(running = false)
        statusLabel.text = "Bereit"
    }

    private fun rememberSearch(provider: OnlineSearchProvider, query: String) {
        val history = historyStore.readQueryHistory(provider).withEntry(query)
        historyStore.writeQueryHistory(provider, history)
        if (selectedProvider() == provider) searchPanel.setQueryHistory(history.entries)
    }

    private fun rememberUrl(provider: OnlineSearchProvider, url: String) {
        val history = historyStore.readUrlHistory(provider).withEntry(url)
        historyStore.writeUrlHistory(provider, history)
        if (selectedProvider() == provider) searchPanel.setUrlHistory(history.entries)
    }

    private fun selectedProvider(): OnlineSearchProvider = senderComboBox.selectedItem as OnlineSearchProvider

    private fun installSelectionSync() {
        table.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                host.updateCurrentResult(table.selectedResult())
            }
        }
    }

    private fun installMouseActions() {
        table.addMouseListener(object : MouseAdapter() {
            override fun mouseClicked(e: MouseEvent) {
                val row = table.rowAtPoint(e.point)
                if (SwingUtilities.isLeftMouseButton(e) && e.clickCount == 2 && row >= 0) {
                    table.resultAtViewRow(row)?.let(host::showFilmInfo)
                }
            }

            override fun mousePressed(e: MouseEvent) = maybeShowPopup(e)
            override fun mouseReleased(e: MouseEvent) = maybeShowPopup(e)
        })
    }

    private fun maybeShowPopup(e: MouseEvent) {
        if (!e.isPopupTrigger) return
        val row = table.rowAtPoint(e.point)
        if (row >= 0 && !table.isRowSelected(row)) {
            table.selectionModel.setSelectionInterval(row, row)
        }
        val selectedResult = table.resultAtViewRow(row)
        val selectedResults = if (row >= 0) table.selectedResults() else emptyList()
        OnlineSearchContextMenu(selectedResult, selectedResults, host).show(table, e.x, e.y)
    }

    private fun startTextSearch(provider: OnlineSearchProvider) {
        val panel = searchPanel
        val query = panel.queryText.trim()
        if (query.length < 3) return
        panel.urlText = ""
        panel.updateRunningState(running = false)
        launchTextSearch(provider, query)
    }

    private fun launchTextSearch(provider: OnlineSearchProvider, query: String) {
        searchJob?.cancel()
        val generation = ++searchGeneration
        val providerPanel = searchPanel
        searchJob = panelScope.launch {
            try {
                senderComboBox.isEnabled = false
                providerPanel.updateRunningState(running = true)
                statusLabel.text = "Suche ${provider.displayName} …"
                clearDisplayedResults()
                host.updateCurrentResult(null)
                var nextToken: String? = null
                var totalResults: Long? = null
                do {
                    val currentToken = nextToken
                    val page = withContext(Dispatchers.IO) {
                        searchProvider(provider, OnlineSearchRequest(provider, query, currentToken))
                    }
                    resultList.updateResults { addAll(page.results) }
                    nextToken = page.nextToken
                    totalResults = page.totalResults ?: totalResults
                    statusLabel.text = formatSearchStatus(resultList.size, totalResults, page.hasNextPage)
                } while (!nextToken.isNullOrBlank())
                rememberSearch(provider, query)
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                logger.warn("Online search failed", ex)
                statusLabel.text = "Suche fehlgeschlagen: ${ex.message ?: ex.javaClass.simpleName}"
            } finally {
                if (generation == searchGeneration) {
                    senderComboBox.isEnabled = true
                    providerPanel.updateRunningState(running = false)
                }
            }
        }
    }

    private suspend fun searchProvider(provider: OnlineSearchProvider, request: OnlineSearchRequest): OnlineSearchPage =
        when (provider) {
            OnlineSearchProvider.ARD -> ardService.search(request)
            OnlineSearchProvider.ZDF -> zdfService.search(request)
            OnlineSearchProvider.ARTE -> arteService.search(request)
        }

    private fun formatSearchStatus(loadedResults: Int, totalResults: Long?, hasNextPage: Boolean): String =
        if (hasNextPage && totalResults != null) {
            "$loadedResults / $totalResults Filme geladen"
        } else {
            "$loadedResults Filme gefunden"
        }

    private fun startUrlSearch(provider: OnlineSearchProvider) {
        val panel = searchPanel
        val url = panel.urlText.trim()
        if (url.isEmpty()) return
        panel.queryText = ""
        clearDisplayedResults()
        host.updateCurrentResult(null)
        launchSearch(provider, onSuccess = { rememberUrl(provider, url) }) {
            val result = when (provider) {
                OnlineSearchProvider.ARD -> ardService.loadByUrl(OnlineUrlRequest(provider, url))
                OnlineSearchProvider.ZDF -> zdfService.loadByUrl(OnlineUrlRequest(provider, url))
                OnlineSearchProvider.ARTE -> arteService.loadByUrl(OnlineUrlRequest(provider, url))
            }
            OnlineSearchPage(listOfNotNull(result), nextToken = null)
        }
    }

    private fun cancelSearch(provider: OnlineSearchProvider) {
        val job = searchJob?.takeIf { it.isActive } ?: return
        statusLabel.text = "Suche ${provider.displayName} abgebrochen"
        job.cancel()
    }

    private fun clearSearchResults() {
        searchJob?.cancel()
        searchJob = null
        searchGeneration += 1
        clearDisplayedResults()
        host.updateCurrentResult(null)
        searchPanel.updateRunningState(running = false)
        statusLabel.text = "Bereit"
    }

    private fun clearDisplayedResults() {
        resultList.updateResults { clear() }
        table.clearSelection()
    }

    private fun launchSearch(
        provider: OnlineSearchProvider,
        onSuccess: () -> Unit = {},
        block: suspend () -> OnlineSearchPage,
    ) {
        searchJob?.cancel()
        val generation = ++searchGeneration
        val providerPanel = searchPanel
        searchJob = panelScope.launch {
            try {
                senderComboBox.isEnabled = false
                providerPanel.updateRunningState(running = true)
                statusLabel.text = "Suche ${provider.displayName} …"
                val page = withContext(Dispatchers.IO) { block() }
                resultList.updateResults {
                    clear()
                    addAll(page.results)
                }
                statusLabel.text = "${resultList.size} Filme gefunden"
                onSuccess()
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                logger.warn("Online search failed", ex)
                statusLabel.text = "Suche fehlgeschlagen: ${ex.message ?: ex.javaClass.simpleName}"
            } finally {
                if (generation == searchGeneration) {
                    senderComboBox.isEnabled = true
                    providerPanel.updateRunningState(running = false)
                }
            }
        }
    }

    companion object {
        private const val SENDER_COMBO_BOX_MAXIMUM_WIDTH = 150
        private val logger = LogManager.getLogger(OnlineSearchPanel::class.java)
    }
}

internal fun TransactionList<OnlineSearchResult>.updateResults(update: EventList<OnlineSearchResult>.() -> Unit) {
    withWriteLock {
        withTransaction {
            update()
        }
    }
}

interface OnlineSearchHost {
    fun updateCurrentResult(result: OnlineSearchResult?)
    fun showFilmInfo(result: OnlineSearchResult)
    fun startDownload(results: List<OnlineSearchResult>)
    fun playResult(result: OnlineSearchResult)
}
