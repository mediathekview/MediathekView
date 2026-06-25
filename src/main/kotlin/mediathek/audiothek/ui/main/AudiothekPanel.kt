/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.audiothek.ui.main

import com.jidesoft.popup.JidePopup
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.audiothek.download.AudioDownloadTaskSnapshot
import mediathek.audiothek.download.AudioDownloadTaskState
import mediathek.audiothek.download.PersistentAudioDownloadManager
import mediathek.audiothek.model.AudioDataset
import mediathek.audiothek.model.AudioEntry
import mediathek.audiothek.repository.AudioLoadResult
import mediathek.audiothek.repository.AudioRepository
import mediathek.audiothek.repository.OnlineSearchProxyRepository
import mediathek.audiothek.ui.download.AudioDownloadManagerPanel
import mediathek.audiothek.ui.download.DownloadSummary
import mediathek.audiothek.ui.table.AudiothekTable
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.SeenHistoryController
import mediathek.gui.actions.ShowAudiothekSearchHelpAction
import mediathek.gui.actions.UrlHyperlinkAction
import mediathek.gui.tabs.tab_film.FilmDescriptionPanel
import mediathek.mac.MacMultimediaPlayerLocator
import mediathek.mac.SingleIinaPlayer
import mediathek.swing.OverlayPanel
import mediathek.tool.FileDialogs
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.notification.MessageType
import mediathek.tool.notification.NotificationMessage
import mediathek.tool.notification.NotificationService
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import org.jdesktop.swingx.VerticalLayout
import java.awt.*
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.MouseEvent
import java.io.File
import java.net.URI
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.*
import kotlin.time.Duration.Companion.ZERO
import kotlin.time.toKotlinDuration

class AudiothekPanel(
    private val repository: AudioRepository,
    private val owner: Frame,
) : JPanel(BorderLayout()) {
    private val logger = LogManager.getLogger(AudiothekPanel::class.java)
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private var loadJob: Job? = null
    private var podcastSearchJob: Job? = null

    private val table = AudiothekTable(
        onOpenAudio = ::openAudioEntry,
        onDownload = ::downloadAudioEntry
    )

    private val statusPanel = AudiothekStatusPanel(ageProvider = ::currentDatasetAge)
    private val detailsPanel = FilmDescriptionPanel()
    private val toolBar = AudiothekToolBar()
    private val onlineSearchProxyRepository = OnlineSearchProxyRepository()
    private val tableScrollPane = JScrollPane(table)
    private val errorOverlay = OverlayPanel("Audiothek konnte nicht geladen werden")
    private val tableContainer = JLayeredPane().apply {
        layout = OverlayLayout(this)
        add(errorOverlay)
        add(tableScrollPane)
    }
    private val southPanel = JPanel(VerticalLayout()).apply {
        add(statusPanel)
        add(detailsPanel)
    }

    private val downloadManager = PersistentAudioDownloadManager(
        ::handleDownloadCompleted,
        ::handleDownloadFailed
    )
    private val downloadManagerPanel = AudioDownloadManagerPanel()
    private val downloadManagerPopup = JidePopup().apply {
        contentPane.layout = BorderLayout()
        contentPane.add(downloadManagerPanel, BorderLayout.CENTER)
        owner = toolBar.downloadManagerAnchor()
        isMovable = false
        isResizable = true
        isAttachable = false
        isTransient = false
        isFocusable = true
        isKeepPreviousSize = false
        defaultMoveOperation = JidePopup.HIDE_ON_MOVED
    }
    private val downloadPopupOutsideClickListener = java.awt.event.AWTEventListener { event ->
        if (event !is MouseEvent || event.id != MouseEvent.MOUSE_PRESSED) {
            return@AWTEventListener
        }
        if (!isDownloadManagerVisible) {
            return@AWTEventListener
        }
        if (isInsideDownloadPopup(event) || SwingUtilities.isDescendingFrom(event.component, toolBar.downloadManagerAnchor())) {
            return@AWTEventListener
        }
        SwingUtilities.invokeLater {
            hideDownloadManagerIfVisible()
        }
    }
    private val activeDownloadCount = AtomicInteger(0)
    private var datasetTimestamp: LocalDateTime? = null
    private val iinaPlayer = SingleIinaPlayer { owner }
    private val isDownloadManagerVisible: Boolean
        get() = downloadManagerPopup.isPopupVisible

    init {
        toolBar.setHelpAction(ShowAudiothekSearchHelpAction())
        toolBar.setOnlineSearchEnabled(isPersistedOnlineSearchEnabled())
        add(toolBar, BorderLayout.NORTH)
        add(tableContainer, BorderLayout.CENTER)
        add(southPanel, BorderLayout.SOUTH)
        errorOverlay.isVisible = false
        syncErrorOverlayBounds()
        Toolkit.getDefaultToolkit().addAWTEventListener(downloadPopupOutsideClickListener, AWTEvent.MOUSE_EVENT_MASK)
        setupListeners()
    }

    fun disposePanel() {
        downloadManagerPopup.hidePopupImmediately()
        Toolkit.getDefaultToolkit().removeAWTEventListener(downloadPopupOutsideClickListener)
        pauseDownloadsForShutdown()
        table.dispose()
        table.saveState()
        podcastSearchJob?.cancel()
        uiScope.cancel()
    }

    fun activeDownloadCount(): Int = activeDownloadCount.get()

    fun loadIfNecessary() = requestInitialLoad()

    fun pauseDownloadsForShutdown() {
        downloadManagerPopup.hidePopupImmediately()
        runBlocking {
            downloadManager.shutdown()
        }
    }

    private fun setupListeners() {
        addComponentListener(object : ComponentAdapter() {
            override fun componentShown(event: ComponentEvent?) {
                SwingUtilities.invokeLater(::requestInitialLoad)
            }
        })
        toolBar.addReloadListener { triggerLoad(isManualReload = true) }
        table.addEntrySelectionListener {
            if (!it.valueIsAdjusting) {
                detailsPanel.setCurrentAudioEntry(table.selectedEntry())
            }
        }
        toolBar.addFilterSubmitListener(::applyFilterNow)
        toolBar.addClearSearchListener { applyFilterNow("") }
        toolBar.addOnlineSearchToggleListener(::handleOnlineSearchToggled)
        toolBar.addDownloadManagerListener(::toggleDownloadManager)
        downloadManagerPanel.addProgressListener(::updateDownloadSummary)
        downloadManagerPanel.addPrimaryActionListener(::handleDownloadPrimaryAction)
        downloadManagerPanel.addSecondaryActionListener(::handleDownloadSecondaryAction)
        downloadManagerPanel.addRemoveActionListener(::handleDownloadRemoveAction)
        downloadManagerPanel.addEmptyListener {
            hideDownloadManagerIfVisible()
        }
        downloadManager.addListener { snapshots ->
            SwingUtilities.invokeLater {
                downloadManagerPanel.setTasks(snapshots)
            }
        }
        tableScrollPane.addComponentListener(object : ComponentAdapter() {
            override fun componentResized(event: ComponentEvent?) {
                syncErrorOverlayBounds()
            }
        })
    }

    private fun shouldLoadWhenShown(): Boolean {
        return datasetTimestamp == null && !table.hasEntries() && loadJob?.isActive != true
    }

    private fun requestInitialLoad() {
        if (shouldLoadWhenShown()) {
            triggerLoad(isManualReload = false)
        }
    }

    private fun isPersistedOnlineSearchEnabled(): Boolean {
        return ApplicationConfiguration.getInstance().audiothekOnlineSearch
    }

    private fun persistOnlineSearchEnabled(enabled: Boolean) {
        ApplicationConfiguration.getInstance().audiothekOnlineSearch = enabled
    }

    private fun triggerLoad(isManualReload: Boolean) {
        loadJob?.cancel()
        loadJob = uiScope.launch {
            setLoadingState(true)
            hideErrorOverlay()

            try {
                try {
                    val result = repository.loadAudiothek()
                    handleLoadSuccess(result, isManualReload)
                } catch (error: Throwable) {
                    handleLoadFailure(error, isManualReload)
                }
            } finally {
                setLoadingState(false)
            }
        }
    }

    private suspend fun handleLoadSuccess(result: AudioLoadResult, isManualReload: Boolean) {
        if (shouldSkipTableRefresh(result, isManualReload)) {
            showReloadMessage(result)
            return
        }

        val query = toolBar.currentQuery()
        val visibleSearchFields = table.visibleSearchFieldsSnapshot()
        val preparedRows = withContext(Dispatchers.Default) {
            AudiothekTable.prepareRows(result.dataset.entries, query, visibleSearchFields)
        }

        datasetTimestamp = result.dataset.createdAtLocal
        table.applyPreparedRows(preparedRows)
        statusPanel.setStandVisible(true)
        statusPanel.setStand(formatDatasetStand(result.dataset))
        refreshVisibleResults()
        triggerPodcastSearch(query, resetState = false)
        if (isManualReload) {
            showReloadMessage(result)
        }
    }

    private fun shouldSkipTableRefresh(result: AudioLoadResult, isManualReload: Boolean): Boolean {
        if (!isManualReload) {
            return false
        }
        if (result.hasUpdatedSource()) {
            return false
        }
        return datasetTimestamp != null
    }

    private fun handleLoadFailure(error: Throwable, isManualReload: Boolean) {
        logger.error("Failed to load Audiothek data", error)

        if (isManualReload && table.rowCount > 0) {
            JOptionPane.showMessageDialog(
                this,
                buildLoadFailureMessage(error),
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE
            )
            return
        }

        table.setRows(emptyList())
        datasetTimestamp = null
        showErrorOverlay()
        detailsPanel.setCurrentAudioEntry(null)
        statusPanel.setStandVisible(false)
        statusPanel.setStand(emptyDatasetStand())
        statusPanel.setCount("0 Einträge")
    }

    private fun currentDatasetAge(): kotlin.time.Duration? {
        val timestamp = datasetTimestamp ?: return null
        return java.time.Duration.between(timestamp, LocalDateTime.now())
            .coerceAtLeast(java.time.Duration.ZERO)
            .toKotlinDuration()
            .coerceAtLeast(ZERO)
    }

    private fun formatDatasetStand(dataset: AudioDataset): String {
        return "Audiothek erstellt: ${formatDatasetTimestamp(dataset.createdAtLocal)}"
    }

    private fun formatDatasetTimestamp(timestamp: LocalDateTime?): String =
        timestamp?.format(DATASET_TIMESTAMP_FORMAT) ?: "-"

    private fun emptyDatasetStand(): String = "Audiothek erstellt: -"

    private fun setLoadingState(loading: Boolean) {
        toolBar.setLoading(loading)
        statusPanel.setLoading(loading)
    }

    private fun toggleDownloadManager() {
        if (isDownloadManagerVisible) {
            hideDownloadManagerIfVisible()
            return
        }
        downloadManagerPopup.owner = toolBar.downloadManagerAnchor()
        downloadManagerPopup.showPopup(toolBar.downloadManagerAnchor())
    }

    private fun hideDownloadManagerIfVisible() {
        if (isDownloadManagerVisible) {
            downloadManagerPopup.hidePopup()
        }
    }

    private fun updateDownloadSummary(summary: DownloadSummary) {
        activeDownloadCount.set(summary.activeCount)
        toolBar.setDownloadProgress(summary)
        statusPanel.setActiveDownloads(summary.activeCount)
    }

    private fun isInsideDownloadPopup(event: MouseEvent): Boolean {
        val component = event.component ?: return false
        if (SwingUtilities.isDescendingFrom(component, downloadManagerPanel)) {
            return true
        }
        val popupWindow = SwingUtilities.getWindowAncestor(downloadManagerPanel) ?: return false
        return component === popupWindow || SwingUtilities.isDescendingFrom(component, popupWindow)
    }

    private fun buildLoadFailureMessage(error: Throwable): String {
        val errorMessage = error.message?.takeIf(String::isNotBlank)
        return buildString {
            append("<html>Das Laden ist fehlgeschlagen")
            append(if (errorMessage == null) "." else ":")
            errorMessage?.let {
                append("<br/>").append("<i>").append(it).append("</i>")
            }
            append("</html>")
        }
    }

    private fun showErrorOverlay() {
        syncErrorOverlayBounds()
        errorOverlay.isVisible = true
        tableContainer.repaint()
    }

    private fun hideErrorOverlay() {
        errorOverlay.isVisible = false
        tableContainer.repaint()
    }

    private fun syncErrorOverlayBounds() {
        errorOverlay.size = tableScrollPane.size
        errorOverlay.revalidate()
    }

    private fun applyFilterNow(query: String) {
        cancelPodcastSearch()
        table.clearExternalSearchEntries()
        table.applyFilter(query)
        refreshVisibleResults()
        triggerPodcastSearch(query, resetState = false)
    }

    private fun handleOnlineSearchToggled(enabled: Boolean) {
        persistOnlineSearchEnabled(enabled)

        val query = toolBar.currentQuery()
        if (query.isBlank() && table.hasCurrentFilterQuery(query)) {
            podcastSearchJob?.cancel()
            toolBar.setPodcastSearchBusy(false)
            refreshResultCount()
            return
        }

        applyFilterNow(query)
    }

    private fun triggerPodcastSearch(query: String, resetState: Boolean = true) {
        if (resetState) {
            resetExternalSearchState()
        } else {
            podcastSearchJob?.cancel()
            toolBar.setPodcastSearchBusy(false)
        }

        val normalizedQuery = query.trim()
        if (!toolBar.isOnlineSearchEnabled()) {
            refreshSelectionState()
            return
        }
        val onlineSearchQuery = AudiothekOnlineSearchQuery.from(normalizedQuery) ?: run {
            refreshSelectionState()
            return
        }

        podcastSearchJob = uiScope.launch {
            toolBar.setPodcastSearchBusy(true)
            try {
                val externalEntries = onlineSearchQuery.filter(loadExternalSearchEntries(onlineSearchQuery.query))

                if (!isCurrentQuery(normalizedQuery) || !toolBar.isOnlineSearchEnabled()) {
                    return@launch
                }

                table.setExternalSearchEntries(externalEntries)
                refreshVisibleResults()
            } finally {
                if (isCurrentQuery(normalizedQuery)) {
                    toolBar.setPodcastSearchBusy(false)
                }
            }
        }
    }

    private fun resetExternalSearchState() {
        cancelPodcastSearch()
        table.clearExternalSearchEntries()
        refreshResultCount()
    }

    private fun cancelPodcastSearch() {
        podcastSearchJob?.cancel()
        toolBar.setPodcastSearchBusy(false)
    }

    private fun refreshVisibleResults() {
        refreshResultCount()
        refreshSelectionState()
    }

    private fun refreshResultCount() {
        statusPanel.setCount("${table.rowCount} Treffer")
    }

    private suspend fun loadExternalSearchEntries(query: String): List<AudioEntry> {
        return runCatching { onlineSearchProxyRepository.search(query) }
            .onFailure { logger.warn("Online-Suche über Proxy fehlgeschlagen für '{}'", query, it) }
            .getOrDefault(emptyList())
    }

    private fun isCurrentQuery(query: String): Boolean {
        return toolBar.currentQuery().trim() == query
    }

    private fun refreshSelectionState() {
        if (table.rowCount > 0) {
            table.selectFirstRow()
            return
        }
        detailsPanel.setCurrentAudioEntry(null)
    }

    private fun openAudioEntry(entry: AudioEntry) {
        (entry.audioUrl ?: entry.websiteUrl)?.let(::openExternal)
    }

    private fun downloadAudioEntry(entry: AudioEntry) {
        val audioUrl = entry.audioUrl
        if (audioUrl == null) {
            JOptionPane.showMessageDialog(
                this,
                "Für diesen Eintrag ist keine Download-URL vorhanden.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE
            )
            return
        }

        val targetFile = chooseDownloadTarget(entry) ?: return
        downloadManager.enqueue(entry, targetFile.toPath())
    }

    private fun chooseDownloadTarget(entry: AudioEntry): File? {
        return FileDialogs.chooseSaveFileLocation(
            owner,
            "Audio speichern",
            suggestFileName(entry)
        )
    }

    private fun suggestFileName(entry: AudioEntry): String {
        val fallbackName = entry.title.ifBlank { "audio" }
        val sanitizedBaseName = fallbackName
            .replace(Regex("""[\\/:*?"<>|]"""), "_")
            .replace(Regex("""\s+"""), " ")
            .trim()
            .ifBlank { "audio" }
        val path = entry.audioUrl?.path.orEmpty()
        val extension = path.substringAfterLast('/', "")
            .substringAfterLast('.', "")
            .takeIf { it.isNotBlank() && it.length <= 8 }
            ?.let { ".$it" }
            .orEmpty()
        return sanitizedBaseName + extension
    }

    private fun handleDownloadPrimaryAction(taskId: String) {
        val snapshot = downloadManager.snapshot(taskId) ?: return
        when (snapshot.state) {
            AudioDownloadTaskState.DOWNLOADING -> downloadManager.pause(taskId)
            AudioDownloadTaskState.PAUSED,
            AudioDownloadTaskState.FAILED,
            AudioDownloadTaskState.CANCELLED -> downloadManager.resume(taskId)
            AudioDownloadTaskState.COMPLETED -> Unit
        }
    }

    private fun handleDownloadSecondaryAction(taskId: String) {
        val snapshot = downloadManager.snapshot(taskId) ?: return
        when (snapshot.state) {
            AudioDownloadTaskState.DOWNLOADING,
            AudioDownloadTaskState.PAUSED -> downloadManager.cancel(taskId)
            AudioDownloadTaskState.FAILED,
            AudioDownloadTaskState.CANCELLED,
            AudioDownloadTaskState.COMPLETED -> Unit
        }
    }

    private fun handleDownloadRemoveAction(taskId: String) {
        val snapshot = downloadManager.snapshot(taskId) ?: return
        when (snapshot.state) {
            AudioDownloadTaskState.FAILED,
            AudioDownloadTaskState.CANCELLED,
            AudioDownloadTaskState.COMPLETED -> downloadManager.remove(taskId)
            AudioDownloadTaskState.DOWNLOADING,
            AudioDownloadTaskState.PAUSED -> Unit
        }
    }

    private fun handleDownloadCompleted(snapshot: AudioDownloadTaskSnapshot) {
        showDownloadNotification(
            title = "Download abgeschlossen",
            message = "\"${snapshot.audioName}\" wurde heruntergeladen.",
            type = MessageType.INFO
        )
        markAudioAsSeen(snapshot)
    }

    private fun handleDownloadFailed(snapshot: AudioDownloadTaskSnapshot) {
        showDownloadNotification(
            title = "Download fehlgeschlagen",
            message = buildString {
                append("„")
                append(snapshot.audioName)
                append("“ konnte nicht heruntergeladen werden.")
                snapshot.errorMessage
                    ?.takeIf { it.isNotBlank() }
                    ?.let {
                        append(' ')
                        append(it)
                    }
            },
            type = MessageType.ERROR
        )
    }

    private fun showDownloadNotification(title: String, message: String, type: MessageType) {
        NotificationService.displayNotification(
            NotificationMessage().apply {
                this.title = title
                this.message = message
                this.type = type
            }
        )
    }

    private fun markAudioAsSeen(snapshot: AudioDownloadTaskSnapshot) {
        try {
            SeenHistoryController().use {
                it.markSeen(snapshot.toAudioEntry())
            }
            SwingUtilities.invokeLater { table.refreshSeenState() }
        } catch (ex: Exception) {
            logger.warn("Failed to mark downloaded audio as seen: {}", snapshot.audioUrl, ex)
        }
    }

    private fun openExternal(url: URI) {
        runCatching {
            if (!SystemUtils.IS_OS_MAC_OSX) {
                try {
                    val vlcPath = GuiFunktionenProgramme.findExecutableOnPath("vlc")
                    ProcessBuilder(vlcPath.toAbsolutePath().toString(), url.toString()).start()
                } catch (_: IllegalStateException) {
                    JOptionPane.showMessageDialog(
                        this,
                        "<html>Es konnte kein VLC auf dem System gefunden werden.<br/>" +
                            "Es wird versucht, die Datei über den Browser zu öffnen.</html>",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.INFORMATION_MESSAGE
                    )
                    UrlHyperlinkAction.openURI(url)
                }
            } else {
                MacMultimediaPlayerLocator.findIinaPlayer().ifPresentOrElse({
                    iinaPlayer.play(url.toString())
                }, {
                    MacMultimediaPlayerLocator.findVlcPlayer().ifPresentOrElse({
                        ProcessBuilder("open", "-a", "VLC", url.toString()).start()
                    }, {
                        Desktop.getDesktop().browse(url)
                    })
                })
            }
        }.onFailure {
            JOptionPane.showMessageDialog(
                this,
                "URL konnte nicht geöffnet werden:\n$url",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE
            )
        }
    }

    private fun showReloadMessage(result: AudioLoadResult) {
        val message = result.reloadMessage() ?: return
        JOptionPane.showMessageDialog(
            this,
            message,
            "Audiothek",
            JOptionPane.INFORMATION_MESSAGE
        )
    }

    companion object {
        private val DATASET_TIMESTAMP_FORMAT: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy HH:mm")
    }
}

private fun AudioDownloadTaskSnapshot.toAudioEntry(): AudioEntry {
    return AudioEntry(
        channel = channel,
        genre = "",
        theme = theme,
        title = audioName,
        durationMinutes = null,
        sizeMb = null,
        description = "",
        audioUrl = audioUrl.takeIf(String::isNotBlank)?.let(URI::create),
        websiteUrl = null,
        isNew = false,
        isPodcast = false,
        isDuplicate = false,
        publishedAt = null
    )
}
