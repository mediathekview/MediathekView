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

package mediathek.gui.tabs.tab_downloads

import com.github.benmanes.caffeine.cache.Caffeine
import mediathek.config.DatenConfigurationPersistence
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.DownloadColumn
import mediathek.controller.history.AboHistoryEntry
import mediathek.controller.starter.*
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.daten.abo.AboServices
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmListLoadCoordinator
import mediathek.filmlisten.FilmListLoadListener
import mediathek.filmlisten.FilmListLoadProgress
import mediathek.gui.actions.*
import mediathek.gui.dialog.DialogBeendenZeit
import mediathek.gui.dialog.DialogFilmBeschreibung
import mediathek.gui.dialog.edit_download.DialogEditDownload
import mediathek.gui.messages.*
import mediathek.gui.tabs.DescriptionTabController
import mediathek.gui.tabs.actions.MarkFilmAsSeenAction
import mediathek.gui.tabs.actions.MarkFilmAsUnseenAction
import mediathek.tool.DirOpenAction
import mediathek.tool.DownloadSizeState
import mediathek.tool.MessageBus
import mediathek.tool.ReplacementRules
import mediathek.tool.cellrenderer.CellRendererDownloads
import mediathek.tool.datum.Datum
import mediathek.tool.listener.BeobTableHeader
import mediathek.tool.models.TModelDownload
import mediathek.tool.table.MVDownloadsTable
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import org.pushingpixels.radiance.swing.ktx.addDelayedComponentListener
import java.awt.*
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import java.io.File
import java.util.*
import java.util.concurrent.atomic.AtomicLong
import java.util.function.BiConsumer
import java.util.function.Consumer
import java.util.function.LongConsumer
import java.util.function.Predicate
import javax.swing.*
import javax.swing.Timer
import kotlin.time.Duration
import kotlin.time.Duration.Companion.seconds
import kotlin.time.toJavaDuration

class GuiDownloads(
    private val programSets: ProgramSetRepository,
    private val filmCatalog: FilmCatalog,
    private val abos: AboServices,
    private val downloads: DownloadServices,
    private val replacementRules: ReplacementRules,
    private val filmListLoader: FilmListLoadCoordinator,
    private val configurationPersistence: DatenConfigurationPersistence,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
    private val ownerFrame: JFrame,
    private val showFilmInformationAction: Action,
    private val selectedListItemsCount: LongConsumer,
    private val selectedFilm: Consumer<DatenFilm?>,
    private val quitApplication: Predicate<Boolean>,
) : JPanel() {
    val startAllDownloadsAction = StartAllDownloadsAction(this)
    val startAllDownloadsTimedAction = StartAllDownloadsTimedAction(this)
    val stopAllDownloadsAction = StopAllDownloadsAction(this)
    val stopAllWaitingDownloadsAction = StopAllWaitingDownloadsAction(this)
    val refreshDownloadListAction = RefreshDownloadListAction(this)
    val cleanupDownloadListAction = CleanupDownloadListAction(this)
    val playDownloadAction = PlayDownloadAction(this)
    val stopDownloadsAction = StopDownloadsAction(this)
    val startDownloadsAction = StartDownloadsAction(this)
    val deferDownloadsAction = DeferDownloadsAction(this)
    val advanceDownloadsAction = AdvanceDownloadsAction(this)
    val deleteDownloadsAction = DeleteDownloadsAction(this)
    val editDownloadAction = EditDownloadAction(this)
    val deleteDownloadAction = DeleteDownloadAction(this)
    val openTargetFolderAction = OpenTargetFolderAction(this)
    val mergeSubtitleWithVideoAction = MergeSubtitleWithVideoAction(ownerFrame)
    val swingToolBar: JToolBar = DownloadsToolBar(
        refreshDownloadListAction,
        startAllDownloadsAction,
        playDownloadAction,
        deferDownloadsAction,
        deleteDownloadsAction,
        cleanupDownloadListAction,
    )

    private val configToolBar: JToolBar = DownloadsConfigToolBar()
    private val displayFilterToolBar = DownloadsDisplayFilterToolBar()
    private val toolBarRow = DownloadsToolBarRow(swingToolBar, displayFilterToolBar, configToolBar)
    private val lastUpdate = AtomicLong(0)
    private val cbShowDownloadDescription = JCheckBoxMenuItem("Filmbeschreibung anzeigen")
    private val descriptionTabController = DescriptionTabController({ ownerFrame }, ::editFilmDescription)
    private val markFilmAsSeenAction = MarkFilmAsSeenAction(::getSelFilme)
    private val markFilmAsUnseenAction = MarkFilmAsUnseenAction(::getSelFilme)
    private val filterController = DownloadsFilterController(displayFilterToolBar, ::reloadTable)
    private val startInfoProperty = DownloadStartInfoProperty(downloads)
    private val statusBar = DownloadsStatusBar(startInfoProperty)
    private val downloadSizeCacheSnapshot = DownloadSizeCacheStorage.load()
    private val downloadSizeLookupService = DownloadSizeLookupService(
        reloadTable = ::reloadAndSave,
        persistedLookupResults = downloadSizeCacheSnapshot.lookupResults,
    )
    private val knownAboSizes = Caffeine.newBuilder()
        .maximumSize(DownloadSizeCachePolicy.MAXIMUM_ENTRIES.toLong())
        .expireAfterWrite(DownloadSizeCachePolicy.maximumEntryAge.toJavaDuration())
        .build<String, CachedAboSize>()
        .apply {
            downloadSizeCacheSnapshot.knownAboSizes.forEach { entry ->
                put(
                    entry.key,
                    CachedAboSize(
                        byteLength = entry.byteLength,
                        storedAtMillis = entry.storedAtMillis,
                    ),
                )
            }
        }

    private var loadFilmlist = false
    private lateinit var model: TModelDownload
    private lateinit var tabelle: MVDownloadsTable
    private lateinit var tableSelection: DownloadsTableSelection
    private lateinit var downloadListScrollPane: JScrollPane

    init {
        initComponents()
        setupDownloadListTable()
        setupShowFilmDescriptionMenuItem()
        descriptionTabController.install(
            tabelle,
            cbShowDownloadDescription,
            { ApplicationConfiguration.getInstance().showDownloadDescription },
            ::getCurrentlySelectedFilm,
        )

        init()
        setupFilmSelectionPropertyListener()
        setupDownloadSizeSelectionUpdater()
        initTable()
        addListenerMediathekView()
        filterController.install()

        if (Taskbar.isTaskbarSupported()) {
            setupTaskbarMenu()
        }

        tabelle.tableHeader.reorderingAllowed = false
    }

    fun tabelleSpeichern() {
        if (::tabelle.isInitialized) {
            tabelle.writeTableConfigurationData()
        }
        DownloadSizeCacheStorage.save(
            DownloadSizeCacheSnapshot(
                lookupResults = downloadSizeLookupService.snapshotLookupResults(),
                knownAboSizes = snapshotKnownAboSizes(),
            )
        )
    }

    private fun getSelectedDownloadsFromTable(): List<DatenDownload> = tableSelection.selectedDownloadsForLookup()

    private fun editFilmDescription(film: DatenFilm) {
        DialogFilmBeschreibung(ownerFrame, programSets, film, replacementRules).isVisible = true
    }

    private fun setupDownloadSizeSelectionUpdater() {
        tabelle.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                downloadSizeLookupService.updateFilmSizes(getSelectedDownloadsFromTable())
            }
        }
    }

    private fun setupFilmSelectionPropertyListener() {
        tabelle.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                updateSelectedListItemsCount(tabelle)
            }
        }
        addDelayedComponentListener(
            onComponentShown = {
                updateSelectedListItemsCount(tabelle)
                onComponentShown()
            }
        )
    }

    private fun setupDownloadListTable() {
        tabelle = MVDownloadsTable(downloads)
        tableSelection = DownloadsTableSelection(tabelle, this)
        downloadListScrollPane.viewport.view = tabelle
    }

    private fun initTable() {
        tabelle.readColumnConfigurationData()
        tabelle.setSpalten()
        if (tabelle.rowCount > 0) {
            tabelle.setRowSelectionInterval(0, 0)
        }
    }

    private fun setupTaskbarMenu() {
        val taskbar = Taskbar.getTaskbar()
        if (taskbar.isSupported(Taskbar.Feature.MENU)) {
            val popupMenu = taskbar.menu ?: PopupMenu()
            MenuItem("Alle Downloads starten").apply {
                addActionListener { starten(true) }
                popupMenu.add(this)
            }
            MenuItem("Alle Downloads stoppen").apply {
                addActionListener { stoppen(true) }
                popupMenu.add(this)
            }
            taskbar.menu = popupMenu
        }
    }

    fun installMenuEntries(menu: JMenu) {
        menu.add(startAllDownloadsAction)
        menu.add(startAllDownloadsTimedAction)
        menu.add(stopAllDownloadsAction)
        menu.add(stopAllWaitingDownloadsAction)
        menu.add(refreshDownloadListAction)
        menu.add(cleanupDownloadListAction)
        menu.addSeparator()
        menu.add(startDownloadsAction)
        menu.add(stopDownloadsAction)
        menu.add(advanceDownloadsAction)
        menu.add(deferDownloadsAction)
        menu.add(deleteDownloadsAction)
        menu.add(editDownloadAction)
        menu.addSeparator()
        menu.add(mergeSubtitleWithVideoAction)
        menu.addSeparator()
        menu.add(cbShowDownloadDescription)
        menu.addSeparator()
        menu.add(markFilmAsSeenAction)
        menu.add(markFilmAsUnseenAction)
        menu.add(playDownloadAction)
    }

    fun onComponentShown() {
        updateFilmData()
        updateUnknownDownloadSizes()
    }

    private fun updateSelectedListItemsCount(table: JTable) {
        selectedListItemsCount.accept(table.selectedRowCount.toLong())
    }

    private fun updateStartInfoProperty() {
        MessageBus.messageBus.publishAsync(UpdateStatusBarLeftDisplayEvent())
    }

    fun starten(alle: Boolean) {
        filmStartenWiederholenStoppen(alle, starten = true, restartFinishedDownloads = true, skipManualDownloads = false)
    }

    fun stoppen(alle: Boolean) {
        filmStartenWiederholenStoppen(alle, starten = false, restartFinishedDownloads = true, skipManualDownloads = false)
    }

    private fun setupKeyMappings() {
        val inputMap = tabelle.inputMap
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_EDIT_DOWNLOAD)
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), ACTION_MAP_KEY_DELETE_DOWNLOAD)
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_G, 0), ACTION_MAP_KEY_MARK_AS_SEEN)
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_U, 0), ACTION_MAP_KEY_MARK_AS_UNSEEN)
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_D, 0), ACTION_MAP_KEY_START_DOWNLOAD)

        val actionMap = tabelle.actionMap
        actionMap.put(ACTION_MAP_KEY_EDIT_DOWNLOAD, object : AbstractAction() {
            override fun actionPerformed(event: ActionEvent?) {
                editDownload()
            }
        })
        actionMap.put(ACTION_MAP_KEY_DELETE_DOWNLOAD, object : AbstractAction() {
            override fun actionPerformed(event: ActionEvent?) {
                downloadLoeschen(true)
            }
        })
        actionMap.put(ACTION_MAP_KEY_MARK_AS_SEEN, markFilmAsSeenAction)
        actionMap.put(ACTION_MAP_KEY_MARK_AS_UNSEEN, markFilmAsUnseenAction)
        actionMap.put(ACTION_MAP_KEY_START_DOWNLOAD, object : AbstractAction() {
            override fun actionPerformed(event: ActionEvent?) {
                filmStartenWiederholenStoppen(false, starten = true, restartFinishedDownloads = true, skipManualDownloads = false)
            }
        })
    }

    private fun init() {
        setupKeyMappings()

        val cellRenderer = CellRendererDownloads()
        tabelle.setDefaultRenderer(Any::class.java, cellRenderer)
        tabelle.setDefaultRenderer(Datum::class.java, cellRenderer)
        tabelle.setDefaultRenderer(DownloadSizeState::class.java, cellRenderer)
        tabelle.setDefaultRenderer(Int::class.javaObjectType, cellRenderer)

        model = TModelDownload()
        tabelle.model = model
        tabelle.addMouseListener(
            DownloadsTableMouseHandler(
                this,
                tabelle,
                programSets,
                filmCatalog,
                abos,
                downloads,
                programSetExporter,
                ownerFrame,
                showFilmInformationAction,
            )
        )
        tabelle.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                updateFilmData()
            }
        }

        tabelle.setLineBreak(ApplicationConfiguration.getInstance().downloadTableLineBreak)
        tabelle.tableHeader.addMouseListener(
            BeobTableHeader(
                tabelle,
                DownloadColumn.visibilityStore(),
                COLUMNS_DISABLED,
                intArrayOf(DownloadColumn.BUTTON_START.index, DownloadColumn.BUTTON_DELETE.index),
                true
            ) {
                ApplicationConfiguration.getInstance().downloadTableLineBreak = it
            }
        )
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleRestartDownloadEvent(event: RestartDownloadEvent) {
        reloadAndSave()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleDownloadQueueRankChanged(event: DownloadQueueRankChangedEvent) {
        reloadAndSave()
    }

    private fun reloadAndSave() {
        SwingUtilities.invokeLater {
            reloadTable()
            configurationPersistence.saveAll()
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleAboListChanged(event: AboListChangedEvent) {
        SwingUtilities.invokeLater {
            if (ApplicationConfiguration.getInstance().searchAbosImmediately) {
                updateDownloads()
            }
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleDownloadListChange(event: DownloadListChangedEvent) {
        SwingUtilities.invokeLater {
            reloadTable()
            configurationPersistence.saveAll()
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleBlacklistChangedEvent(event: BlacklistChangedEvent) {
        SwingUtilities.invokeLater {
            if (ApplicationConfiguration.getInstance().searchAbosImmediately &&
                ApplicationConfiguration.getInstance().blacklistApplyToAbo
            ) {
                updateDownloads()
            }
        }
    }

    private fun addListenerMediathekView() {
        MessageBus.messageBus.subscribe(this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleBlacklistAboSettingChangedEvent(event: BlacklistAboSettingChangedEvent) {
        SwingUtilities.invokeLater {
            if (ApplicationConfiguration.getInstance().searchAbosImmediately) {
                updateDownloads()
            }
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleDownloadProgressChanged(event: DownloadProgressChangedEvent) {
        val now = System.currentTimeMillis()
        if (now - lastUpdate.get() >= 500) {
            lastUpdate.set(now)
            SwingUtilities.invokeLater {
                downloads.updateTableModelProgress(model)
            }
        }
    }

    @Handler
    private fun handleDownloadFinishedEvent(event: DownloadFinishedEvent) {
        evictFinishedDownloadSize(event.download)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleGeoStateChangedEvent(event: GeoStateChangedEvent) {
        SwingUtilities.invokeLater {
            tabelle.fireTableDataChanged(true)
            updateStartInfoProperty()
        }
    }

    private fun setupShowFilmDescriptionMenuItem() {
        cbShowDownloadDescription.isSelected = ApplicationConfiguration.getInstance().showDownloadDescription
        cbShowDownloadDescription.addActionListener {
            val visible = cbShowDownloadDescription.isSelected
            descriptionTabController.setVisible(visible)
            ApplicationConfiguration.getInstance().showDownloadDescription = visible
        }
    }

    @Synchronized
    private fun reloadTable() {
        tabelle.getSpalten()

        val displayFilter = filterController.displayFilter
        val viewFilter = filterController.viewFilter
        downloads.reloadTableModel(
            model,
            DownloadListFilter(
                onlyAbos = displayFilter.onlyAbos(),
                onlyDownloads = displayFilter.onlyDownloads(),
                onlyNotStarted = viewFilter.onlyNotStarted(),
                onlyStarted = viewFilter.onlyStarted(),
                onlyWaiting = viewFilter.onlyWaiting(),
                onlyRun = viewFilter.onlyRun(),
                onlyFinished = viewFilter.onlyFinished(),
            ),
        )
        tabelle.setSpalten()
        updateFilmData()
        updateStartInfoProperty()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleStartEvent(event: StartEvent) {
        SwingUtilities.invokeLater(::reloadTable)
    }

    @Synchronized
    fun updateDownloads() {
        if (loadFilmlist) {
            return
        }

        rememberAboSizes(downloads.queuedDownloads())
        downloads.refreshAboDownloads()
        downloads.searchAboDownloads(ownerFrame)
        val updatedDownloads = downloads.queuedDownloads()
        updatedDownloads.restoreKnownAboSizes()
        rememberAboSizes(updatedDownloads)
        reloadTable()
        updateUnknownDownloadSizes()

        if (ApplicationConfiguration.getInstance().startDownloadsImmediately) {
            filmStartenWiederholenStoppen(true, starten = true, restartFinishedDownloads = false, skipManualDownloads = true)
        }
    }

    private fun rememberAboSizes(downloads: Iterable<DatenDownload>) {
        for (download in downloads) {
            if (download.isFromAbo && download.runtime.filmSize.size > 0L) {
                rememberAboSizeKeys(download)
            }
        }
    }

    private fun rememberAboSizeKeys(download: DatenDownload) {
        val size = download.runtime.filmSize.size
        val storedAtMillis = System.currentTimeMillis()
        download.sizeMemoryKeys().forEach { key ->
            knownAboSizes.put(key, CachedAboSize(size, storedAtMillis))
        }
    }

    private fun evictFinishedDownloadSize(download: DatenDownload) {
        if (download.runtime.runState?.status != StartStatus.FINISHED) {
            return
        }

        evictDownloadSizeCache(download)
    }

    private fun evictDownloadSizeCache(download: DatenDownload) {
        downloadSizeLookupService.invalidate(download)
        forgetAboSizeKeys(download)
    }

    private fun forgetAboSizeKeys(download: DatenDownload) {
        download.sizeMemoryKeys().forEach(knownAboSizes::invalidate)
    }

    private fun Iterable<DatenDownload>.restoreKnownAboSizes() {
        for (download in this) {
            if (download.runtime.filmSize.size == 0L) {
                download.findKnownAboSize()?.let { knownSize ->
                    download.runtime.filmSize.size = knownSize
                }
            }
        }
    }

    private fun DatenDownload.findKnownAboSize(): Long? =
        sizeMemoryKeys()
            .firstNotNullOfOrNull { key -> knownAboSizes.getIfPresent(key)?.byteLength }

    private fun DatenDownload.sizeMemoryKeys(): Sequence<String> =
        sequenceOf(downloadUrl, historyUrl, filmUrl)
            .filter(String::isNotBlank)

    private fun snapshotKnownAboSizes(): List<PersistentKnownAboSize> =
        knownAboSizes.asMap().map { (key, value) ->
            PersistentKnownAboSize(
                key = key,
                byteLength = value.byteLength,
                storedAtMillis = value.storedAtMillis,
            )
        }

    private fun updateUnknownDownloadSizes() {
        downloadSizeLookupService.updateFilmSizes(downloads.queuedDownloads())
    }

    @Synchronized
    fun cleanupDownloads() {
        downloads.cleanupFinishedDownloads()
    }

    @Synchronized
    fun downloadsAufraeumen(datenDownload: DatenDownload) {
        downloads.cleanupFinishedDownload(datenDownload)
    }

    private fun getSelDownloads(): ArrayList<DatenDownload> = tableSelection.selectedDownloadsOrShowError()

    fun getCurrentlySelectedFilm(): Optional<DatenFilm> = tableSelection.currentlySelectedFilm()

    private fun getSelDownload(): DatenDownload? = tableSelection.selectedDownloadOrShowError()

    @Synchronized
    fun editDownload() {
        val datenDownload = getSelDownload() ?: return
        val gestartet = datenDownload.runtime.runState?.let { it.status >= StartStatus.RUNNING } == true
        val datenDownloadCopy = datenDownload.copy
        val dialog = DialogEditDownload(ownerFrame, datenDownloadCopy, gestartet)
        dialog.isVisible = true
        if (dialog.isConfirmed()) {
            datenDownload.aufMichKopieren(datenDownloadCopy)
            reloadTable()
        }
    }

    fun downloadsVorziehen() {
        val downloads = getSelDownloads()
        if (downloads.isEmpty()) {
            return
        }
        this.downloads.advanceDownloads(downloads)
    }

    fun zielordnerOeffnen() {
        val datenDownload = getSelDownload() ?: return
        val targetPath = datenDownload.targetPath
        DirOpenAction.zielordnerOeffnen(ownerFrame, targetPath)
    }

    fun filmAbspielen() {
        val datenDownload = getSelDownload() ?: return
        val targetFile = datenDownload.targetPathFileName
        OpenPlayerAction.filmAbspielen(ownerFrame, targetFile)
    }

    fun deleteDownloadedFile() {
        val datenDownload = getSelDownload() ?: return

        val currentStart = datenDownload.runtime.runState
        if (currentStart != null && currentStart.status < StartStatus.FINISHED) {
            JOptionPane.showMessageDialog(ownerFrame, "Download erst stoppen!", "Film löschen", JOptionPane.ERROR_MESSAGE)
            return
        }

        try {
            val file = getExistingDownloadFile(datenDownload)
            if (!file.exists()) {
                JOptionPane.showMessageDialog(ownerFrame, "Die Datei existiert nicht!", "Film löschen", JOptionPane.ERROR_MESSAGE)
                return
            }

            val result = JOptionPane.showConfirmDialog(
                ownerFrame,
                file.absolutePath,
                "Film Löschen?",
                JOptionPane.YES_NO_OPTION,
            )
            if (result == JOptionPane.OK_OPTION) {
                logger.info("Datei löschen: {}", file.absolutePath)
                if (!file.delete()) {
                    throw IllegalStateException("Could not delete ${file.absolutePath}")
                }
            }
        } catch (_: Exception) {
            JOptionPane.showMessageDialog(ownerFrame, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE)
            logger.error("Fehler beim löschen: {}", datenDownload.targetPathFileName)
        }
    }

    private fun getExistingDownloadFile(datenDownload: DatenDownload): File {
        val finalFile = File(datenDownload.targetPathFileName)
        if (finalFile.exists()) {
            return finalFile
        }

        val partFile = DirectDownloadPartFiles.partFileFor(finalFile)
        return if (partFile.exists()) partFile else finalFile
    }

    fun downloadLoeschen(permanentDeletion: Boolean) {
        try {
            val rowToSelectAfterDeletion = tabelle.selectedRow
            val downloads = getSelDownloads()
            if (downloads.isEmpty()) {
                return
            }

            val downloadsToDelete = ArrayList<DatenDownload>()
            val aboUrls = mutableListOf<AboHistoryEntry>()

            for (datenDownload in downloads) {
                if (permanentDeletion) {
                    downloadsToDelete.add(datenDownload)
                    if (datenDownload.isFromAbo) {
                        aboUrls.add(
                            AboHistoryEntry.today(
                                datenDownload.topic,
                                datenDownload.title,
                                datenDownload.historyUrl,
                            )
                        )
                    }
                } else {
                    DownloadLifecycleActions.defer(datenDownload)
                }
            }

            if (aboUrls.isNotEmpty()) {
                abos.historyController.add(aboUrls)
            }

            downloadsToDelete.forEach(::evictDownloadSizeCache)
            this.downloads.deleteDownloads(downloadsToDelete)
            reloadTable()
            selectSingleRowAfterDeletion(rowToSelectAfterDeletion)
        } catch (ex: Exception) {
            logger.error("downloadLoeschen()", ex)
        }
    }

    private fun selectSingleRowAfterDeletion(rowToSelect: Int) {
        val rowCount = tabelle.rowCount
        if (rowCount == 0) {
            tabelle.clearSelection()
            return
        }

        val validRow = rowToSelect.coerceIn(0, rowCount - 1)
        tabelle.setRowSelectionInterval(validRow, validRow)
    }

    private fun addAllDownloadsToList(): List<DatenDownload> {
        val rowCount = tabelle.rowCount
        val tableModel = tabelle.model
        val downloads = ArrayList<DatenDownload>()

        for (i in 0 until rowCount) {
            val datenDownload = tableModel.getValueAt(
                tabelle.convertRowIndexToModel(i),
                DownloadColumn.REF.index,
            ) as DatenDownload
            downloads.add(datenDownload)
        }
        return downloads
    }

    fun startAllDownloadsAtSpecificTime() {
        if (tabelle.rowCount == 0) {
            JOptionPane.showMessageDialog(
                this,
                "Es sind keine Downloads in der Liste zum Starten vorhanden.",
                Konstanten.PROGRAMMNAME,
                JOptionPane.INFORMATION_MESSAGE,
            )
            return
        }

        tabelle.sortDownloadListByTableRows()
        val allDownloads = addAllDownloadsToList()
        val downloadsToCancel = ArrayList<DatenDownload>()
        val downloadsToStart = ArrayList<DatenDownload>()

        for (download in allDownloads) {
            val start = download.runtime.runState
            if (start != null) {
                if (start.status == StartStatus.RUNNING) {
                    continue
                }
                if (start.status > StartStatus.RUNNING) {
                    val reply = createDismissableMessageDialog(
                        ownerFrame,
                        "Fertiger Download",
                        "Film nochmal starten?  ==> " + download.title,
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.NO_OPTION,
                        10.seconds,
                        JOptionPane.QUESTION_MESSAGE,
                    )
                    if (reply != JOptionPane.YES_OPTION) {
                        continue
                    }
                    downloadsToCancel.add(download)
                    if (download.isFromAbo) {
                        abos.historyController.removeUrl(download.historyUrl)
                    }
                }
            }
            downloadsToStart.add(download)
        }

        downloads.cancelDownloads(downloadsToCancel)

        val dialogBeenden = DialogBeendenZeit(ownerFrame, downloads, downloadsToStart)
        dialogBeenden.isVisible = true
        if (dialogBeenden.applicationCanTerminate()) {
            quitApplication.test(dialogBeenden.isShutdownRequested())
        }

        reloadTable()
    }

    fun filmStartenWiederholenStoppen(
        processAllDownloads: Boolean,
        starten: Boolean,
        restartFinishedDownloads: Boolean,
        skipManualDownloads: Boolean,
    ) {
        val downloadsToCancel = ArrayList<DatenDownload>()
        val downloadsToStart = ArrayList<DatenDownload>()

        if (tabelle.rowCount == 0) {
            return
        }

        if (starten && processAllDownloads) {
            tabelle.sortDownloadListByTableRows()
        }

        val selectedDownloads = if (processAllDownloads) addAllDownloadsToList() else getSelDownloads()

        if (!starten) {
            downloads.delayNewStarts()
        }

        var answer = -1
        for (download in selectedDownloads) {
            val start = download.runtime.runState
            if (starten) {
                if (start != null) {
                    if (start.status == StartStatus.RUNNING ||
                        !restartFinishedDownloads && start.status > StartStatus.RUNNING
                    ) {
                        continue
                    }
                    if (start.status > StartStatus.RUNNING) {
                        if (answer == -1) {
                            val text = if (selectedDownloads.size > 1) {
                                "Es sind bereits fertige Filme dabei,\n" +
                                    "diese nochmal starten?"
                            } else {
                                "Film nochmal starten?  ==> " + download.title
                            }
                            answer = createDismissableMessageDialog(
                                ownerFrame,
                                "Fertiger Download",
                                text,
                                JOptionPane.YES_NO_CANCEL_OPTION,
                                JOptionPane.NO_OPTION,
                                10.seconds,
                                JOptionPane.QUESTION_MESSAGE,
                            )
                        }
                        if (answer == JOptionPane.CANCEL_OPTION) {
                            return
                        }
                        if (answer == JOptionPane.NO_OPTION) {
                            continue
                        }
                        downloadsToCancel.add(download)
                        if (download.isFromAbo) {
                            abos.historyController.removeUrl(download.historyUrl)
                        }
                    }
                }
                downloadsToStart.add(download)
            } else if (start != null && start.status <= StartStatus.RUNNING) {
                downloadsToCancel.add(download)
            }
        }

        downloads.cancelDownloads(downloadsToCancel)

        if (skipManualDownloads) {
            downloadsToStart.removeIf { download -> !download.isFromAbo || download.isAutomaticStartBlockedByAbo }
        }

        if (starten) {
            DownloadStartActions.startAll(downloadsToStart)
        }

        reloadTable()
    }

    fun stopAllWaitingDownloads() {
        val downloadsToStop = ArrayList<DatenDownload>()
        for (i in 0 until tabelle.rowCount) {
            val datenDownload = tabelle.model.getValueAt(
                tabelle.convertRowIndexToModel(i),
                DownloadColumn.REF.index,
            ) as DatenDownload
            val start = datenDownload.runtime.runState
            if (start != null && start.status < StartStatus.RUNNING) {
                downloadsToStop.add(datenDownload)
            }
        }
        downloads.cancelDownloads(downloadsToStop)
    }

    private fun updateFilmData() {
        if (!isShowing) {
            return
        }

        selectedFilm.accept(getCurrentlySelectedFilm().orElse(null))
    }

    private fun createDismissableMessageDialog(
        parentComponent: Component?,
        title: String,
        message: String,
        optionType: Int,
        defaultValue: Int,
        defaultDelay: Duration,
        style: Int,
    ): Int {
        val optionPane = JOptionPane(message, style, optionType, null, null)
        val dialog = optionPane.createDialog(parentComponent, title)
        Timer(defaultDelay.inWholeMilliseconds.toInt()) {
            optionPane.value = defaultValue
        }.start()
        dialog.isVisible = true
        return optionPane.value as Int
    }

    fun getSelFilme(): List<DatenFilm> = tableSelection.selectedFilmsOrShowError()

    private fun initComponents() {
        val downloadListArea = JPanel()
        downloadListScrollPane = JScrollPane()

        layout = BorderLayout()

        downloadListArea.layout = BorderLayout()
        val tempPanel = JPanel(BorderLayout())
        tempPanel.add(downloadListScrollPane, BorderLayout.CENTER)
        tempPanel.add(statusBar, BorderLayout.SOUTH)
        downloadListArea.add(tempPanel, BorderLayout.CENTER)
        downloadListArea.add(descriptionTabController.tabbedPane, BorderLayout.SOUTH)

        add(downloadListArea, BorderLayout.CENTER)
        add(toolBarRow, BorderLayout.NORTH)

        filmListLoader.addLoadListener(object : FilmListLoadListener {
            override fun loadStarted(@Suppress("UNUSED_PARAMETER") progress: FilmListLoadProgress) {
                loadFilmlist = true
                SwingUtilities.invokeLater {
                    refreshDownloadListAction.isEnabled = false
                }
            }

            override fun loadFinished(@Suppress("UNUSED_PARAMETER") progress: FilmListLoadProgress) {
                loadFilmlist = false
                SwingUtilities.invokeLater {
                    refreshDownloadListAction.isEnabled = true
                }
                downloads.reconnectFilms()
                if (ApplicationConfiguration.getInstance().searchAbosImmediately) {
                    updateDownloads()
                } else {
                    reloadTable()
                }
            }
        })
    }

    private data class CachedAboSize(
        val byteLength: Long,
        val storedAtMillis: Long,
    )

    companion object {
        const val NAME = "Downloads"
        private const val ACTION_MAP_KEY_EDIT_DOWNLOAD = "dl_aendern"
        private const val ACTION_MAP_KEY_DELETE_DOWNLOAD = "dl_delete"
        private const val ACTION_MAP_KEY_MARK_AS_SEEN = "seen"
        private const val ACTION_MAP_KEY_MARK_AS_UNSEEN = "unseen"
        private const val ACTION_MAP_KEY_START_DOWNLOAD = "dl_start"
        private val COLUMNS_DISABLED = intArrayOf(
            DownloadColumn.BUTTON_START.index,
            DownloadColumn.BUTTON_DELETE.index,
            DownloadColumn.REF.index,
            DownloadColumn.RTMP_URL.index,
        )
        private val logger = LogManager.getLogger(GuiDownloads::class.java)
    }
}
