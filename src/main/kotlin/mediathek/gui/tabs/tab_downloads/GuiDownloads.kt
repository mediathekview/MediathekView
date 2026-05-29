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

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.controller.history.MVUsedUrl
import mediathek.controller.starter.DirectDownloadPartFiles
import mediathek.controller.starter.Start
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.gui.actions.*
import mediathek.gui.dialog.DialogBeendenZeit
import mediathek.gui.dialog.edit_download.DialogEditDownload
import mediathek.gui.messages.*
import mediathek.gui.tabs.DescriptionTabController
import mediathek.gui.tabs.actions.MarkFilmAsSeenAction
import mediathek.gui.tabs.actions.MarkFilmAsUnseenAction
import mediathek.mainwindow.MediathekGui
import mediathek.tool.*
import mediathek.tool.cellrenderer.CellRendererDownloads
import mediathek.tool.datum.Datum
import mediathek.tool.listener.BeobTableHeader
import mediathek.tool.models.TModelDownload
import mediathek.tool.table.MVDownloadsTable
import net.engio.mbassy.listener.Handler
import org.apache.commons.configuration2.Configuration
import org.apache.logging.log4j.LogManager
import java.awt.BorderLayout
import java.awt.MenuItem
import java.awt.PopupMenu
import java.awt.Taskbar
import java.awt.event.ActionEvent
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import java.awt.event.KeyEvent
import java.io.File
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.Optional
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicLong
import javax.swing.*

class GuiDownloads(
    private val daten: Daten,
    private val mediathekGui: MediathekGui,
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
    val mergeSubtitleWithVideoAction = MergeSubtitleWithVideoAction(MediathekGui.ui())
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
    private val config: Configuration = ApplicationConfiguration.getConfiguration()
    private val descriptionTabController = DescriptionTabController()
    private val markFilmAsSeenAction = MarkFilmAsSeenAction(::getSelFilme)
    private val markFilmAsUnseenAction = MarkFilmAsUnseenAction(::getSelFilme)
    private val filterController = DownloadsFilterController(displayFilterToolBar, config, ::reloadTable)
    private val startInfoProperty = DownloadStartInfoProperty()
    private val statusBar = DownloadsStatusBar(startInfoProperty)
    private val downloadSizeLookupService = DownloadSizeLookupService(::reloadTable)

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
            ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION,
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
    }

    private fun getSelectedDownloadsFromTable(): List<DatenDownload> = tableSelection.selectedDownloadsForLookup()

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
        addComponentListener(object : ComponentAdapter() {
            override fun componentShown(event: ComponentEvent) {
                updateSelectedListItemsCount(tabelle)
                onComponentShown()
            }
        })
    }

    private fun setupDownloadListTable() {
        tabelle = MVDownloadsTable()
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
    }

    private fun updateSelectedListItemsCount(table: JTable) {
        mediathekGui.selectedListItemsProperty.setSelectedItems(table.selectedRowCount.toLong())
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
        tabelle.setDefaultRenderer(MVFilmSize::class.java, cellRenderer)
        tabelle.setDefaultRenderer(Int::class.javaObjectType, cellRenderer)

        model = TModelDownload()
        tabelle.model = model
        tabelle.addMouseListener(DownloadsTableMouseHandler(this, tabelle, daten, mediathekGui))
        tabelle.selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                updateFilmData()
            }
        }

        tabelle.setLineBreak(MVConfig.getBool(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_LINEBREAK))
        tabelle.tableHeader.addMouseListener(
            BeobTableHeader(
                tabelle,
                DatenDownload.getColumnVisibilityStore(),
                COLUMNS_DISABLED,
                intArrayOf(DatenDownload.DOWNLOAD_BUTTON_START, DatenDownload.DOWNLOAD_BUTTON_DEL),
                true,
                MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_LINEBREAK,
            )
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
            daten.allesSpeichern()
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleAboListChanged(event: AboListChangedEvent) {
        SwingUtilities.invokeLater {
            if (MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN).toBoolean()) {
                updateDownloads()
            }
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleDownloadListChange(event: DownloadListChangedEvent) {
        SwingUtilities.invokeLater {
            reloadTable()
            daten.allesSpeichern()
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleBlacklistChangedEvent(event: BlacklistChangedEvent) {
        SwingUtilities.invokeLater {
            if (MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN).toBoolean() &&
                MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO).toBoolean()
            ) {
                updateDownloads()
            }
        }
    }

    private fun addListenerMediathekView() {
        MessageBus.messageBus.subscribe(this)

        Listener.addListener(object : Listener(EREIGNIS_BLACKLIST_AUCH_FUER_ABOS, GuiDownloads::class.simpleName) {
            override fun ping() {
                if (MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN).toBoolean()) {
                    updateDownloads()
                }
            }
        })
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleDownloadProgressChanged(event: DownloadProgressChangedEvent) {
        val now = System.currentTimeMillis()
        if (now - lastUpdate.get() >= 500) {
            lastUpdate.set(now)
            SwingUtilities.invokeLater {
                daten.listeDownloads.setModelProgress(model)
            }
        }
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
        cbShowDownloadDescription.isSelected = config.getBoolean(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, true)
        cbShowDownloadDescription.addActionListener {
            val visible = cbShowDownloadDescription.isSelected
            descriptionTabController.setVisible(visible)
            config.setProperty(ApplicationConfiguration.DOWNLOAD_SHOW_DESCRIPTION, visible)
        }
    }

    @Synchronized
    private fun reloadTable() {
        tabelle.getSpalten()

        val displayFilter = filterController.displayFilter
        val viewFilter = filterController.viewFilter
        daten.listeDownloads.getModel(
            model,
            displayFilter.onlyAbos(),
            displayFilter.onlyDownloads(),
            viewFilter.onlyNotStarted(),
            viewFilter.onlyStarted(),
            viewFilter.onlyWaiting(),
            viewFilter.onlyRun(),
            viewFilter.onlyFinished(),
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

        val listeDownloads = daten.listeDownloads
        listeDownloads.abosAuffrischen()
        listeDownloads.abosSuchen(mediathekGui)
        reloadTable()

        if (MVConfig.get(MVConfig.Configs.SYSTEM_DOWNLOAD_SOFORT_STARTEN).toBoolean()) {
            filmStartenWiederholenStoppen(true, starten = true, restartFinishedDownloads = false, skipManualDownloads = true)
        }
    }

    @Synchronized
    fun cleanupDownloads() {
        daten.listeDownloads.listePutzen()
    }

    @Synchronized
    fun downloadsAufraeumen(datenDownload: DatenDownload) {
        daten.listeDownloads.listePutzen(datenDownload)
    }

    private fun getSelDownloads(): ArrayList<DatenDownload> = tableSelection.selectedDownloadsOrShowError()

    fun getCurrentlySelectedFilm(): Optional<DatenFilm> = tableSelection.currentlySelectedFilm()

    private fun getSelDownload(): DatenDownload? = tableSelection.selectedDownloadOrShowError()

    @Synchronized
    fun editDownload() {
        val datenDownload = getSelDownload() ?: return
        val gestartet = datenDownload.start?.let { it.status >= Start.STATUS_RUN } == true
        val datenDownloadCopy = datenDownload.copy
        val dialog = DialogEditDownload(mediathekGui, datenDownloadCopy, gestartet)
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
        daten.listeDownloads.downloadsVorziehen(downloads)
    }

    fun zielordnerOeffnen() {
        val datenDownload = getSelDownload() ?: return
        val targetPath = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD]
        DirOpenAction.zielordnerOeffnen(mediathekGui, targetPath)
    }

    fun filmAbspielen() {
        val datenDownload = getSelDownload() ?: return
        val targetFile = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME]
        OpenPlayerAction.filmAbspielen(mediathekGui, targetFile)
    }

    fun filmLoeschen_() {
        val datenDownload = getSelDownload() ?: return

        if (datenDownload.start != null && datenDownload.start.status < Start.STATUS_FERTIG) {
            JOptionPane.showMessageDialog(mediathekGui, "Download erst stoppen!", "Film löschen", JOptionPane.ERROR_MESSAGE)
            return
        }

        try {
            val file = getExistingDownloadFile(datenDownload)
            if (!file.exists()) {
                JOptionPane.showMessageDialog(mediathekGui, "Die Datei existiert nicht!", "Film löschen", JOptionPane.ERROR_MESSAGE)
                return
            }

            val result = JOptionPane.showConfirmDialog(
                mediathekGui,
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
            JOptionPane.showMessageDialog(mediathekGui, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE)
            logger.error("Fehler beim löschen: {}", datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
        }
    }

    private fun getExistingDownloadFile(datenDownload: DatenDownload): File {
        val finalFile = File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME])
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

            val date = DateTimeFormatter.ofPattern("dd.MM.yyyy")
                .format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()))
            val downloadsToDelete = ArrayList<DatenDownload>()
            val aboUrls = mutableListOf<MVUsedUrl>()

            for (datenDownload in downloads) {
                if (permanentDeletion) {
                    downloadsToDelete.add(datenDownload)
                    if (datenDownload.isFromAbo) {
                        aboUrls.add(
                            MVUsedUrl(
                                date,
                                datenDownload.arr[DatenDownload.DOWNLOAD_THEMA],
                                datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
                                datenDownload.arr[DatenDownload.DOWNLOAD_HISTORY_URL],
                            )
                        )
                    }
                } else {
                    datenDownload.zurueckstellen()
                }
            }

            if (aboUrls.isNotEmpty()) {
                daten.aboHistoryController.add(aboUrls)
            }

            daten.listeDownloads.downloadLoeschen(downloadsToDelete)
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
                DatenDownload.DOWNLOAD_REF,
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
            if (download.start != null) {
                if (download.start.status == Start.STATUS_RUN) {
                    continue
                }
                if (download.start.status > Start.STATUS_RUN) {
                    val reply = GuiFunktionen.createDismissableMessageDialog(
                        mediathekGui,
                        "Fertiger Download",
                        "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL],
                        JOptionPane.YES_NO_OPTION,
                        JOptionPane.NO_OPTION,
                        10,
                        TimeUnit.SECONDS,
                        JOptionPane.QUESTION_MESSAGE,
                    )
                    if (reply != JOptionPane.YES_OPTION) {
                        continue
                    }
                    downloadsToCancel.add(download)
                    if (download.isFromAbo) {
                        daten.aboHistoryController.removeUrl(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL])
                    }
                }
            }
            downloadsToStart.add(download)
        }

        daten.listeDownloads.downloadAbbrechen(downloadsToCancel)

        val dialogBeenden = DialogBeendenZeit(mediathekGui, downloadsToStart)
        dialogBeenden.isVisible = true
        if (dialogBeenden.applicationCanTerminate()) {
            mediathekGui.quitApplication(dialogBeenden.isShutdownRequested())
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
            daten.starterClass.delayNewStarts()
        }

        var answer = -1
        for (download in selectedDownloads) {
            if (starten) {
                if (download.start != null) {
                    if (download.start.status == Start.STATUS_RUN ||
                        !restartFinishedDownloads && download.start.status > Start.STATUS_RUN
                    ) {
                        continue
                    }
                    if (download.start.status > Start.STATUS_RUN) {
                        if (answer == -1) {
                            val text = if (selectedDownloads.size > 1) {
                                "Es sind bereits fertige Filme dabei,\n" +
                                    "diese nochmal starten?"
                            } else {
                                "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL]
                            }
                            answer = GuiFunktionen.createDismissableMessageDialog(
                                mediathekGui,
                                "Fertiger Download",
                                text,
                                JOptionPane.YES_NO_CANCEL_OPTION,
                                JOptionPane.NO_OPTION,
                                10,
                                TimeUnit.SECONDS,
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
                            daten.aboHistoryController.removeUrl(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL])
                        }
                    }
                }
                downloadsToStart.add(download)
            } else if (download.start != null && download.start.status <= Start.STATUS_RUN) {
                downloadsToCancel.add(download)
            }
        }

        daten.listeDownloads.downloadAbbrechen(downloadsToCancel)

        if (skipManualDownloads) {
            downloadsToStart.removeIf { download -> !download.isFromAbo || download.isAutomaticStartBlockedByAbo }
        }

        if (starten) {
            DatenDownload.startenDownloads(downloadsToStart)
        }

        reloadTable()
    }

    fun stopAllWaitingDownloads() {
        val downloadsToStop = ArrayList<DatenDownload>()
        for (i in 0 until tabelle.rowCount) {
            val datenDownload = tabelle.model.getValueAt(
                tabelle.convertRowIndexToModel(i),
                DatenDownload.DOWNLOAD_REF,
            ) as DatenDownload
            if (datenDownload.start != null && datenDownload.start.status < Start.STATUS_RUN) {
                downloadsToStop.add(datenDownload)
            }
        }
        daten.listeDownloads.downloadAbbrechen(downloadsToStop)
    }

    private fun updateFilmData() {
        if (!isShowing) {
            return
        }

        mediathekGui.filmInfoDialog?.updateCurrentFilm(getCurrentlySelectedFilm().orElse(null))
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

        daten.filmeLaden.addAdListener(object : ListenerFilmeLaden() {
            override fun start(event: ListenerFilmeLadenEvent) {
                loadFilmlist = true
                SwingUtilities.invokeLater {
                    refreshDownloadListAction.isEnabled = false
                }
            }

            override fun fertig(event: ListenerFilmeLadenEvent) {
                loadFilmlist = false
                SwingUtilities.invokeLater {
                    refreshDownloadListAction.isEnabled = true
                }
                daten.listeDownloads.filmEintragen()
                if (MVConfig.get(MVConfig.Configs.SYSTEM_ABOS_SOFORT_SUCHEN).toBoolean()) {
                    updateDownloads()
                } else {
                    reloadTable()
                }
            }
        })
    }

    companion object {
        const val NAME = "Downloads"
        private const val ACTION_MAP_KEY_EDIT_DOWNLOAD = "dl_aendern"
        private const val ACTION_MAP_KEY_DELETE_DOWNLOAD = "dl_delete"
        private const val ACTION_MAP_KEY_MARK_AS_SEEN = "seen"
        private const val ACTION_MAP_KEY_MARK_AS_UNSEEN = "unseen"
        private const val ACTION_MAP_KEY_START_DOWNLOAD = "dl_start"
        private val COLUMNS_DISABLED = intArrayOf(
            DatenDownload.DOWNLOAD_BUTTON_START,
            DatenDownload.DOWNLOAD_BUTTON_DEL,
            DatenDownload.DOWNLOAD_REF,
            DatenDownload.DOWNLOAD_URL_RTMP,
        )
        private val logger = LogManager.getLogger(GuiDownloads::class.java)
    }
}
