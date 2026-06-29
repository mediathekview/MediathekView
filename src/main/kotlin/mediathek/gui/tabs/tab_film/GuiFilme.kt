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

package mediathek.gui.tabs.tab_film

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.application.ApplicationConfiguration
import mediathek.config.application.FilterConfiguration
import mediathek.controller.starter.DownloadServices
import mediathek.daten.*
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmeLaden
import mediathek.gui.actions.DeleteBookmarksAction
import mediathek.gui.actions.ManageBookmarkAction
import mediathek.gui.actions.PlayFilmAction
import mediathek.gui.bookmark.BookmarkDialog
import mediathek.gui.bookmark.BookmarkServices
import mediathek.gui.dialog.DialogFilmBeschreibung
import mediathek.gui.dialog.add_download.DialogAddDownload
import mediathek.gui.messages.*
import mediathek.gui.messages.history.DownloadHistoryChangedEvent
import mediathek.gui.tabs.DescriptionTabController
import mediathek.gui.tabs.actions.MarkFilmAsSeenAction
import mediathek.gui.tabs.actions.MarkFilmAsUnseenAction
import mediathek.gui.tabs.tab_film.actions.*
import mediathek.gui.tabs.tab_film.bookmark.FilmBookmarkController
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialog
import mediathek.gui.tabs.tab_film.filter_selection.FilmFilterSelectionSynchronizer
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel
import mediathek.gui.tabs.tab_film.lifecycle.BookmarkStartupReloadCoordinator
import mediathek.gui.tabs.tab_film.lifecycle.FilmLifecycleController
import mediathek.gui.tabs.tab_film.lifecycle.FilmLifecycleHostAdapter
import mediathek.gui.tabs.tab_film.search.LuceneSearchField
import mediathek.gui.tabs.tab_film.search.RegularSearchField
import mediathek.gui.tabs.tab_film.search.SearchField
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import mediathek.gui.tabs.tab_film.selection.FilmSelectionController
import mediathek.gui.tabs.tab_film.selection.FilmSelectionHostAdapter
import mediathek.gui.tabs.tab_film.table.*
import mediathek.gui.tabs.tab_film.view.FilmViewController
import mediathek.mainwindow.FilmBookmarkHost
import mediathek.tool.MessageBus
import mediathek.tool.table.MVFilmTable
import net.engio.mbassy.listener.Handler
import org.jdesktop.swingx.VerticalLayout
import java.awt.BorderLayout
import java.util.*
import java.util.function.BiConsumer
import java.util.function.Consumer
import java.util.function.LongConsumer
import javax.swing.*
import kotlin.time.Duration.Companion.milliseconds

class GuiFilme(
    private val programSets: ProgramSetRepository,
    private val filmCatalog: FilmCatalog,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val bookmarks: BookmarkServices,
    private val downloads: DownloadServices,
    private val filmListLoader: FilmeLaden,
    private val programSetExporter: BiConsumer<Array<DatenPset>, String>,
    private val ownerFrame: JFrame,
    private val toggleBlacklistAction: Action,
    private val editBlacklistAction: Action,
    private val showFilmInformationAction: Action,
    private val showLuceneTutorialAction: Action,
    private val selectedListItemsCount: LongConsumer,
    private val currentFilm: Consumer<DatenFilm?>,
) : JPanel() {
    private val copyHqUrlToClipboardActionValue: CopyUrlToClipboardAction
    private val copyNormalUrlToClipboardActionValue: CopyUrlToClipboardAction
    private var swingFilterDialog: SwingFilterDialog? = null
    private var swingFilterDialogFactory: () -> SwingFilterDialog
    private val toggleFilterDialogVisibilityActionValue: ToggleFilterDialogVisibilityAction
    private val reloadTableScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val filterController: FilmFilterController
    private val bookmarkController: FilmBookmarkController
    private var stopBeob = false
    private val tabelle = MVFilmTable()
    private val lifecycleController: FilmLifecycleController
    private val viewController: FilmViewController
    private val selectionController: FilmSelectionController
    private val tableReloader: FilmTableReloader
    private val tableInstaller: FilmTableInstaller
    private val filterSelectionSynchronizer: FilmFilterSelectionSynchronizer
    private var reloadTableDataJob: Job? = null

    private data class SelectionComponents(
        val selectionController: FilmSelectionController,
        val bookmarkController: FilmBookmarkController,
        val saveSelectedFilm: (DatenPset?) -> Unit,
        val filmActionHost: FilmActionHost,
    )

    private data class FilmActions(
        val playFilmAction: PlayFilmAction,
        val saveFilmAction: SaveFilmAction,
        val copyHqUrlToClipboardAction: CopyUrlToClipboardAction,
        val copyNormalUrlToClipboardAction: CopyUrlToClipboardAction,
        val toggleFilterDialogVisibilityAction: ToggleFilterDialogVisibilityAction,
        val bookmarkAddFilmAction: BookmarkAddFilmAction,
        val bookmarkRemoveFilmAction: BookmarkRemoveFilmAction,
        val deleteBookmarksAction: DeleteBookmarksAction,
        val manageBookmarkAction: ManageBookmarkAction,
        val filmUiActions: FilmUiActions,
    )

    private data class FilterComponents(
        val filterController: FilmFilterController,
        val filterSelectionComboBoxModel: FilterSelectionComboBoxModel,
    )

    private data class ViewComponents(
        val tableInstaller: FilmTableInstaller,
        val viewController: FilmViewController,
    )

    private data class InstalledUi(
        val searchField: SearchField,
        val filmToolBar: FilmToolBar,
        val swingFilterDialogFactory: () -> SwingFilterDialog,
    )

    init {
        val psetButtonsTab = JTabbedPane()
        val descriptionTabController = DescriptionTabController({ ownerFrame }, ::editFilmDescription)
        val filterConfiguration = ApplicationConfiguration.getInstance().createFilterConfiguration()
        val selectionComponents = createSelectionComponents(filterConfiguration)
        selectionController = selectionComponents.selectionController
        bookmarkController = selectionComponents.bookmarkController
        val bookmarkActionHost = createBookmarkActionHost()
        val deleteBookmarksAction = DeleteBookmarksAction(bookmarks, bookmarkActionHost)
        val filmActions = createFilmActions(deleteBookmarksAction, selectionComponents)
        copyHqUrlToClipboardActionValue = filmActions.copyHqUrlToClipboardAction
        copyNormalUrlToClipboardActionValue = filmActions.copyNormalUrlToClipboardAction
        toggleFilterDialogVisibilityActionValue = filmActions.toggleFilterDialogVisibilityAction
        val bookmarkStartupReloadCoordinator = BookmarkStartupReloadCoordinator()
        val filmListScrollPane = JScrollPane()
        val cbkShowDescription = JCheckBoxMenuItem("Beschreibung anzeigen")
        val cbShowButtons = JCheckBoxMenuItem("Buttons anzeigen")
        val filterComponents = createFilterComponents(filterConfiguration)
        filterController = filterComponents.filterController
        val searchFieldHost = createSearchFieldHost()
        val viewComponents = createViewComponents(
            psetButtonsTab,
            cbShowButtons,
            cbkShowDescription,
            descriptionTabController,
            filmListScrollPane,
            selectionComponents,
            filmActions,
        )
        tableInstaller = viewComponents.tableInstaller
        viewController = viewComponents.viewController
        val installedUi = installUi(
            filmListScrollPane,
            descriptionTabController,
            psetButtonsTab,
            filterComponents,
            selectionComponents,
            filmActions,
            viewComponents,
            cbkShowDescription,
            searchFieldHost,
        )
        swingFilterDialogFactory = installedUi.swingFilterDialogFactory

        tableInstaller.setupTable()
        tableReloader = createTableReloader(installedUi.searchField, filterComponents.filterController)
        filterSelectionSynchronizer = createFilterSelectionSynchronizer(
            filterComponents.filterSelectionComboBoxModel,
            filterComponents.filterController,
        )
        restoreStartupFilterDialogVisibility()
        lifecycleController = createLifecycleController(
            filterConfiguration,
            bookmarkStartupReloadCoordinator,
            installedUi,
            filmActions,
            filterComponents,
        )
        lifecycleController.start()
    }

    private fun createSelectionComponents(filterConfiguration: FilterConfiguration): SelectionComponents {
        val selectionHost = FilmSelectionHostAdapter(
            { tabelle },
            this,
            this::startFilmDownloads,
            { pset, film, resolution -> downloads.startWithProgram(pset, film, resolution) },
            { filterConfiguration.isShowHighQualityOnly },
            currentFilm,
        )
        val selectionController = FilmSelectionController(selectionHost)
        val bookmarkHost = object : FilmBookmarkController.Host {
            override fun ownerFrame() = ownerFrame

            override fun bookmarks() = bookmarks

            override fun programSets() = programSets

            override fun downloads() = downloads

            override fun addDownloads(films: List<DatenFilm>) {
                startFilmDownloads(films, null, null)
            }

            override fun editFilmDescription(film: DatenFilm) {
                this@GuiFilme.editFilmDescription(film)
            }

            override fun repaintOwner() {
                repaint()
            }
        }
        val bookmarkController = FilmBookmarkController(bookmarkHost)
        val saveSelectedFilm = { pset: DatenPset? ->
            synchronized(this) {
                selectionController.saveFilm(pset)
            }
        }
        val filmActionHost = object : FilmActionHost {
            override fun saveFilm(pSet: DatenPset?) {
                saveSelectedFilm(pSet)
            }

            override fun selectedFilms() = selectionController.getSelectedFilms()

            override fun updateBookmarkListAndRefresh(films: List<DatenFilm>) {
                bookmarkController.updateBookmarkListAndRefresh(films)
            }

            override fun currentlySelectedFilm() = selectionController.getCurrentlySelectedFilm()

            override fun toggleFilterDialogVisibility() {
                this@GuiFilme.toggleFilterDialogVisibility()
            }
        }

        return SelectionComponents(
            selectionController,
            bookmarkController,
            saveSelectedFilm,
            filmActionHost,
        )
    }

    private fun createBookmarkActionHost(): FilmBookmarkHost =
        object : FilmBookmarkHost {
            override fun ownerFrame() = ownerFrame

            override val bookmarkDialog: BookmarkDialog?
                get() = bookmarkController.getBookmarkDialog()

            override fun showManageBookmarkWindow() {
                bookmarkController.showManageBookmarkWindow()
            }

            override fun resetFilterDialogPosition() {
                this@GuiFilme.resetFilterDialogPosition()
            }

            override fun repaintFilmTab() {
                repaint()
            }
        }

    private fun editFilmDescription(film: DatenFilm) {
        DialogFilmBeschreibung(ownerFrame, programSets, film).isVisible = true
    }

    private fun startFilmDownloads(
        films: List<DatenFilm>,
        pSet: DatenPset?,
        requestedResolution: FilmResolution.Enum?,
    ) {
        startDownloads(
            programSets,
            downloads,
            ownerFrame,
            films,
            pSet,
            requestedResolution,
            programSetExporter,
        ) { film, effectivePSet, resolution ->
            DialogAddDownload(
                ownerFrame,
                programSets,
                downloads,
                film,
                effectivePSet,
                Optional.ofNullable(resolution),
            ).isVisible = true
        }
    }

    private fun createSearchFieldHost(): SearchField.Host =
        object : SearchField.Host {
            override val showLuceneTutorialAction: Action = this@GuiFilme.showLuceneTutorialAction

            override fun ownerWindow() = ownerFrame

            override fun loadTable() {
                this@GuiFilme.loadTable()
            }

            override fun loadTable(fromSearchField: Boolean) {
                this@GuiFilme.loadTable(fromSearchField)
            }
        }

    private fun createFilmActions(
        deleteBookmarksAction: DeleteBookmarksAction,
        selectionComponents: SelectionComponents,
    ): FilmActions {
        val selectionController = selectionComponents.selectionController
        val filmActionHost = selectionComponents.filmActionHost
        val playFilmAction = PlayFilmAction(programSets, { selectionController.startFilm(it) }) { ownerFrame }
        val saveFilmAction = SaveFilmAction(filmActionHost)
        val copyHqUrlToClipboardAction =
            CopyUrlToClipboardAction(filmActionHost, FilmResolution.Enum.HIGH_QUALITY)
        val copyNormalUrlToClipboardAction =
            CopyUrlToClipboardAction(filmActionHost, FilmResolution.Enum.NORMAL)
        val toggleFilterDialogVisibilityAction = ToggleFilterDialogVisibilityAction(filmActionHost)
        val bookmarkAddFilmAction = BookmarkAddFilmAction(filmActionHost)
        val bookmarkRemoveFilmAction = BookmarkRemoveFilmAction(filmActionHost)
        val manageBookmarkAction = ManageBookmarkAction(createBookmarkActionHost())
        val markFilmAsSeenAction =
            MarkFilmAsSeenAction { selectionController.getSelectedFilms() }
        val markFilmAsUnseenAction =
            MarkFilmAsUnseenAction { selectionController.getSelectedFilms() }
        val downloadSubtitleAction =
            DownloadSubtitleAction(ownerFrame) { selectionController.getCurrentlySelectedFilm() }
        val filmUiActions = FilmUiActions(
            playFilmAction,
            saveFilmAction,
            bookmarkAddFilmAction,
            bookmarkRemoveFilmAction,
            deleteBookmarksAction,
            manageBookmarkAction,
            copyNormalUrlToClipboardAction,
            copyHqUrlToClipboardAction,
            markFilmAsSeenAction,
            markFilmAsUnseenAction,
            toggleBlacklistAction,
            editBlacklistAction,
            showFilmInformationAction,
            downloadSubtitleAction,
        )

        return FilmActions(
            playFilmAction,
            saveFilmAction,
            copyHqUrlToClipboardAction,
            copyNormalUrlToClipboardAction,
            toggleFilterDialogVisibilityAction,
            bookmarkAddFilmAction,
            bookmarkRemoveFilmAction,
            deleteBookmarksAction,
            manageBookmarkAction,
            filmUiActions,
        )
    }

    private fun createFilterComponents(filterConfiguration: FilterConfiguration): FilterComponents {
        val filterController = FilmFilterController(
            filterConfiguration,
            FilmFilterDataProviderAdapter(filmCatalog),
            object : FilmFilterController.ReloadRequester {
                override fun requestTableReload() {
                    this@GuiFilme.requestTableReload()
                }

                override fun requestZeitraumReload() {
                    this@GuiFilme.requestZeitraumReload()
                }
            },
        )
        val filterSelectionComboBoxModel = FilterSelectionComboBoxModel(
            filterController::currentFilter,
            filterController::availableFilters,
            filterController::isFilterLocked,
            filterController.selectionObserverRegistry(),
        )

        return FilterComponents(filterController, filterSelectionComboBoxModel)
    }

    private fun createViewComponents(
        psetButtonsTab: JTabbedPane,
        cbShowButtons: JCheckBoxMenuItem,
        cbkShowDescription: JCheckBoxMenuItem,
        descriptionTabController: DescriptionTabController,
        filmListScrollPane: JScrollPane,
        selectionComponents: SelectionComponents,
        filmActions: FilmActions,
    ): ViewComponents {
        val selectionController = selectionComponents.selectionController
        val filmUiActions = filmActions.filmUiActions
        val psetButtonsPanel = PsetButtonsPanel(programSets) { pset -> selectionController.startFilm(pset) }
        val viewHost = object : FilmViewController.Host {
            override fun psetButtonsTab() = psetButtonsTab

            override fun psetButtonsPanel() = psetButtonsPanel

            override fun showButtonsMenuItem() = cbShowButtons

            override fun showDescriptionMenuItem() = cbkShowDescription

            override fun actions() = filmUiActions

            override fun setDescriptionTabVisible(visible: Boolean) {
                descriptionTabController.setVisible(visible)
            }

        }
        val tableContextMenuHost = TableContextMenuHostAdapter(
            downloads,
            programSets,
            filmCatalog,
            abos,
            blacklist,
            programSetExporter,
            { tabelle },
            selectionController::getCurrentlySelectedFilm,
            selectionController::getFilm,
            { filmActions.playFilmAction.actionPerformed(null) },
            { selectionComponents.saveSelectedFilm(null) },
            selectionController::startFilm,
            { suspended -> stopBeob = suspended },
            { showFilmInformationAction.actionPerformed(null) },
            ownerFrame,
            { filmUiActions },
        )
        val tableInstallerHost = FilmTableInstallerHostAdapter(
            downloads,
            { tabelle },
            filmListScrollPane,
            this,
            { tableContextMenuHost },
            selectionComponents.filmActionHost,
            { filmUiActions },
            { updateSelectedListItemsCount(tabelle) },
            ::onComponentShown,
            selectionController::updateFilmData,
            { stopBeob },
        )

        return ViewComponents(
            FilmTableInstaller(tableInstallerHost),
            FilmViewController(viewHost),
        )
    }

    private fun installUi(
        filmListScrollPane: JScrollPane,
        descriptionTabController: DescriptionTabController,
        psetButtonsTab: JTabbedPane,
        filterComponents: FilterComponents,
        selectionComponents: SelectionComponents,
        filmActions: FilmActions,
        viewComponents: ViewComponents,
        cbkShowDescription: JCheckBoxMenuItem,
        searchFieldHost: SearchField.Host,
    ): InstalledUi {
        layout = BorderLayout()
        add(filmListScrollPane, BorderLayout.CENTER)
        val extensionArea = JPanel(VerticalLayout())
        add(extensionArea, BorderLayout.SOUTH)

        val searchField = if (filmCatalog.filteredFilms is IndexedFilmList) {
            LuceneSearchField(searchFieldHost)
        } else {
            RegularSearchField(searchFieldHost)
        }

        extensionArea.add(descriptionTabController.tabbedPane)
        extensionArea.add(psetButtonsTab)

        viewComponents.tableInstaller.setupFilmListTable()
        viewComponents.tableInstaller.setupFilmSelectionPropertyListener()
        viewComponents.viewController.setupShowFilmDescriptionMenuItem()
        descriptionTabController.install(
            tabelle,
            cbkShowDescription,
            { ApplicationConfiguration.getInstance().filmDescriptionVisible },
        ) { selectionComponents.selectionController.getCurrentlySelectedFilm() }
        viewComponents.viewController.setupPsetButtonsTab()

        val filmToolBar = FilmToolBar(
            filterComponents.filterSelectionComboBoxModel,
            filmActions.bookmarkAddFilmAction,
            filmActions.bookmarkRemoveFilmAction,
            filmActions.deleteBookmarksAction,
            filmActions.manageBookmarkAction,
            filmActions.playFilmAction,
            filmActions.saveFilmAction,
            searchField,
            filmActions.toggleFilterDialogVisibilityAction,
        )
        add(filmToolBar, BorderLayout.NORTH)

        val swingFilterDialogFactory = {
            SwingFilterDialog(
                ownerFrame,
                filterComponents.filterSelectionComboBoxModel,
                filmToolBar.toggleFilterDialogVisibilityButton,
                filterComponents.filterController,
            )
        }

        return InstalledUi(searchField, filmToolBar, swingFilterDialogFactory)
    }

    private fun swingFilterDialog(): SwingFilterDialog =
        swingFilterDialog ?: swingFilterDialogFactory().also { swingFilterDialog = it }

    private fun existingSwingFilterDialog(): SwingFilterDialog? = swingFilterDialog

    private fun restoreStartupFilterDialogVisibility() {
        if (ApplicationConfiguration.getInstance().filterDialogVisible) {
            swingFilterDialog()
        }
    }

    private fun createFilterSelectionSynchronizer(
        filterSelectionComboBoxModel: FilterSelectionComboBoxModel,
        filterController: FilmFilterController,
    ): FilmFilterSelectionSynchronizer =
        FilmFilterSelectionSynchronizer(
            filterSelectionComboBoxModel,
            filterController,
            object : FilmFilterController.ReloadRequester {
                override fun requestTableReload() = this@GuiFilme.requestTableReload()
                override fun requestZeitraumReload() = this@GuiFilme.requestZeitraumReload()
            },
        )

    private fun createTableReloader(
        searchField: SearchField,
        filterController: FilmFilterController,
    ): FilmTableReloader {
        val tableReloadHost = FilmTableReloadHostAdapter(
            filmCatalog,
            ownerFrame,
            { tabelle },
            {
                SearchFieldData(searchField.text, searchField.getSearchMode())
            },
            filterController,
            { suspended -> stopBeob = suspended },
            ::updateStartInfoProperty,
            selectionController::updateFilmData,
        )

        return FilmTableReloader(tableReloadHost)
    }

    private fun createLifecycleController(
        filterConfiguration: FilterConfiguration,
        bookmarkStartupReloadCoordinator: BookmarkStartupReloadCoordinator,
        installedUi: InstalledUi,
        filmActions: FilmActions,
        filterComponents: FilterComponents,
    ): FilmLifecycleController {
        val lifecycleHost = FilmLifecycleHostAdapter(
            this,
            filmListLoader,
            { tabelle },
            filterConfiguration,
            bookmarkStartupReloadCoordinator,
            ::existingSwingFilterDialog,
            { installedUi.filmToolBar },
            { installedUi.searchField },
            { filmActions.filmUiActions },
            ::requestTableReload,
            ::updateStartInfoProperty,
            ::tabelleSpeichern,
            filterComponents.filterSelectionComboBoxModel::close,
        )

        return FilmLifecycleController(lifecycleHost)
    }

    private fun toggleFilterDialogVisibility() {
        val filterDialog = swingFilterDialog()
        filterDialog.isVisible = !filterDialog.isVisible
    }

    private fun requestTableReload() {
        reloadTableDataJob?.cancel()
        reloadTableDataJob = reloadTableScope.launch {
            delay(RELOAD_TABLE_DATA_DELAY)
            tableReloader.loadTable()
        }
    }

    private fun requestZeitraumReload() {
        blacklist.applyToFilmList()
        requestTableReload()
    }

    fun copyHqUrlToClipboardAction(): Action = copyHqUrlToClipboardActionValue

    fun copyNormalUrlToClipboardAction(): Action = copyNormalUrlToClipboardActionValue

    fun toggleFilterDialogVisibilityAction(): Action = toggleFilterDialogVisibilityActionValue

    fun resetFilterDialogPosition() {
        swingFilterDialog().setLocation(100, 100)
    }

    fun disposePanel() {
        reloadTableScope.cancel()
        filterSelectionSynchronizer.close()
        tableReloader.dispose()
        lifecycleController.disposePanel()
    }

    val currentZeitraumFilterValue: String
        get() = filterController.state().zeitraum

    @Handler
    fun handleTableModelChange(event: TableModelChangeEvent) {
        lifecycleController.handleTableModelChange(event)
    }

    fun tabelleSpeichern() {
        tableInstaller.writeTableConfigurationData()
    }

    fun installViewMenuEntry(jMenuAnsicht: JMenu) {
        viewController.installViewMenuEntry(jMenuAnsicht)
    }

    fun installMenuEntries(menu: JMenu) {
        viewController.installMenuEntries(menu)
    }

    private fun onComponentShown() {
        selectionController.updateFilmData()
        updateStartInfoProperty()
    }

    private fun updateSelectedListItemsCount(table: JTable) {
        selectedListItemsCount.accept(table.selectedRowCount.toLong())
    }

    private fun updateStartInfoProperty() {
        MessageBus.messageBus.publishAsync(UpdateStatusBarLeftDisplayEvent())
    }

    val tableRowCount: Int
        get() = selectionController.getTableRowCount()

    @Handler
    private fun handleDownloadHistoryChangedEvent(event: DownloadHistoryChangedEvent) {
        lifecycleController.handleDownloadHistoryChangedEvent(event)
    }

    @Handler
    private fun handleButtonStart(event: ButtonStartEvent) {
        lifecycleController.handleButtonStart(event)
    }

    @Handler
    private fun handleStartEvent(message: StartEvent) {
        lifecycleController.handleStartEvent(message)
    }

    fun showManageBookmarkWindow() {
        bookmarkController.showManageBookmarkWindow()
    }

    fun getBookmarkDialog(): BookmarkDialog? = bookmarkController.getBookmarkDialog()

    @Handler
    private fun handleReloadTableDataEvent(event: ReloadTableDataEvent) {
        lifecycleController.handleReloadTableDataEvent(event)
    }

    @Handler
    private fun handleBookmarkRefreshCompletedEvent(event: BookmarkRefreshCompletedEvent) {
        lifecycleController.handleBookmarkRefreshCompletedEvent(event)
    }

    private fun loadTable() {
        tableReloader.loadTable()
    }

    private fun loadTable(fromSearchField: Boolean) {
        tableReloader.loadTable(fromSearchField)
    }

    companion object {
        const val NAME = "Filme"
        private val RELOAD_TABLE_DATA_DELAY = 250.milliseconds
    }
}
