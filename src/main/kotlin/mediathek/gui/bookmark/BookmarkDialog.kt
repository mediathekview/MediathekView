/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.bookmark

import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.ObservableElementList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.impl.beans.BeanTableFormat
import ca.odell.glazedlists.swing.DefaultEventSelectionModel
import ca.odell.glazedlists.swing.GlazedListsSwing
import ca.odell.glazedlists.swing.TableComparatorChooser
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.audiothek.ui.table.CenteredTextCellRenderer
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.SeenHistoryController
import mediathek.gui.bookmark.renderer.*
import mediathek.gui.tabs.tab_film.FilmDescriptionPanel
import mediathek.gui.tabs.tab_film.startDownloads
import mediathek.mainwindow.MediathekGui
import mediathek.swing.IconOnlyButton
import mediathek.swing.IconUtils
import mediathek.swing.NoIconMenuItem
import mediathek.swing.table.GlazedSortKeysPersister
import mediathek.swing.table.IconHeaderCellRenderer
import mediathek.swing.table.TableUtils
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.withReadLock
import org.kordamp.ikonli.fontawesome6.FontAwesomeRegular
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.materialdesign2.MaterialDesignE
import org.kordamp.ikonli.materialdesign2.MaterialDesignN
import java.awt.BorderLayout
import java.awt.Frame
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.*

class BookmarkDialog(owner: Frame) : JDialog(owner) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val filmDescriptionPanel = FilmDescriptionPanel()
    private val noteArea = JTextArea()
    private val table = JTable()
    private val playFilmAction = PlayFilmAction()
    private val addDownloadAction = AddDownloadAction()
    private val addNoteAction = AddNoteAction()
    private val markSeenAction = MarkSeenAction()
    private val markUnseenAction = MarkUnseenAction()
    private val removeNoteAction = RemoveNoteAction()
    private val deleteBookmarkAction = DeleteBookmarkAction()
    private val dialogJob = SupervisorJob()
    private val uiScope = CoroutineScope(dialogJob + Dispatchers.Swing)

    private lateinit var selectionModel: DefaultEventSelectionModel<BookmarkData>
    private lateinit var tableColumnSettingsManager: BookmarkTableColumnSettingsManager<BookmarkData>
    private lateinit var sortPersister: GlazedSortKeysPersister<BookmarkData>

    init {
        title = "Merkliste verwalten"
        isModal = false
        defaultCloseOperation = DISPOSE_ON_CLOSE

        EscapeKeyHandler.installHandler(this, this::dispose)

        setupToolBar()
        contentPane.add(JScrollPane(table), BorderLayout.CENTER)

        setupNoteArea()
        contentPane.add(createTabbedPane(), BorderLayout.SOUTH)

        setupTable()
        TableUtils.fitColumnHeaders(table, 5)

        installListener()
        restoreBounds()

        updateActionStates()
    }

    override fun dispose() {
        dialogJob.cancel()
        super.dispose()
    }

    private fun installTableContextMenu() {
        table.addMouseListener(
            object : MouseAdapter() {
                private fun createPopupMenu(): JPopupMenu =
                    JPopupMenu().apply {
                        add(NoIconMenuItem(playFilmAction))
                        addSeparator()
                        add(NoIconMenuItem(addDownloadAction))
                        addSeparator()
                        add(NoIconMenuItem(addNoteAction))
                        add(NoIconMenuItem(removeNoteAction))
                        addSeparator()
                        add(NoIconMenuItem(markSeenAction))
                        add(NoIconMenuItem(markUnseenAction))
                        addSeparator()
                        add(NoIconMenuItem(deleteBookmarkAction))
                    }

                private fun showPopup(event: MouseEvent) {
                    if (!event.isPopupTrigger) {
                        return
                    }

                    val row = table.rowAtPoint(event.point)
                    if (row != -1 && !table.isRowSelected(row)) {
                        table.selectionModel.setSelectionInterval(row, row)
                    }

                    if (selectionModel.selected.isEmpty()) {
                        return
                    }

                    createPopupMenu().show(event.component, event.x, event.y)
                }

                override fun mousePressed(event: MouseEvent) {
                    showPopup(event)
                }

                override fun mouseReleased(event: MouseEvent) {
                    showPopup(event)
                }
            },
        )
    }

    private fun installListener() {
        addWindowListener(
            object : WindowAdapter() {
                override fun windowClosing(event: WindowEvent) {
                    saveBounds()
                    tableColumnSettingsManager.save()
                    dispose()
                }

                override fun windowClosed(event: WindowEvent) {
                    saveBounds()
                }
            },
        )
    }

    private fun saveBounds() {
        val bounds = bounds
        applicationConfiguration.setBookmarkDialogBounds(
            bounds.x,
            bounds.y,
            bounds.width,
            bounds.height,
        )
    }

    private fun restoreBounds() {
        val bounds = applicationConfiguration.bookmarkDialogBounds
        setBounds(bounds.x, bounds.y, bounds.width, bounds.height)
    }

    private fun setupNoteArea() {
        noteArea.lineWrap = true
        noteArea.wrapStyleWord = true
        noteArea.isEditable = false
    }

    private fun createTabbedPane(): JTabbedPane =
        JTabbedPane(JTabbedPane.TOP).apply {
            addTab("Beschreibung", filmDescriptionPanel)
            addTab(
                "Notizen",
                JPanel(BorderLayout()).apply {
                    border = BorderFactory.createEmptyBorder(5, 5, 5, 5)
                    add(noteArea, BorderLayout.CENTER)
                },
            )
        }

    private fun getTableFormat(): TableFormat<BookmarkData> {
        val propertyNames = arrayOf(
            "seen",
            "sender",
            "thema",
            "title",
            "dauer",
            "sendedatum",
            "AvailableUntil",
            "NormalQualityUrl",
            "note",
            "filmHashCode",
            "BookmarkAdded",
        )
        val columnLabels = arrayOf(
            "Gesehen",
            "Sender",
            "Thema",
            "Titel",
            "Dauer",
            "Sendedatum",
            "Verfügbar bis",
            "URL",
            "Notiz",
            "Hash Code",
            "hinzugefügt am",
        )
        return BeanTableFormat(BookmarkData::class.java, propertyNames, columnLabels)
    }

    private fun disableSortableColumns(comparatorChooser: TableComparatorChooser<BookmarkData>) {
        comparatorChooser.getComparatorsForColumn(COLUMN_NORMAL_QUALITY_URL).clear()
        comparatorChooser.getComparatorsForColumn(COLUMN_HASHCODE).clear()
        comparatorChooser.getComparatorsForColumn(COLUMN_NOTIZ).clear()
        comparatorChooser.getComparatorsForColumn(COLUMN_SEEN).clear()
    }

    private fun setupTable() {
        val bookmarkConnector = GlazedLists.beanConnector(BookmarkData::class.java) as ObservableElementList.Connector<BookmarkData>
        val sourceEventList = Daten.getInstance().listeBookmarkList.getEventList()

        val sortedList = sourceEventList.withReadLock {
            val observedBookmarks = ObservableElementList(sourceEventList, bookmarkConnector)
            SortedList(observedBookmarks, BookmarkAddedAtComparator())
        }

        val model = GlazedListsSwing.eventTableModelWithThreadProxyList(sortedList, getTableFormat())
        selectionModel = DefaultEventSelectionModel(sortedList)
        selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                updateActionStates()
                updateInfoTabs()
            }
        }

        table.autoResizeMode = JTable.AUTO_RESIZE_OFF
        table.model = model
        table.selectionModel = selectionModel

        val comparatorChooser = TableComparatorChooser.install(table, sortedList, TableComparatorChooser.MULTIPLE_COLUMN_MOUSE)
        disableSortableColumns(comparatorChooser)

        sortPersister = GlazedSortKeysPersister(CONFIG_PREFIX, comparatorChooser)
        sortPersister.restoreSortState()
        comparatorChooser.addSortActionListener { sortPersister.saveSortState() }
        setupCellRenderers()

        tableColumnSettingsManager = BookmarkTableColumnSettingsManager(table, CONFIG_PREFIX, comparatorChooser)
        tableColumnSettingsManager.load()
        tableColumnSettingsManager.installContextMenu()

        installTableContextMenu()
    }

    private fun setupCellRenderers() {
        val columnModel = table.columnModel

        val seenColumn = columnModel.getColumn(COLUMN_SEEN)
        seenColumn.cellRenderer = SeenCellRenderer()
        seenColumn.headerRenderer = IconHeaderCellRenderer(IconUtils.of(MaterialDesignE.EYE), "Gesehen")

        columnModel.getColumn(COLUMN_SENDER).cellRenderer = CenteredTextCellRenderer()
        columnModel.getColumn(COLUMN_DAUER).cellRenderer = FilmLengthCellRenderer()
        columnModel.getColumn(COLUMN_SENDEDATUM).cellRenderer = CenteredTextCellRenderer()
        columnModel.getColumn(COLUMN_AVAILABLE_UNTIL).cellRenderer = AvailableUntilCellRenderer()

        val noteColumn = columnModel.getColumn(COLUMN_NOTIZ)
        noteColumn.cellRenderer = NoteCellRenderer()
        noteColumn.headerRenderer = IconHeaderCellRenderer(IconUtils.of(MaterialDesignN.NOTE), "Notiz vorhanden")

        columnModel.getColumn(COLUM_BOOKMARK_ADDED_AT).cellRenderer = AddedAtCellRenderer()
    }

    private fun setupToolBar() {
        val toolBar =
            JToolBar().apply {
                isFloatable = false
                add(IconOnlyButton(playFilmAction))
                addSeparator()
                add(IconOnlyButton(addDownloadAction))
                addSeparator()
                add(IconOnlyButton(addNoteAction))
                add(IconOnlyButton(removeNoteAction))
                addSeparator()
                add(IconOnlyButton(markSeenAction))
                add(IconOnlyButton(markUnseenAction))
                addSeparator()
                add(IconOnlyButton(deleteBookmarkAction))
            }

        contentPane.add(toolBar, BorderLayout.NORTH)
    }

    private fun updateActionStates() {
        playFilmAction.isEnabled = selectedPlayableFilm() != null
        addDownloadAction.isEnabled = selectedDownloadableFilms().isNotEmpty()
        addNoteAction.isEnabled = selectionModel.selected.size == 1 && selectionModel.selected.isNotEmpty()
    }

    private fun selectedPlayableFilm(): mediathek.daten.DatenFilm? =
        selectionModel.selected.singleOrNull()?.datenFilm

    private fun selectedDownloadableFilms() =
        selectionModel.selected
            .mapNotNull { it.datenFilm }

    private fun updateInfoTabs() {
        val selectedBookmarks = selectionModel.selected
        if (selectedBookmarks.size == 1) {
            val bookmark = selectedBookmarks.first()
            val film = bookmark.datenFilm
            filmDescriptionPanel.setCurrentFilm(film)
            noteArea.text = bookmark.note.orEmpty()
        } else {
            filmDescriptionPanel.setCurrentFilm(null)
            noteArea.text = ""
        }
    }

    private fun persistBookmarksAsync() {
        uiScope.launch {
            withContext(Dispatchers.IO) {
                Daten.getInstance().listeBookmarkList.saveToFile()
            }
        }
    }

    inner class PlayFilmAction : AbstractAction() {
        init {
            putValue(NAME, "Film abspielen")
            putValue(SHORT_DESCRIPTION, "Ausgewählten Merklisteintrag abspielen")
            putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.PLAY))
        }

        override fun actionPerformed(event: java.awt.event.ActionEvent?) {
            val film = selectedPlayableFilm() ?: return
            val pSet = Daten.getInstance().listePset.psetAbspielen
            if (pSet == null) {
                JOptionPane.showMessageDialog(
                    MediathekGui.ui(),
                    "Es wurde kein Videoplayer eingerichtet.\n" +
                        "Bitte legen Sie diesen unter \"Einstellungen->Set bearbeiten\" fest.",
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE,
                )
                return
            }

            Daten.getInstance().downloadStartCoordinator.urlMitProgrammStarten(pSet, film, "")
        }
    }

    inner class AddDownloadAction : AbstractAction() {
        init {
            putValue(NAME, "Download anlegen...")
            putValue(SHORT_DESCRIPTION, "Download für ausgewählte Merklisteinträge anlegen")
            putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.DOWNLOAD))
        }

        override fun actionPerformed(event: java.awt.event.ActionEvent?) {
            val selectedBookmarks = ArrayList(selectionModel.selected)
            val films = selectedBookmarks.mapNotNull { it.datenFilm }
            if (films.isEmpty()) {
                return
            }

            startDownloads(MediathekGui.ui(), films, null, null)

            val skippedBookmarks = selectedBookmarks.size - films.size
            if (skippedBookmarks > 0) {
                JOptionPane.showMessageDialog(
                    this@BookmarkDialog,
                    "$skippedBookmarks Merklisteinträge konnten nicht geladen werden,\n" +
                        "weil die Filme nicht mehr in der Filmliste vorhanden sind.",
                    title,
                    JOptionPane.INFORMATION_MESSAGE,
                )
            }
        }
    }

    inner class MarkSeenAction : AbstractAction() {
        init {
            putValue(NAME, "Als gesehen markieren")
            putValue(SHORT_DESCRIPTION, "Als gesehen markieren")
            putValue(SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignE.EYE))
        }

        override fun actionPerformed(event: java.awt.event.ActionEvent?) {
            val selectedFilms =
                selectionModel.selected
                    .mapNotNull { it.datenFilm }

            SeenHistoryController().use { controller ->
                controller.markSeen(selectedFilms)
            }
        }
    }

    inner class MarkUnseenAction : AbstractAction() {
        init {
            putValue(NAME, "Als ungesehen markieren")
            putValue(SHORT_DESCRIPTION, "Als ungesehen markieren")
            putValue(SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignE.EYE_OFF))
        }

        override fun actionPerformed(event: java.awt.event.ActionEvent?) {
            val selectedFilms =
                selectionModel.selected
                    .mapNotNull { it.datenFilm }

            SeenHistoryController().use { controller ->
                controller.markUnseen(selectedFilms)
            }
        }
    }

    inner class AddNoteAction : AbstractAction() {
        init {
            putValue(NAME, "Notiz hinzufügen...")
            putValue(SHORT_DESCRIPTION, "Notiz hinzufügen")
            putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeRegular.EDIT))
        }

        override fun actionPerformed(event: java.awt.event.ActionEvent?) {
            val bookmark = selectionModel.selected.first()
                val dialog = BookmarkEditNoteDialog(this@BookmarkDialog, bookmark)
                dialog.isVisible = true
                if (dialog.isOkPressed()) {
                    var noteText: String? = dialog.getNotiz()
                    if (noteText.isNullOrBlank()) {
                        noteText = null
                    }
                    bookmark.note = noteText
                    bookmark.availableUntil = dialog.getAvailableUntilDate()
                    updateInfoTabs()
                    persistBookmarksAsync()
                }
            }
        }

    inner class RemoveNoteAction : AbstractAction() {
        init {
            putValue(NAME, "Notiz löschen...")
            putValue(SHORT_DESCRIPTION, "Notiz löschen")
            putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.ERASER))
        }

        override fun actionPerformed(event: java.awt.event.ActionEvent?) {
            if (!selectionModel.isSelectionEmpty) {
                selectionModel.selected.forEach { it.note = null }
                updateInfoTabs()
                persistBookmarksAsync()
            }
        }
    }

    inner class DeleteBookmarkAction : AbstractAction() {
        init {
            putValue(NAME, "Aus Merkliste löschen...")
            putValue(SHORT_DESCRIPTION, "Aus Merkliste löschen")
            putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeRegular.TRASH_ALT))
        }

        override fun actionPerformed(event: java.awt.event.ActionEvent?) {
            val bookmarkList = Daten.getInstance().listeBookmarkList
            val bookmarksToRemove = ArrayList(selectionModel.selected)
            for (bookmark in bookmarksToRemove) {
                bookmarkList.removeBookmark(bookmark)
            }

            updateInfoTabs()
            uiScope.launch {
                withContext(Dispatchers.IO) {
                    bookmarkList.saveToFile()
                }
                MediathekGui.ui().repaintFilmTab()
            }
        }
    }

    private companion object {
        const val COLUMN_SEEN = 0
        const val COLUMN_SENDER = 1
        const val COLUMN_DAUER = 4
        const val COLUMN_SENDEDATUM = 5
        const val COLUMN_AVAILABLE_UNTIL = 6
        const val COLUMN_NORMAL_QUALITY_URL = 7
        const val COLUMN_NOTIZ = 8
        const val COLUMN_HASHCODE = 9
        const val COLUM_BOOKMARK_ADDED_AT = 10

        const val CONFIG_PREFIX = "ui.bookmark-dialog"
    }
}
