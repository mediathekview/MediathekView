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

package mediathek.gui.abo

import ca.odell.glazedlists.swing.AdvancedTableModel
import ca.odell.glazedlists.swing.GlazedListsSwing
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.audiothek.ui.table.CenteredTextCellRenderer
import mediathek.config.Daten
import mediathek.daten.abo.AboTags
import mediathek.daten.abo.DatenAbo
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.gui.actions.CreateNewAboAction
import mediathek.gui.dialog.DialogEditAbo
import mediathek.gui.dialog.MissingProgramSetDialog
import mediathek.gui.messages.AboListChangedEvent
import mediathek.swing.InfiniteProgressPanel
import mediathek.tool.*
import mediathek.tool.cellrenderer.CellRendererBase
import mediathek.tool.datum.DateUtil
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import org.jdesktop.swingx.JXStatusBar
import java.awt.BorderLayout
import java.awt.Color
import java.awt.Component
import java.awt.Dimension
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.time.LocalDate
import javax.swing.*
import kotlin.time.Duration.Companion.milliseconds

class ManageAboPanel(dialog: JDialog, private val owner: JFrame) : JPanel() {
    private val tabelle = AboTable()
    private val daten = Daten.getInstance()
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val createAboAction = CreateNewAboAction(daten.listeAbo) { owner }
    private lateinit var tableBinding: AboTableBinding
    private lateinit var tableColumnSettings: AboTableColumnSettings
    private val infoPanel = JXStatusBar()
    private val totalAbos = JLabel("totalAbos")
    private val activeAbos = JLabel("activeAbos")
    private val inactiveAbos = JLabel("inactiveAbos")
    private val swingToolBar = JToolBar()
    private val senderCombo = JComboBox<String>()
    private val infiniteProgressPanel = InfiniteProgressPanel()
    private val btnEditAbo = JButton()
    private val scrollPane = JScrollPane(tabelle)
    private val filmLoadListener = object : ListenerFilmeLaden() {
        @Suppress("UNUSED_PARAMETER")
        override fun start(event: ListenerFilmeLadenEvent) {
            markAboFilmCountsLoadingFromLoad()
        }

        @Suppress("UNUSED_PARAMETER")
        override fun fertig(event: ListenerFilmeLadenEvent) {
            scheduleAboFilmCountRefresh()
        }

        @Suppress("UNUSED_PARAMETER")
        override fun fertigOnlyOne(event: ListenerFilmeLadenEvent) {
            scheduleAboFilmCountRefresh()
        }
    }
    private var aboFilmCounts: Map<DatenAbo, Int> = emptyMap()
    private var aboFilmCountsLoading = false
    private var countRefreshJob: Job? = null
    private var countRefreshSequence = 0
    private var disposed = false

    init {
        initComponents()

        tableBinding = AboTableBinding(tabelle, daten.listeAbo, this::filmCountForAbo)
        setupToolBar()
        setupInfoPanel()
        updateInfoText()

        MessageBus.messageBus.subscribe(this)
        daten.filmeLaden.addAdListener(filmLoadListener)

        initListeners()
        initializeTable()
        initializeAboFilmCounts()

        tableBinding.addSelectionListener {
            btnEditAbo.isEnabled = tableBinding.selectedAboCount <= 1
        }

        dialog.glassPane = infiniteProgressPanel
    }

    fun tabelleSpeichern() {
        if (::tableBinding.isInitialized) {
            tableBinding.saveSortState()
        }
        if (::tableColumnSettings.isInitialized) {
            tableColumnSettings.save()
        }
    }

    override fun removeNotify() {
        if (!disposed) {
            disposed = true
            daten.filmeLaden.removeAdListener(filmLoadListener)
            countRefreshJob?.cancel()
            uiScope.cancel()
            if (::tableBinding.isInitialized) {
                tableBinding.dispose()
            }
        }
        super.removeNotify()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleAboListChanged(event: AboListChangedEvent) {
        SwingUtilities.invokeLater {
            updateInfoText()
            tableBinding.selectFirstRowIfNecessary()
            scheduleAboFilmCountRefresh()
        }
    }

    fun editAbo() {
        val selectedAbos = tableBinding.selectedAbos
        if (selectedAbos.isEmpty()) {
            NoSelectionErrorDialog.show(this)
            return
        }

        val editedAbo = selectedAbos.first()
        val multiEdit = selectedAbos.size > 1
        val dialogAbo = if (multiEdit) editedAbo.copyForEditDialog() else editedAbo

        if (!MissingProgramSetDialog.ensureAboProgramSetAvailable(owner)) {
            return
        }

        val dialog = DialogEditAbo(owner, dialogAbo, multiEdit)
        dialog.title = EDIT_ABO_TEXT
        dialog.isVisible = true
        if (!dialog.successful()) {
            return
        }

        if (multiEdit) {
            applyMultiEdit(dialogAbo, selectedAbos, dialog.multiEditCbIndices)
        } else {
            daten.listeAbo.fireAboChanged(editedAbo)
        }

        processAboChanges()
    }

    private fun DatenAbo.copyForEditDialog(): DatenAbo =
        DatenAbo().also { copy ->
            copy.isActive = isActive
            copy.name = name
            copy.sender = sender
            copy.thema = thema
            copy.title = title
            copy.themaTitel = themaTitel
            copy.irgendwo = irgendwo
            copy.mindestDauerMinuten = mindestDauerMinuten
            copy.filmLengthState = filmLengthState
            copy.zielpfad = zielpfad
            copy.downloadDate = downloadDate
            copy.psetName = psetName
            copy.isDoNotStartAutomatically = isDoNotStartAutomatically
        }

    private fun applyMultiEdit(
        sourceAbo: DatenAbo,
        targetAbos: List<DatenAbo>,
        selectedTagIndices: BooleanArray,
    ) {
        for (targetAbo in targetAbos) {
            for (index in selectedTagIndices.indices) {
                if (!selectedTagIndices[index]) {
                    continue
                }

                AboTags.fromIndex(index).ifPresent { tag ->
                    when (tag) {
                        AboTags.EINGESCHALTET -> targetAbo.isActive = sourceAbo.isActive
                        AboTags.MINDESTDAUER -> targetAbo.mindestDauerMinuten = sourceAbo.mindestDauerMinuten
                        AboTags.MIN -> targetAbo.filmLengthState = sourceAbo.filmLengthState
                        AboTags.ZIELPFAD -> targetAbo.zielpfad = sourceAbo.zielpfad
                        AboTags.PSET -> targetAbo.psetName = sourceAbo.psetName
                        AboTags.DO_NOT_START_AUTOMATICALLY ->
                            targetAbo.isDoNotStartAutomatically = sourceAbo.isDoNotStartAutomatically

                        else -> logger.error("Unhandled tag called {}", tag)
                    }
                }
            }
            daten.listeAbo.fireAboChanged(targetAbo)
        }
    }

    private fun setupInfoPanel() {
        infoPanel.add(totalAbos)
        infoPanel.add(activeAbos)
        infoPanel.add(inactiveAbos)
    }

    private fun initializeTable() {
        applySenderFilter()
        tableColumnSettings = AboTableColumnSettings(tabelle, tableBinding::clearSorting)
        tableColumnSettings.load()
        tableColumnSettings.installContextMenu()
        if (tabelle.rowCount > 0) {
            tabelle.setRowSelectionInterval(0, 0)
        }
    }

    private fun setupToolBar() {
        swingToolBar.addToolbarButton(
            tooltip = "Abos einschalten",
            iconPath = "icons/fontawesome/check.svg",
        ) {
            changeAboActiveState(true)
        }

        swingToolBar.addToolbarButton(
            tooltip = "Abos ausschalten",
            iconPath = "icons/fontawesome/xmark.svg",
            iconSize = 16f,
        ) {
            changeAboActiveState(false)
        }
        swingToolBar.addSeparator()

        val button = JButton(createAboAction)
        button.text = ""
        swingToolBar.add(button)

        swingToolBar.addToolbarButton(
            tooltip = "Abos löschen",
            iconPath = "icons/fontawesome/trash-can.svg",
        ) {
            aboLoeschen()
        }

        btnEditAbo.toolTipText = EDIT_ABO_TEXT
        btnEditAbo.addActionListener { editAbo() }
        btnEditAbo.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg")
        swingToolBar.add(btnEditAbo)
        swingToolBar.addSeparator()

        swingToolBar.add(JLabel("Abos für Sender:"))
        senderCombo.maximumSize = Dimension(150, Int.MAX_VALUE)
        val model = GlazedListsSwing.eventComboBoxModel(EventListWithEmptyFirstEntry(daten.allSendersList))
        senderCombo.model = model
        senderCombo.selectedIndex = 0
        senderCombo.addActionListener { applySenderFilter() }
        swingToolBar.add(senderCombo)
    }

    private fun numActiveAbos(): Int = daten.listeAbo.count { abo -> abo.isActive }

    private fun numInactiveAbos(): Int = daten.listeAbo.count { abo -> !abo.isActive }

    private fun updateInfoText() {
        val listeAbo = daten.listeAbo
        val numAbos = listeAbo.size

        totalAbos.text = if (numAbos == 1) {
            "Gesamt: 1 Abo"
        } else {
            String.format("Gesamt: %d Abos", numAbos)
        }

        activeAbos.text = String.format("%d eingeschaltet", numActiveAbos())
        inactiveAbos.text = String.format("%d ausgeschaltet", numInactiveAbos())
    }

    private fun filmCountForAbo(abo: DatenAbo): Int? =
        if (aboFilmCountsLoading) {
            null
        } else {
            aboFilmCounts[abo] ?: 0
        }

    private fun initializeAboFilmCounts() {
        if (daten.filmeLaden.isLoadRunning) {
            markAboFilmCountsLoading()
        } else {
            scheduleAboFilmCountRefresh()
        }
    }

    private fun scheduleAboFilmCountRefresh() {
        uiScope.launch {
            if (disposed) {
                return@launch
            }

            val refreshSequence = ++countRefreshSequence
            countRefreshJob?.cancel()
            markAboFilmCountsLoading()
            countRefreshJob = launch {
                val counts = withContext(Dispatchers.Default) {
                    AboFilmCounts.countMatchingFilms(
                        daten.listeAbo.withReadLock { toList() },
                        daten.listeFilme.snapshot(),
                    )
                }
                if (!disposed && refreshSequence == countRefreshSequence) {
                    applyAboFilmCounts(counts)
                }
            }
        }
    }

    private fun markAboFilmCountsLoadingFromLoad() {
        uiScope.launch {
            if (disposed) {
                return@launch
            }

            ++countRefreshSequence
            countRefreshJob?.cancel()
            markAboFilmCountsLoading()
        }
    }

    private fun applyAboFilmCounts(counts: Map<DatenAbo, Int>) {
        val changedAbos = if (aboFilmCountsLoading) {
            daten.listeAbo.withReadLock { toList() }
        } else {
            AboFilmCounts.changedAbos(aboFilmCounts, counts)
        }
        aboFilmCounts = counts
        aboFilmCountsLoading = false
        if (changedAbos.isNotEmpty()) {
            daten.listeAbo.fireAbosChanged(changedAbos)
        }
    }

    private fun markAboFilmCountsLoading() {
        aboFilmCountsLoading = true
        daten.listeAbo.fireAbosChanged(daten.listeAbo.withReadLock { toList() })
    }

    private fun setupKeyMap() {
        val inputMap = tabelle.inputMap
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), ACTION_MAP_KEY_EDIT_ABO)
        inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0), ACTION_MAP_KEY_DELETE_ABO)

        val actionMap = tabelle.actionMap
        actionMap.put(
            ACTION_MAP_KEY_EDIT_ABO,
            object : AbstractAction() {
                override fun actionPerformed(e: ActionEvent?) {
                    editAbo()
                }
            },
        )
        actionMap.put(
            ACTION_MAP_KEY_DELETE_ABO,
            object : AbstractAction() {
                override fun actionPerformed(e: ActionEvent?) {
                    aboLoeschen()
                }
            },
        )
    }

    private fun createContextMenu(): JPopupMenu {
        val itemEinschalten = JMenuItem("Abo einschalten")
        itemEinschalten.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/check.svg")
        itemEinschalten.addActionListener { changeAboActiveState(true) }

        val itemDeaktivieren = JMenuItem("Abo ausschalten")
        itemDeaktivieren.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/xmark.svg")
        itemDeaktivieren.addActionListener { changeAboActiveState(false) }

        val itemLoeschen = JMenuItem("Abo löschen")
        itemLoeschen.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/minus.svg")
        itemLoeschen.addActionListener { aboLoeschen() }

        val itemAendern = JMenuItem(EDIT_ABO_TEXT)
        itemAendern.icon = SVGIconUtilities.createSVGIcon("icons/fontawesome/pen-to-square.svg")
        itemAendern.addActionListener { editAbo() }

        val itemNeu = JMenuItem()
        itemNeu.action = createAboAction

        return JPopupMenu().apply {
            add(itemEinschalten)
            add(itemDeaktivieren)
            addSeparator()
            add(itemNeu)
            add(itemLoeschen)
            add(itemAendern)
        }
    }

    private fun installTableMouseHandler() {
        tabelle.addMouseListener(
            object : MouseAdapter() {
                override fun mousePressed(event: MouseEvent) {
                    if (showContextMenu(event)) {
                        return
                    }

                    if (event.clickCount == 2 &&
                        tabelle.selectedRow != -1 &&
                        tabelle.rowAtPoint(event.point) != -1
                    ) {
                        editAbo()
                    }
                }

                override fun mouseReleased(event: MouseEvent) {
                    showContextMenu(event)
                }
            },
        )
    }

    private fun showContextMenu(event: MouseEvent): Boolean {
        if (!event.isPopupTrigger) {
            return false
        }

        val row = tabelle.rowAtPoint(event.point)
        if (row != -1 && !tabelle.isRowSelected(row)) {
            tabelle.selectionModel.setSelectionInterval(row, row)
        } else if (row == -1) {
            tabelle.clearSelection()
        }

        createContextMenu().show(event.component, event.x, event.y)
        return true
    }

    private fun JToolBar.addToolbarButton(
        tooltip: String,
        iconPath: String,
        iconSize: Float? = null,
        action: () -> Unit,
    ) {
        val button = JButton().apply {
            toolTipText = tooltip
            addActionListener { action() }
            icon = if (iconSize == null) {
                SVGIconUtilities.createSVGIcon(iconPath)
            } else {
                SVGIconUtilities.createSVGIcon(iconPath, iconSize)
            }
        }
        add(button)
    }

    private class MinMaxCellRenderer : CenteredTextCellRenderer() {
        override fun getTableCellRendererComponent(
            table: JTable,
            value: Any?,
            isSelected: Boolean,
            hasFocus: Boolean,
            row: Int,
            column: Int
        ): Component {
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

            text = value?.toString().orEmpty()

            return this
        }
    }

    private class FilmCountCellRenderer : CenteredTextCellRenderer() {
        override fun getTableCellRendererComponent(
            table: JTable,
            value: Any?,
            isSelected: Boolean,
            hasFocus: Boolean,
            row: Int,
            column: Int
        ): Component {
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

            text = value?.toString().orEmpty()
            foreground = when {
                isSelected -> table.selectionForeground
                value == 0 -> Color.RED
                else -> table.foreground
            }

            return this
        }
    }

    private class SenderCellRenderer : CellRendererBase() {
        init {
            horizontalAlignment = CENTER
        }

        override fun getTableCellRendererComponent(
            table: JTable,
            value: Any?,
            isSelected: Boolean,
            hasFocus: Boolean,
            row: Int,
            column: Int
        ): Component {
            background = null
            foreground = null
            font = null
            icon = null

            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

            val abo = aboAtViewRow(table, row)
            when ((table as AboTable).showSenderIcons()) {
                true -> {
                    val targetDim: Dimension = getSenderCellDimension(table, row, column)
                    setSenderIcon(abo?.sender.orEmpty(), targetDim, isSelected)
                }
                false -> {
                    text = abo?.sender.orEmpty()
                    icon = null
                }
            }

            return this
        }
    }

    private class LastUsedCellRenderer : CenteredTextCellRenderer() {
        override fun getTableCellRendererComponent(
            table: JTable,
            value: Any?,
            isSelected: Boolean,
            hasFocus: Boolean,
            row: Int,
            column: Int
        ): Component {
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

            text = (value as? LocalDate)?.format(DateUtil.FORMATTER).orEmpty()

            val abo = aboAtViewRow(table, row)
            foreground = if (isSelected) {
                table.selectionForeground
            } else {
                colorForDate(abo?.downloadDate) ?: table.foreground
            }

            return this
        }

        private fun colorForDate(date: LocalDate?): Color? {
            date ?: return null
            val today = LocalDate.now()

            return when {
                date.isBefore(today.minusMonths(6)) -> Color.RED
                date.isBefore(today.minusMonths(3)) -> Color.YELLOW
                else -> null
            }
        }
    }

    private fun initListeners() {
        installTableMouseHandler()

        tabelle.columnModel.getColumn(DatenAbo.ABO_MINDESTDAUER).cellRenderer = CenteredTextCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_FILM_COUNT).cellRenderer = FilmCountCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_DOWN_DATUM).cellRenderer = LastUsedCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_MIN).cellRenderer = MinMaxCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_SENDER).cellRenderer = SenderCellRenderer()

        setupKeyMap()
    }

    private fun applySenderFilter() {
        val selectedItem = senderCombo.selectedItem?.toString()
        tableBinding.setSenderFilter(selectedItem)
        tableBinding.selectFirstRowIfNecessary()
    }

    private fun aboLoeschen() {
        val selectedAbos = tableBinding.selectedAbos
        if (selectedAbos.isNotEmpty()) {
            val text = if (selectedAbos.size == 1) {
                val abo = selectedAbos.first()
                "\"${abo.name}\" löschen?"
            } else {
                "Möchten Sie wirklich ${selectedAbos.size} Abos löschen?"
            }

            val ret = JOptionPane.showConfirmDialog(this, text, "Abo löschen", JOptionPane.YES_NO_OPTION)
            if (ret == JOptionPane.OK_OPTION) {
                try {
                    daten.listeAbo.removeAbosWithoutNotification(selectedAbos)
                } catch (e: Exception) {
                    logger.error("aboLoeschen", e)
                }
            }

            selectFirstRow()

            processAboChanges()
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun selectFirstRow() {
        tableBinding.selectFirstRowIfNecessary()
    }

    private fun changeAboActiveState(ein: Boolean) {
        val selectedAbos = tableBinding.selectedAbos
        if (selectedAbos.isNotEmpty()) {
            for (abo in selectedAbos) {
                abo.isActive = ein
                daten.listeAbo.fireAboChanged(abo)
            }
            tabelle.requestFocusInWindow()

            processAboChanges()
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun initComponents() {
        layout = BorderLayout()

        add(swingToolBar, BorderLayout.NORTH)
        add(scrollPane, BorderLayout.CENTER)
        add(infoPanel, BorderLayout.SOUTH)
    }

    private fun processAboChanges() {
        uiScope.launch {
            val progressJob = launch {
                delay(PROGRESS_PANEL_DELAY.milliseconds)
                infiniteProgressPanel.setText("Verarbeite Abos...")
                infiniteProgressPanel.start()
            }
            try {
                withContext(Dispatchers.Default) {
                    daten.listeAbo.aenderungMelden()
                }
            } finally {
                progressJob.cancel()
                infiniteProgressPanel.interrupt()
                infiniteProgressPanel.setText("")
            }
        }
    }

    private companion object {
        private const val EDIT_ABO_TEXT = "Abo ändern"
        private const val ACTION_MAP_KEY_EDIT_ABO = "edit_abo"
        private const val ACTION_MAP_KEY_DELETE_ABO = "delete_abo"
        private const val PROGRESS_PANEL_DELAY = 150L
        private val logger = LogManager.getLogger()

        private fun aboAtViewRow(table: JTable, viewRow: Int): DatenAbo? {
            val model = table.model as? AdvancedTableModel<*> ?: return null
            if (viewRow !in 0 until table.rowCount) {
                return null
            }

            val modelRow = table.convertRowIndexToModel(viewRow)
            if (modelRow !in 0 until model.rowCount) {
                return null
            }

            return model.getElementAt(modelRow) as? DatenAbo
        }
    }
}
