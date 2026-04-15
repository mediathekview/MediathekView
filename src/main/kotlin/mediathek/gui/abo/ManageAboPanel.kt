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

import ca.odell.glazedlists.swing.GlazedListsSwing
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.audiothek.ui.table.CenteredTextCellRenderer
import mediathek.config.Daten
import mediathek.daten.abo.AboTags
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.gui.actions.CreateNewAboAction
import mediathek.gui.dialog.DialogAboNoSet
import mediathek.gui.dialog.DialogEditAbo
import mediathek.gui.messages.AboListChangedEvent
import mediathek.mainwindow.MediathekGui
import mediathek.swing.InfiniteProgressPanel
import mediathek.tool.EventListWithEmptyFirstEntry
import mediathek.tool.MessageBus
import mediathek.tool.NoSelectionErrorDialog
import mediathek.tool.SVGIconUtilities
import mediathek.tool.cellrenderer.CellRendererBase
import mediathek.tool.datum.DateUtil
import mediathek.tool.listener.BeobTableHeader
import mediathek.tool.models.TModelAbo
import mediathek.tool.table.MVAbosTable
import mediathek.tool.table.MVTable
import mediathek.tool.table.PersistentColumnConfigurationTable
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

class ManageAboPanel(dialog: JDialog) : JPanel() {
    private val tabelle: PersistentColumnConfigurationTable = MVAbosTable()
    private val daten = Daten.getInstance()
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val createAboAction = CreateNewAboAction(daten.listeAbo)
    private val infoPanel = JXStatusBar()
    private val totalAbos = JLabel("totalAbos")
    private val activeAbos = JLabel("activeAbos")
    private val inactiveAbos = JLabel("inactiveAbos")
    private val swingToolBar = JToolBar()
    private val senderCombo = JComboBox<String>()
    private val infiniteProgressPanel = InfiniteProgressPanel()
    private val btnEditAbo = JButton()
    private val scrollPane = JScrollPane(tabelle)

    init {
        initComponents()

        setupToolBar()
        setupInfoPanel()
        updateInfoText()

        MessageBus.messageBus.subscribe(this)

        initListeners()
        initializeTable()

        tabelle.selectionModel.addListSelectionListener {
            btnEditAbo.isEnabled = tabelle.selectedRows.size <= 1
        }

        tabelle.addMouseListener(
            object : MouseAdapter() {
                override fun mousePressed(mouseEvent: MouseEvent) {
                    if (mouseEvent.clickCount == 2 &&
                        tabelle.selectedRow != -1 &&
                        tabelle.rowAtPoint(mouseEvent.point) != -1
                    ) {
                        editAbo()
                    }
                }
            },
        )

        dialog.glassPane = infiniteProgressPanel
    }

    fun tabelleSpeichern() {
        tabelle.writeTableConfigurationData()
    }

    override fun removeNotify() {
        uiScope.cancel()
        super.removeNotify()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleAboListChanged(event: AboListChangedEvent) {
        SwingUtilities.invokeLater {
            tabelleLaden()
            updateInfoText()
        }
    }

    fun editAbo() {
        if (tabelle.selectedRowCount == 0) {
            NoSelectionErrorDialog.show(this)
            return
        }

        val rows = tabelle.selectedRows
        var modelRow = tabelle.convertRowIndexToModel(tabelle.selectedRow)
        val editedAbo = tabelle.model.getValueAt(modelRow, DatenAbo.ABO_REF) as DatenAbo

        if (!DialogAboNoSet.ensureAboProgramSetAvailable(MediathekGui.ui())) {
            return
        }

        val dialog = DialogEditAbo(MediathekGui.ui(), editedAbo, tabelle.selectedRowCount > 1)
        dialog.title = EDIT_ABO_TEXT
        dialog.isVisible = true
        if (!dialog.successful()) {
            return
        }

        if (tabelle.selectedRowCount > 1) {
            for (row in rows) {
                for (b in dialog.multiEditCbIndices.indices) {
                    if (!dialog.multiEditCbIndices[b]) {
                        continue
                    }

                    modelRow = tabelle.convertRowIndexToModel(row)
                    val curSelAbo = tabelle.model.getValueAt(modelRow, DatenAbo.ABO_REF) as DatenAbo

                    AboTags.fromIndex(b).ifPresent { tag ->
                        when (tag) {
                            AboTags.EINGESCHALTET -> curSelAbo.isActive = editedAbo.isActive
                            AboTags.MINDESTDAUER -> curSelAbo.mindestDauerMinuten = editedAbo.mindestDauerMinuten
                            AboTags.MIN -> curSelAbo.filmLengthState = editedAbo.filmLengthState
                            AboTags.ZIELPFAD -> curSelAbo.zielpfad = editedAbo.zielpfad
                            AboTags.PSET -> curSelAbo.psetName = editedAbo.psetName
                            AboTags.DO_NOT_START_AUTOMATICALLY ->
                                curSelAbo.isDoNotStartAutomatically = editedAbo.isDoNotStartAutomatically
                            else -> logger.error("Unhandled tag called {}", tag)
                        }
                    }
                }
            }
        }

        tabelleLaden()
        processAboChanges()
    }

    private fun setupInfoPanel() {
        infoPanel.add(totalAbos)
        infoPanel.add(activeAbos)
        infoPanel.add(inactiveAbos)
    }

    private fun initializeTable() {
        tabelleLaden()
        tabelle.readColumnConfigurationData()
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
        senderCombo.addActionListener { tabelleLaden() }
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

            val abo = table.model.getValueAt(table.convertRowIndexToModel(row), DatenAbo.ABO_REF) as DatenAbo
            text = when (abo.filmLengthState) {
                FilmLengthState.MINIMUM -> "min"
                else -> "max"
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

            val abo = table.model.getValueAt(table.convertRowIndexToModel(row), DatenAbo.ABO_REF) as DatenAbo
            when ((table as MVTable).showSenderIcons()) {
                true -> {
                    val targetDim: Dimension = getSenderCellDimension(table, row, column)
                    setSenderIcon(abo.sender, targetDim, isSelected)
                }
                false -> {
                    text = abo.sender
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

            val abo = table.model.getValueAt(table.convertRowIndexToModel(row), DatenAbo.ABO_REF) as DatenAbo
            foreground = if (isSelected) {
                table.selectionForeground
            } else {
                colorForDate(abo.downDatum) ?: table.foreground
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
        tabelle.componentPopupMenu = createContextMenu()

        tabelle.model = TModelAbo(daten.listeAbo)
        tabelle.columnModel.getColumn(DatenAbo.ABO_NR).cellRenderer = CenteredTextCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_MINDESTDAUER).cellRenderer = CenteredTextCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_DOWN_DATUM).cellRenderer = LastUsedCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_MIN).cellRenderer = MinMaxCellRenderer()
        tabelle.columnModel.getColumn(DatenAbo.ABO_SENDER).cellRenderer = SenderCellRenderer()

        tabelle.isLineBreak = false
        tabelle.tableHeader.addMouseListener(
            BeobTableHeader(
                tabelle,
                DatenAbo.getColumnVisibilityStore(),
                intArrayOf(DatenAbo.ABO_EINGESCHALTET, DatenAbo.ABO_REF),
                intArrayOf(),
                true,
                null,
            ),
        )

        setupKeyMap()
    }

    private fun tabelleLaden() {
        tabelle.getSpalten()

        val selectedItem = senderCombo.selectedItem?.toString()
        if (selectedItem != null) {
            (tabelle.model as TModelAbo).setSenderFilter(selectedItem)
            tabelle.setSpalten()
        }
    }

    private fun aboLoeschen() {
        val rows = tabelle.selectedRows
        if (rows.isNotEmpty()) {
            val text = if (rows.size == 1) {
                val delRow = tabelle.convertRowIndexToModel(rows[0])
                val abo = tabelle.model.getValueAt(delRow, DatenAbo.ABO_REF) as DatenAbo
                "\"${abo.name}\" löschen?"
            } else {
                "Möchten Sie wirklich ${rows.size} Abos löschen?"
            }

            val ret = JOptionPane.showConfirmDialog(this, text, "Abo löschen", JOptionPane.YES_NO_OPTION)
            if (ret == JOptionPane.OK_OPTION) {
                try {
                    val listeAbo = daten.listeAbo
                    for (row in rows) {
                        val modelRow = tabelle.convertRowIndexToModel(row)
                        val abo = tabelle.model.getValueAt(modelRow, DatenAbo.ABO_REF) as DatenAbo
                        listeAbo.remove(abo)
                    }
                } catch (e: Exception) {
                    logger.error("aboLoeschen", e)
                }
            }
            tabelleLaden()

            selectFirstRow()

            processAboChanges()
        } else {
            NoSelectionErrorDialog.show(this)
        }
    }

    private fun selectFirstRow() {
        if (tabelle.rowCount > 0 && tabelle.selectedRow == -1) {
            tabelle.requestFocus()
            tabelle.setRowSelectionInterval(0, 0)
        }
    }

    private fun changeAboActiveState(ein: Boolean) {
        val rows = tabelle.selectedRows
        if (rows.isNotEmpty()) {
            for (row in rows) {
                val modelRow = tabelle.convertRowIndexToModel(row)
                val abo = tabelle.model.getValueAt(modelRow, DatenAbo.ABO_REF) as DatenAbo
                abo.isActive = ein
            }
            tabelleLaden()
            tabelle.clearSelection()
            tabelle.requestFocus()
            for (row in rows) {
                tabelle.addRowSelectionInterval(row, row)
            }

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
    }
}
