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

package mediathek.gui.history

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.audiothek.ui.table.TriStateTableRowSorter
import mediathek.controller.history.AboHistoryController
import mediathek.controller.history.AboHistoryEntry
import mediathek.gui.messages.history.AboHistoryChangedEvent
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.GuiFunktionen
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import net.miginfocom.swing.MigLayout
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.Frame
import java.awt.Toolkit
import java.awt.event.KeyEvent
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import javax.swing.*
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.table.AbstractTableModel

class AboHistoryDialog(
    owner: Frame?,
    private val controller: AboHistoryController,
) : JDialog(owner, "Abo-Historie", true) {
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val deleteAction = object : AbstractAction("Einträge löschen") {
        override fun actionPerformed(e: java.awt.event.ActionEvent?) {
            deleteSelectedEntries()
        }
    }
    private val copyAction = object : AbstractAction("URL(s) kopieren") {
        override fun actionPerformed(e: java.awt.event.ActionEvent?) {
            copySelectedUrls()
        }
    }
    private val tableModel = AboHistoryTableModel()
    private val table = JTable(tableModel)
    private val sorter = TriStateTableRowSorter(tableModel)
    private val filterField = JTextField()
    private val summaryLabel = JLabel("", SwingConstants.LEFT)
    private val loadingLabel = JLabel("", SwingConstants.RIGHT)
    private val deleteButton = JButton("Einträge löschen")
    private val copyButton = JButton("URL(s) kopieren")
    private val closeButton = JButton("Schließen")

    init {
        defaultCloseOperation = DISPOSE_ON_CLOSE
        minimumSize = Dimension(860, 520)
        preferredSize = Dimension(1024, 640)

        buildUi()
        configureTable()
        configureActions()
        installHandlers()

        MessageBus.messageBus.subscribe(this)
        loadEntries()

        pack()
        setLocationRelativeTo(owner)
    }

    override fun dispose() {
        MessageBus.messageBus.unsubscribe(this)
        uiScope.cancel()
        super.dispose()
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleAboHistoryChanged(event: AboHistoryChangedEvent) {
        uiScope.launch {
            loadEntries()
        }
    }

    private fun buildUi() {
        layout = BorderLayout(0, 8)
        rootPane.border = BorderFactory.createEmptyBorder(12, 12, 12, 12)

        val toolbar = JPanel(MigLayout("insets 0, fillx", "[grow,fill][]", "[][]"))
        toolbar.add(JLabel("Filter:"), "split 2")
        toolbar.add(filterField, "growx, wrap")
        toolbar.add(summaryLabel, "growx")
        toolbar.add(loadingLabel, "alignx right")

        val buttonBar = JPanel(MigLayout("insets 0, fillx", "[grow,fill][][]", "[]"))
        buttonBar.add(JPanel(), "growx")
        buttonBar.add(copyButton)
        buttonBar.add(deleteButton)
        buttonBar.add(closeButton)

        val scrollPane = JScrollPane(table).apply {
            border = BorderFactory.createEmptyBorder()
        }

        add(toolbar, BorderLayout.NORTH)
        add(scrollPane, BorderLayout.CENTER)
        add(buttonBar, BorderLayout.SOUTH)
    }

    private fun configureTable() {
        table.autoCreateRowSorter = false
        table.rowSorter = sorter
        table.setSelectionMode(ListSelectionModel.MULTIPLE_INTERVAL_SELECTION)
        table.fillsViewportHeight = true
        table.autoResizeMode = JTable.AUTO_RESIZE_OFF
        table.setShowGrid(false)
        table.intercellSpacing = Dimension(0, 0)
        table.columnModel.getColumn(AboHistoryTableModel.COLUMN_DATE).preferredWidth = 110
        table.columnModel.getColumn(AboHistoryTableModel.COLUMN_THEME).preferredWidth = 180
        table.columnModel.getColumn(AboHistoryTableModel.COLUMN_TITLE).preferredWidth = 300
        table.columnModel.getColumn(AboHistoryTableModel.COLUMN_URL).preferredWidth = 520

        sorter.setComparator(AboHistoryTableModel.COLUMN_DATE) { left, right ->
            compareDateStrings(left?.toString().orEmpty(), right?.toString().orEmpty())
        }

        table.selectionModel.addListSelectionListener {
            if (!it.valueIsAdjusting) {
                updateUiState()
            }
        }

        val popupMenu = JPopupMenu().apply {
            add(JMenuItem(copyAction))
            add(JMenuItem(deleteAction))
        }
        table.componentPopupMenu = popupMenu
    }

    private fun configureActions() {
        deleteButton.action = deleteAction
        deleteButton.text = "Einträge löschen"

        copyButton.action = copyAction
        copyButton.text = "URL(s) kopieren"

        closeButton.addActionListener { dispose() }
        rootPane.defaultButton = closeButton

        val deleteKey = KeyStroke.getKeyStroke(KeyEvent.VK_DELETE, 0)
        table.inputMap.put(deleteKey, "delete-selected")
        table.actionMap.put("delete-selected", deleteAction)

        val copyKey = KeyStroke.getKeyStroke(KeyEvent.VK_C, Toolkit.getDefaultToolkit().menuShortcutKeyMaskEx)
        table.inputMap.put(copyKey, "copy-selected")
        table.actionMap.put("copy-selected", copyAction)
    }

    private fun installHandlers() {
        EscapeKeyHandler.installHandler(this) { dispose() }
        filterField.document.addDocumentListener(object : DocumentListener {
            override fun insertUpdate(e: DocumentEvent) = applyFilter()
            override fun removeUpdate(e: DocumentEvent) = applyFilter()
            override fun changedUpdate(e: DocumentEvent) = applyFilter()
        })
    }

    private fun loadEntries() {
        setLoading(true)
        uiScope.launch {
            val entries = withContext(Dispatchers.IO) { controller.getDataList() }
            tableModel.setEntries(entries)
            updateUiState()
            setLoading(false)
        }
    }

    private fun applyFilter() {
        val query = filterField.text.trim().lowercase()
        sorter.rowFilter = if (query.isBlank()) {
            null
        } else {
            object : RowFilter<AboHistoryTableModel, Int>() {
                override fun include(entry: Entry<out AboHistoryTableModel, out Int>): Boolean {
                    return (0 until entry.valueCount).any { column ->
                        entry.getStringValue(column).lowercase().contains(query)
                    }
                }
            }
        }
        updateUiState()
    }

    private fun selectedEntries(): List<AboHistoryEntry> =
        table.selectedRows
            .asSequence()
            .map(table::convertRowIndexToModel)
            .map(tableModel::getEntryAt)
            .toList()

    private fun deleteSelectedEntries() {
        val selectedEntries = selectedEntries()
        if (selectedEntries.isEmpty()) {
            return
        }

        val count = selectedEntries.size
        val message = if (count == 1) {
            "Soll der ausgewählte Eintrag aus der Abo-Historie gelöscht werden?"
        } else {
            "Sollen $count ausgewählte Einträge aus der Abo-Historie gelöscht werden?"
        }

        val confirmed = JOptionPane.showConfirmDialog(
            this,
            message,
            "Abo-Historie",
            JOptionPane.YES_NO_OPTION,
            JOptionPane.WARNING_MESSAGE
        ) == JOptionPane.YES_OPTION

        if (!confirmed) {
            return
        }

        val urls = selectedEntries.map(AboHistoryEntry::url)
        setLoading(true)
        uiScope.launch {
            val removedCount = withContext(Dispatchers.IO) { controller.removeUrls(urls) }
            if (removedCount == 0) {
                setLoading(false)
                updateUiState()
                return@launch
            }

            tableModel.removeUrls(urls.toSet())
            updateUiState()
            setLoading(false)
        }
    }

    private fun copySelectedUrls() {
        val selectedUrls = selectedEntries()
            .map(AboHistoryEntry::url)
            .distinct()
        if (selectedUrls.isEmpty()) {
            return
        }

        GuiFunktionen.copyToClipboard(selectedUrls.joinToString(System.lineSeparator()))
    }

    private fun updateUiState() {
        val selectedCount = table.selectedRowCount
        val visibleCount = table.rowCount
        val totalCount = tableModel.rowCount

        summaryLabel.text = buildString {
            append("Einträge: ").append(visibleCount)
            if (visibleCount != totalCount) {
                append(" von ").append(totalCount)
            }
            if (selectedCount > 0) {
                append(" | Ausgewählt: ").append(selectedCount)
            }
        }

        deleteAction.isEnabled = selectedCount > 0
        copyAction.isEnabled = selectedCount > 0
    }

    private fun setLoading(loading: Boolean) {
        loadingLabel.text = if (loading) "Lade..." else ""
        table.isEnabled = !loading
        deleteAction.isEnabled = !loading && table.selectedRowCount > 0
        copyAction.isEnabled = !loading && table.selectedRowCount > 0
        filterField.isEnabled = !loading
    }

    private fun compareDateStrings(left: String, right: String): Int {
        val leftDate = parseDate(left)
        val rightDate = parseDate(right)
        return when {
            leftDate != null && rightDate != null -> leftDate.compareTo(rightDate)
            leftDate != null -> 1
            rightDate != null -> -1
            else -> left.compareTo(right)
        }
    }

    private fun parseDate(value: String): LocalDate? {
        return try {
            LocalDate.parse(value, DATE_FORMATTER)
        } catch (_: DateTimeParseException) {
            null
        }
    }

    private class AboHistoryTableModel : AbstractTableModel() {
        private val entries = mutableListOf<AboHistoryEntry>()

        fun setEntries(newEntries: List<AboHistoryEntry>) {
            entries.clear()
            entries.addAll(newEntries)
            fireTableDataChanged()
        }

        fun getEntryAt(modelRow: Int): AboHistoryEntry = entries[modelRow]

        fun removeUrls(urlsToRemove: Set<String>) {
            if (urlsToRemove.isEmpty()) {
                return
            }
            entries.removeAll { it.url in urlsToRemove }
            fireTableDataChanged()
        }

        override fun getRowCount(): Int = entries.size

        override fun getColumnCount(): Int = COLUMN_NAMES.size

        override fun getColumnName(column: Int): String = COLUMN_NAMES[column]

        override fun getValueAt(rowIndex: Int, columnIndex: Int): String {
            val entry = entries[rowIndex]
            return when (columnIndex) {
                COLUMN_DATE -> entry.formattedDate
                COLUMN_THEME -> entry.theme
                COLUMN_TITLE -> entry.title
                COLUMN_URL -> entry.url
                else -> throw IllegalArgumentException("Unknown column $columnIndex")
            }
        }

        companion object {
            const val COLUMN_DATE = 0
            const val COLUMN_THEME = 1
            const val COLUMN_TITLE = 2
            const val COLUMN_URL = 3

            private val COLUMN_NAMES = arrayOf("Datum", "Thema", "Titel", "URL")
        }
    }

    private companion object {
        private val DATE_FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
    }
}
