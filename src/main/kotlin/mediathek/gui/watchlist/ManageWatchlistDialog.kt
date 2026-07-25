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

package mediathek.gui.watchlist

import mediathek.daten.watchlist.DatenWatchlistEntry
import mediathek.daten.watchlist.WatchlistServices
import mediathek.gui.messages.WatchlistChangedEvent
import mediathek.swing.SwingDispatch
import mediathek.tool.EscapeKeyHandler
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import java.awt.BorderLayout
import java.awt.Dimension
import java.awt.FlowLayout
import javax.swing.*
import javax.swing.table.AbstractTableModel

/**
 * Modal dialog to view and delete the watchlist entries of the film tab.
 */
class ManageWatchlistDialog(
    owner: JFrame,
    private val watchlist: WatchlistServices,
) : JDialog(owner) {
    private val tableModel = WatchlistTableModel(watchlist)
    private val table = JTable(tableModel)
    private val deleteButton = JButton("Löschen")

    init {
        title = "Watchlist verwalten"
        defaultCloseOperation = DISPOSE_ON_CLOSE
        isResizable = true
        isModal = true

        deleteButton.isEnabled = false
        deleteButton.addActionListener { deleteSelectedEntries() }
        table.selectionModel.addListSelectionListener {
            deleteButton.isEnabled = table.selectedRowCount > 0
        }

        val buttonPanel = JPanel(FlowLayout(FlowLayout.RIGHT)).apply {
            add(deleteButton)
            add(JButton("Schließen").apply { addActionListener { dispose() } })
        }

        contentPane.layout = BorderLayout()
        contentPane.add(JScrollPane(table), BorderLayout.CENTER)
        contentPane.add(buttonPanel, BorderLayout.SOUTH)
        preferredSize = Dimension(560, 320)
        pack()
        setLocationRelativeTo(owner)

        tableModel.refresh()
        MessageBus.messageBus.subscribe(this)
        EscapeKeyHandler.installHandler(this) { dispose() }
    }

    override fun dispose() {
        MessageBus.messageBus.unsubscribe(this)
        super.dispose()
    }

    @Handler
    @Suppress("UNUSED_PARAMETER")
    fun handleWatchlistChangedEvent(event: WatchlistChangedEvent) {
        SwingDispatch.dispatch { tableModel.refresh() }
    }

    private fun deleteSelectedEntries() {
        val selectedEntries = table.selectedRows.map { row -> tableModel.entryAt(row) }
        selectedEntries.forEach(watchlist::removeEntry)
        tableModel.refresh()
    }
}

private class WatchlistTableModel(
    private val watchlist: WatchlistServices,
) : AbstractTableModel() {
    private var entries: List<DatenWatchlistEntry> = emptyList()

    fun refresh() {
        entries = watchlist.entriesSnapshot()
        fireTableDataChanged()
    }

    fun entryAt(row: Int): DatenWatchlistEntry = entries[row]

    override fun getRowCount(): Int = entries.size

    override fun getColumnCount(): Int = 3

    override fun getColumnName(column: Int): String =
        when (column) {
            0 -> "Sender"
            1 -> "Thema"
            else -> "Titel"
        }

    override fun getValueAt(row: Int, column: Int): String {
        val entry = entries[row]
        return when (column) {
            0 -> entry.sender
            1 -> entry.thema
            else -> entry.title
        }
    }
}
