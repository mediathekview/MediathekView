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

import ca.odell.glazedlists.FilterList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.matchers.AbstractMatcherEditor
import ca.odell.glazedlists.swing.AdvancedTableModel
import ca.odell.glazedlists.swing.DefaultEventSelectionModel
import ca.odell.glazedlists.swing.GlazedListsSwing
import ca.odell.glazedlists.swing.TableComparatorChooser
import mediathek.daten.ListeAbo
import mediathek.daten.abo.DatenAbo
import mediathek.swing.table.GlazedSortKeysPersister
import mediathek.tool.withReadLock
import org.apache.logging.log4j.LogManager
import javax.swing.DefaultListSelectionModel
import javax.swing.JTable
import javax.swing.ListSelectionModel
import javax.swing.table.DefaultTableModel

class AboTableBinding(
    private val table: JTable,
    sourceList: ListeAbo,
    filmCountProvider: (DatenAbo) -> Int? = { 0 },
) {
    private val tableFormat = AboTableFormat(filmCountProvider)
    private val senderMatcherEditor = SenderAboMatcherEditor()
    private val filteredAbos = FilterList(sourceList, senderMatcherEditor)
    private val sortedAbos = SortedList(filteredAbos)
    private val tableModel: AdvancedTableModel<DatenAbo> =
        GlazedListsSwing.eventTableModelWithThreadProxyList(sortedAbos, tableFormat)
    private val selectionModel = DefaultEventSelectionModel(sortedAbos)
    private val comparatorChooser: TableComparatorChooser<DatenAbo>
    private val sortPersister: GlazedSortKeysPersister<DatenAbo>
    private var disposed = false

    init {
        table.autoCreateRowSorter = false
        table.rowSorter = null
        table.model = tableModel
        selectionModel.selectionMode = ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
        table.selectionModel = selectionModel

        comparatorChooser = TableComparatorChooser.install(table, sortedAbos, TableComparatorChooser.SINGLE_COLUMN, tableFormat)
        sortPersister = GlazedSortKeysPersister(SORT_CONFIG_PREFIX, comparatorChooser)
        sortPersister.restoreSortState()
        comparatorChooser.addSortActionListener { sortPersister.saveSortState() }
    }

    val selectedAbos: List<DatenAbo>
        get() = selectionModel.selected.withReadLock { toList() }

    val selectedAboCount: Int
        get() = selectionModel.selected.size

    fun addSelectionListener(listener: (Boolean) -> Unit) {
        selectionModel.addListSelectionListener { event ->
            if (!event.valueIsAdjusting) {
                listener(selectionModel.selected.isEmpty())
            }
        }
    }

    fun setSenderFilter(sender: String?) {
        val selectedBeforeFilter = selectedAbos
        val selectedSender = sender.orEmpty()
        selectionModel.valueIsAdjusting = true
        try {
            senderMatcherEditor.setMatchSet(
                if (selectedSender.isEmpty()) emptySet() else setOf(selectedSender),
            )
            restoreSelection(selectedBeforeFilter)
        } finally {
            selectionModel.valueIsAdjusting = false
        }
    }

    private fun restoreSelection(previouslySelected: List<DatenAbo>) {
        selectionModel.clearSelection()
        for ((index, abo) in swingAbos.withIndex()) {
            if (previouslySelected.any { selectedAbo -> selectedAbo === abo }) {
                selectionModel.addSelectionInterval(index, index)
            }
        }
    }

    fun aboAtViewRow(viewRow: Int): DatenAbo? {
        if (viewRow !in 0 until table.rowCount) {
            return null
        }

        val modelRow = table.convertRowIndexToModel(viewRow)
        if (modelRow !in 0 until tableModel.rowCount) {
            return null
        }

        return tableModel.getElementAt(modelRow)
    }

    fun selectFirstRowIfNecessary() {
        if (table.rowCount > 0 && table.selectedRow == -1) {
            table.requestFocusInWindow()
            table.selectionModel.setSelectionInterval(0, 0)
        }
    }

    fun clearSorting() {
        comparatorChooser.clearComparator()
        sortPersister.saveSortState()
    }

    fun saveSortState() {
        if (!disposed) {
            sortPersister.saveSortState()
        }
    }

    fun dispose() {
        if (disposed) {
            return
        }
        disposed = true

        table.selectionModel = DefaultListSelectionModel()
        table.rowSorter = null
        table.model = DefaultTableModel()

        runCatching { comparatorChooser.dispose() }
            .onFailure { logger.debug("Ignoring already disposed abo table comparator chooser", it) }
        runCatching { selectionModel.dispose() }
            .onFailure { logger.debug("Ignoring already disposed abo table selection model", it) }
        runCatching { tableModel.dispose() }
            .onFailure { logger.debug("Ignoring already disposed abo table model", it) }
        runCatching { sortedAbos.dispose() }
            .onFailure { logger.debug("Ignoring already disposed abo sorted list", it) }
        runCatching { filteredAbos.dispose() }
            .onFailure { logger.debug("Ignoring already disposed abo filtered list", it) }
    }

    private class SenderAboMatcherEditor : AbstractMatcherEditor<DatenAbo>() {
        var sender: String = ""
            set(value) {
                if (field == value) {
                    return
                }

                field = value
                if (value.isEmpty()) {
                    fireMatchAll()
                } else {
                    fireChanged { abo -> abo.sender == value }
                }
            }
    }

    private companion object {
        private const val SORT_CONFIG_PREFIX = "abo-v3"
        private val logger = LogManager.getLogger()
    }
}
