/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package mediathek.gui.tabs.tab_film.table

import ca.odell.glazedlists.gui.AdvancedTableFormat
import ca.odell.glazedlists.gui.TableFormat
import ca.odell.glazedlists.swing.AdvancedTableModel
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.daten.DatenFilm
import java.util.*
import java.util.concurrent.atomic.AtomicLong
import javax.swing.DefaultListSelectionModel
import javax.swing.JTable
import javax.swing.ListSelectionModel
import javax.swing.SwingUtilities
import javax.swing.table.AbstractTableModel
import javax.swing.table.DefaultTableModel

/** Non-suspending row and selection operations are confined to the Swing EDT. */
interface FilmTableModelBinding {
    val table: JTable
    val rowCount: Int

    fun filmAtViewRow(viewRow: Int): DatenFilm?
    fun selectedFilms(): List<DatenFilm>
    suspend fun replaceFilms(films: Collection<DatenFilm>)
    fun removeFilms(films: Collection<DatenFilm>): Boolean
    fun repaintVisibleRows()
    fun dispose()
}

class FilmTableBinding(
    override val table: JTable,
) : FilmTableModelBinding {
    private val tableFormat = FilmTableFormat()
    private val tableModel = SnapshotFilmTableModel(tableFormat)
    private val selectionModel = DefaultListSelectionModel()
    private val modelDispatcher = Dispatchers.Default.limitedParallelism(1)
    private val modelScope = CoroutineScope(SupervisorJob() + modelDispatcher)
    private val updateGeneration = AtomicLong()
    private var sourceFilms: List<DatenFilm> = emptyList()
    private val excludedFilms = Collections.newSetFromMap(IdentityHashMap<DatenFilm, Boolean>())

    @Volatile
    private var disposed = false

    internal val sorting: FilmTableSorting
        field: FilmTableSortController

    init {
        table.autoCreateRowSorter = false
        table.rowSorter = null
        table.model = tableModel
        selectionModel.selectionMode = ListSelectionModel.MULTIPLE_INTERVAL_SELECTION
        table.selectionModel = selectionModel
        sorting = FilmTableSortController(table, tableFormat, ::scheduleResort)
    }

    override val rowCount: Int
        get() {
            checkEdt()
            return tableModel.rowCount
        }

    override fun filmAtViewRow(viewRow: Int): DatenFilm? {
        checkEdt()
        return viewRow.takeIf { it in 0 until tableModel.rowCount }?.let(tableModel::getElementAt)
    }

    override fun selectedFilms(): List<DatenFilm> {
        checkEdt()
        return table.selectedRows
            .asSequence()
            .mapNotNull(::filmAtViewRow)
            .toList()
    }

    override suspend fun replaceFilms(films: Collection<DatenFilm>) {
        if (disposed) {
            return
        }

        val replacement = films as? List<DatenFilm> ?: films.toList()
        val selection = withContext(Dispatchers.Swing) { captureSelection() }
        val requestedGeneration = updateGeneration.incrementAndGet()
        var selectionRows = IntArray(0)
        var displayedFilms: List<DatenFilm> = emptyList()
        withContext(modelDispatcher) {
            if (requestedGeneration != updateGeneration.get()) {
                return@withContext
            }
            excludedFilms.clear()
            sourceFilms = replacement
            displayedFilms = prepareDisplayedFilms()
            if (disposed || requestedGeneration != updateGeneration.get()) {
                return@withContext
            }
            selectionRows = findSelectedRows(displayedFilms, selection)
        }
        withContext(Dispatchers.Swing) {
            if (!disposed && requestedGeneration == updateGeneration.get()) {
                tableModel.replaceElements(displayedFilms)
                restoreSelection(selection, selectionRows)
            }
        }
    }

    override fun removeFilms(films: Collection<DatenFilm>): Boolean {
        checkEdt()
        if (disposed || films.isEmpty()) {
            return false
        }

        val filmsToRemove = films.toList()
        val selection = captureSelection()
        val requestedGeneration = updateGeneration.incrementAndGet()
        modelScope.launch {
            excludedFilms.addAll(filmsToRemove)
            val displayedFilms = prepareDisplayedFilms()
            if (disposed || requestedGeneration != updateGeneration.get()) {
                return@launch
            }
            val selectionRows = findSelectedRows(displayedFilms, selection)
            withContext(Dispatchers.Swing) {
                if (!disposed && requestedGeneration == updateGeneration.get()) {
                    tableModel.replaceElements(displayedFilms)
                    restoreSelection(selection, selectionRows)
                }
            }
        }
        return true
    }

    override fun repaintVisibleRows() {
        checkEdt()
        if (!disposed) {
            table.repaint()
        }
    }

    override fun dispose() {
        if (disposed) {
            return
        }
        disposed = true
        updateGeneration.incrementAndGet()
        modelScope.cancel()
        runOnEdtAndWait {
            sorting.dispose()
            table.clearSelection()
            table.selectionModel = DefaultListSelectionModel()
            table.rowSorter = null
            table.model = DefaultTableModel()
            tableModel.dispose()
        }
        // A cancelled background preparation may still be unwinding, so comparator state is left intact for GC.
    }

    private fun captureSelection(): SelectionSnapshot = SelectionSnapshot(
        films = selectedFilms(),
        anchorRow = table.selectionModel.anchorSelectionIndex.takeIf { it >= 0 } ?: table.selectedRow,
    )

    private fun restoreSelection(snapshot: SelectionSnapshot, selectedRows: IntArray) {
        table.clearSelection()
        var firstSelectedRow = -1
        selectionModel.valueIsAdjusting = true
        try {
            for (row in selectedRows) {
                if (row in 0 until tableModel.rowCount) {
                    table.addRowSelectionInterval(row, row)
                    if (firstSelectedRow == -1) {
                        firstSelectedRow = row
                    }
                }
            }
            if (firstSelectedRow == -1 && tableModel.rowCount > 0) {
                firstSelectedRow = snapshot.anchorRow.coerceAtLeast(0).coerceAtMost(tableModel.rowCount - 1)
                table.selectionModel.setSelectionInterval(firstSelectedRow, firstSelectedRow)
            }
        } finally {
            selectionModel.valueIsAdjusting = false
        }

        if (firstSelectedRow >= 0) {
            table.scrollRectToVisible(table.getCellRect(firstSelectedRow, 0, true))
            table.requestFocusInWindow()
        }
    }

    private fun scheduleResort() {
        if (disposed) {
            return
        }
        val selection = captureSelection()
        val requestedGeneration = updateGeneration.incrementAndGet()
        modelScope.launch {
            val displayedFilms = prepareDisplayedFilms()
            if (disposed || requestedGeneration != updateGeneration.get()) {
                return@launch
            }
            val selectionRows = findSelectedRows(displayedFilms, selection)
            withContext(Dispatchers.Swing) {
                if (!disposed && requestedGeneration == updateGeneration.get()) {
                    tableModel.replaceElements(displayedFilms)
                    restoreSelection(selection, selectionRows)
                }
            }
        }
    }

    private fun prepareDisplayedFilms(): List<DatenFilm> {
        val comparator = sorting.comparator()
        if (excludedFilms.isEmpty() && comparator == null) {
            return sourceFilms
        }

        val result = if (excludedFilms.isEmpty()) {
            ArrayList(sourceFilms)
        } else {
            sourceFilms.filterTo(ArrayList(sourceFilms.size)) { it !in excludedFilms }
        }
        if (comparator != null) {
            result.sortWith(comparator)
        }
        return result
    }

    private fun findSelectedRows(films: List<DatenFilm>, selection: SelectionSnapshot): IntArray {
        if (selection.films.isEmpty()) {
            return IntArray(0)
        }

        val selectedReferences = Collections.newSetFromMap(IdentityHashMap<DatenFilm, Boolean>()).apply {
            addAll(selection.films)
        }
        val matchedReferences = Collections.newSetFromMap(IdentityHashMap<DatenFilm, Boolean>())
        val selectedRows = ArrayList<Int>(selectedReferences.size)
        films.forEachIndexed { index, film ->
            if (film in selectedReferences) {
                selectedRows.add(index)
                matchedReferences.add(film)
            }
        }

        if (matchedReferences.size == selectedReferences.size) {
            return selectedRows.toIntArray()
        }

        val missingIdentitiesByUrl = selection.films
            .asSequence()
            .filterNot { it in matchedReferences }
            .map(DatenFilm::filmIdentity)
            .groupBy(DatenFilm.FilmIdentity::storedNormalQualityUrl)
        films.forEachIndexed { index, film ->
            if (film in matchedReferences) {
                return@forEachIndexed
            }
            val candidates = missingIdentitiesByUrl[film.storedNormalQualityUrl] ?: return@forEachIndexed
            if (candidates.any { identity -> film.matches(identity) }) {
                selectedRows.add(index)
            }
        }
        selectedRows.sort()
        return selectedRows.toIntArray()
    }

    private fun DatenFilm.matches(identity: DatenFilm.FilmIdentity): Boolean =
        sender == identity.sender &&
                thema == identity.thema &&
                storedNormalQualityUrl == identity.storedNormalQualityUrl &&
                storedWebsiteUrl == identity.storedWebsiteUrl

    private fun runOnEdtAndWait(action: () -> Unit) {
        if (SwingUtilities.isEventDispatchThread()) {
            action()
        } else {
            SwingUtilities.invokeAndWait(action)
        }
    }

    private fun checkEdt() {
        check(SwingUtilities.isEventDispatchThread()) { "Film table access must run on the Swing EDT" }
    }

    private data class SelectionSnapshot(
        val films: List<DatenFilm>,
        val anchorRow: Int,
    )

    private class SnapshotFilmTableModel(
        private var format: AdvancedTableFormat<DatenFilm>,
    ) : AbstractTableModel(), AdvancedTableModel<DatenFilm> {
        private var elements: List<DatenFilm> = emptyList()

        override var tableFormat: TableFormat<in DatenFilm>
            get() = format
            set(value) {
                require(value is AdvancedTableFormat<*>)
                @Suppress("UNCHECKED_CAST")
                format = value as AdvancedTableFormat<DatenFilm>
                fireTableStructureChanged()
            }

        override fun getElementAt(index: Int): DatenFilm = elements[index]

        override fun getRowCount(): Int = elements.size

        override fun getColumnCount(): Int = format.getColumnCount()

        override fun getColumnName(column: Int): String = format.getColumnName(column)

        override fun getColumnClass(columnIndex: Int): Class<*> = format.getColumnClass(columnIndex)

        override fun getValueAt(rowIndex: Int, columnIndex: Int): Any? =
            format.getColumnValue(elements[rowIndex], columnIndex)

        fun replaceElements(replacement: List<DatenFilm>) {
            check(SwingUtilities.isEventDispatchThread())
            elements = replacement
            fireTableDataChanged()
        }

        override fun dispose() {
            elements = emptyList()
        }
    }

}
