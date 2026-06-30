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

package mediathek.tool.table

import mediathek.config.MVColor
import mediathek.controller.history.SeenHistoryController
import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.table.FilmColumnVisibility
import mediathek.tool.models.FilmColumn
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.awt.Component
import java.awt.event.MouseEvent
import javax.swing.table.TableCellRenderer
import javax.swing.table.TableColumnModel
import javax.swing.table.TableModel
import javax.swing.table.TableRowSorter

class MVFilmTable : PersistentColumnConfigurationTable(
    FilmColumn.PERSISTED_COLUMN_COUNT,
    FilmColumnVisibility.store(),
    TableConfigurationStores.FILM,
) {
    private var sorter: FilmRowSorter? = null
    private var selectedFilmIdentities: List<DatenFilm.FilmIdentity> = emptyList()
    private var selectionAnchorRow = -1

    init {
        autoCreateRowSorter = false
        addPropertyChangeListener("model") { event ->
            val newModel = event.newValue as? TableModel ?: return@addPropertyChangeListener
            val currentSorter = sorter
            if (currentSorter == null) {
                val createdSorter = FilmRowSorter(newModel)
                sorter = createdSorter
                rowSorter = createdSorter
            } else {
                currentSorter.model = newModel
            }
        }
    }

    override fun prepareRenderer(renderer: TableCellRenderer, row: Int, column: Int): Component {
        val component = super.prepareRenderer(renderer, row, column)
        if (!isRowSelected(row)) {
            decorateUnselectedRow(component, row)
        }
        return component
    }

    override fun getToolTipText(event: MouseEvent): String? {
        val point = event.point
        val viewColumn = columnAtPoint(point)
        val viewRow = rowAtPoint(point)

        if (!isTitleColumn(viewColumn)) {
            return super.getToolTipText(event)
        }

        return try {
            if (isTitleTruncatedAt(viewRow, viewColumn)) {
                filmAtViewRow(viewRow).title
            } else {
                null
            }
        } catch (_: RuntimeException) {
            null
        }
    }

    override fun resetTabelle() {
        for (column in 0 until maxSpalten) {
            resetFilmeTab(column)
        }

        rowSorter?.sortKeys = null
        spaltenAusschalten()
        setSpaltenEinAus(breite)
        setSpalten()
        calculateRowHeight()
    }

    override fun spaltenAusschalten() {
        // nothing to hide by default
    }

    override fun getSpalten() {
        saveSelectedTableRows()

        val columnCount = model.columnCount
        for (index in 0 until minOf(reihe.size, columnCount)) {
            reihe[index] = convertColumnIndexToModel(index)
        }

        val tableColumnModel = columnModel
        for (index in 0 until minOf(breite.size, columnCount)) {
            breite[index] = tableColumnModel.getColumn(convertColumnIndexToView(index)).width
        }

        listeSortKeys = rowSorter?.sortKeys
    }

    override fun setSpalten() {
        try {
            changeInternalColumnWidths()
            changeTableModelColumnWidths()
            reorderColumns()
            restoreSortKeys()
            restoreSelectedTableRows()
            refreshTableLayout()
        } catch (exception: Exception) {
            logger.error("setSpalten", exception)
        }
    }

    override fun saveSelectedTableRows() {
        super.saveSelectedTableRows()

        val selectedRows = selectedRows
        selectionAnchorRow = selectedRows.firstOrNull() ?: -1
        if (selectedRows.isEmpty()) {
            selectedFilmIdentities = emptyList()
            return
        }

        selectedFilmIdentities = selectedRows
            .asSequence()
            .filter { it in 0 until rowCount }
            .map { filmAtViewRow(it).filmIdentity }
            .toList()
    }

    override fun restoreSelectedTableRows() {
        if (selectedFilmIdentities.isEmpty()) {
            super.restoreSelectedTableRows()
            return
        }

        clearSelection()

        var firstVisibleRow = -1
        selectionModel.valueIsAdjusting = true
        try {
            for (viewRow in 0 until rowCount) {
                val filmIdentity = filmAtViewRow(viewRow).filmIdentity
                if (filmIdentity in selectedFilmIdentities) {
                    addRowSelectionInterval(viewRow, viewRow)
                    if (firstVisibleRow == -1) {
                        firstVisibleRow = viewRow
                    }
                }
            }
        } finally {
            selectionModel.valueIsAdjusting = false
        }

        if (firstVisibleRow != -1) {
            scrollToIndexDelegate(firstVisibleRow)
            requestFocusInWindow()
            return
        }

        if (rowCount > 0 && selectionAnchorRow >= 0) {
            val fallbackRow = minOf(selectionAnchorRow, rowCount - 1)
            selectionModel.setSelectionInterval(fallbackRow, fallbackRow)
            scrollToIndexDelegate(fallbackRow)
            requestFocusInWindow()
        }
    }

    private fun decorateUnselectedRow(component: Component, viewRow: Int) {
        val film = filmAtViewRow(viewRow)
        component.background = backgroundForRow(viewRow, film)
        component.foreground = foregroundFor(film)
    }

    private fun filmAtViewRow(viewRow: Int): DatenFilm =
        model.getValueAt(convertRowIndexToModel(viewRow), FilmColumn.REF.index) as DatenFilm

    private fun foregroundFor(film: DatenFilm): Color = if (film.isNew) MVColor.NEW_COLOR.color else foreground

    private fun backgroundForRow(viewRow: Int, film: DatenFilm): Color {
        val backgrounds = ArrayList<Color>(4)
        backgrounds.add(defaultRowBackground(viewRow))

        if (SeenHistoryController.hasBeenSeenFromSharedCache(film)) {
            backgrounds.add(MVColor.FILM_HISTORY.color)
        }
        if (film.isBookmarked) {
            backgrounds.add(MVColor.FILM_BOOKMARKED.color)
        }
        if (film.isDuplicate) {
            backgrounds.add(MVColor.FILM_DUPLICATE.color)
        }

        return backgrounds.firstOrNull()?.takeIf { backgrounds.size == 1 } ?: blend(backgrounds)
    }

    private fun isTitleColumn(viewColumn: Int): Boolean =
        viewColumn >= 0 && convertColumnIndexToModel(viewColumn) == FilmColumn.TITLE.index

    private fun isTitleTruncatedAt(viewRow: Int, viewColumn: Int): Boolean {
        val component = prepareRenderer(getCellRenderer(viewRow, viewColumn), viewRow, viewColumn)
        val bounds = getCellRect(viewRow, viewColumn, false)
        return component.preferredSize.width > bounds.width
    }

    private fun resetFilmeTab(column: Int) {
        reihe[column] = column
        breite[column] = defaultColumnWidth(column)
    }

    private fun defaultColumnWidth(column: Int): Int = when (FilmColumn.fromIndex(column)) {
        FilmColumn.NUMBER -> 75
        FilmColumn.TITLE -> 300
        FilmColumn.DATE,
        FilmColumn.TIME,
        FilmColumn.SENDER,
        FilmColumn.SIZE,
        FilmColumn.DURATION,
        FilmColumn.GEO,
            -> 100
        FilmColumn.URL -> 500
        FilmColumn.PLAY,
        FilmColumn.SAVE,
        FilmColumn.BOOKMARK,
            -> 20
        FilmColumn.HIGH_QUALITY,
        FilmColumn.SUBTITLE,
            -> 50
        else -> 200
    }

    private fun reorderColumns() {
        val tableColumnModel: TableColumnModel = columnModel
        val numberOfColumns = columnCount
        for (index in 0 until minOf(reihe.size, numberOfColumns)) {
            if (reihe[index] != index) {
                tableColumnModel.moveColumn(convertColumnIndexToView(reihe[index]), index)
            }
        }
    }

    private fun restoreSortKeys() {
        val savedSortKeys = listeSortKeys ?: return
        val currentSorter = rowSorter ?: return
        if (savedSortKeys !== currentSorter.sortKeys && savedSortKeys.isNotEmpty()) {
            currentSorter.sortKeys = savedSortKeys
        }
    }

    private class FilmRowSorter(model: TableModel) : TableRowSorter<TableModel>(model) {
        override fun setModel(model: TableModel) {
            super.setModel(model)
            configureSortableColumns()
            configureComparators()
        }

        override fun setSortKeys(sortKeys: MutableList<out SortKey>?) {
            super.setSortKeys(sortKeys?.take(1))
        }

        private fun configureSortableColumns() {
            setSortable(FilmColumn.PLAY.index, false)
            setSortable(FilmColumn.SAVE.index, false)
            setSortable(FilmColumn.GEO.index, false)
            setSortable(FilmColumn.BOOKMARK.index, false)
        }

        private fun configureComparators() {
            setComparator(FilmColumn.SIZE.index, Comparator<Int> { left, right -> left.compareTo(right) })
            setComparator(FilmColumn.SENDER.index, Comparator<String> { left, right -> left.compareTo(right) })
            setComparator(FilmColumn.TIME.index, Comparator<String> { left, right -> left.compareTo(right) })
            setComparator(FilmColumn.URL.index, Comparator<String> { left, right -> left.compareTo(right) })
            setComparator(FilmColumn.DURATION.index, Comparator<Int> { left, right -> left.compareTo(right) })
        }
    }

    companion object {
        private val logger = LogManager.getLogger()

        private fun blend(colors: Collection<Color>): Color {
            require(colors.isNotEmpty())

            var alpha = 0
            var red = 0
            var green = 0
            var blue = 0

            for (color in colors) {
                alpha += color.alpha
                red += color.red
                green += color.green
                blue += color.blue
            }

            val size = colors.size
            return Color(red / size, green / size, blue / size, alpha / size)
        }
    }
}
