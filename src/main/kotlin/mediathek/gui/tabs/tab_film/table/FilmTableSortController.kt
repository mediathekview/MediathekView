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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser
import ca.odell.glazedlists.gui.AbstractTableComparatorChooser.SortKey
import ca.odell.glazedlists.swing.TableComparatorChooser
import mediathek.daten.DatenFilm
import mediathek.swing.table.GlazedSortKeysPersister
import mediathek.tool.models.FilmColumn
import mediathek.tool.withReadLock
import javax.swing.JTable

internal interface FilmTableSorting {
    fun restoreLegacySort(column: Int, descending: Boolean)
    fun clear()
    fun save()
}

/**
 * Owns the GlazedLists header/comparator integration without putting the large film snapshot in an EventList.
 * The empty sorted list is comparator state only; FilmTableBinding applies its comparator off the EDT.
 */
internal class FilmTableSortController(
    table: JTable,
    tableFormat: FilmTableFormat,
    private val onSortChanged: () -> Unit,
) : FilmTableSorting {
    private val sortControlSource = BasicEventList<DatenFilm>()
    private val sortedFilms = SortedList(sortControlSource, null)
    private val comparatorChooser = TableComparatorChooser.install(
        table,
        sortedFilms,
        AbstractTableComparatorChooser.SINGLE_COLUMN,
        tableFormat,
    )
    private val sortPersister = GlazedSortKeysPersister(SORT_CONFIG_PREFIX, comparatorChooser)

    init {
        NON_SORTABLE_COLUMNS.forEach { column ->
            comparatorChooser.disableSortingForColumn(column.index)
        }
        sortPersister.restoreSortState()
        comparatorChooser.addSortActionListener {
            save()
            onSortChanged()
        }
    }

    fun comparator(): Comparator<in DatenFilm>? = sortedFilms.withReadLock { sortedFilms.comparator }

    override fun restoreLegacySort(column: Int, descending: Boolean) {
        if (column !in FilmColumn.entries.indices || FilmColumn.fromIndex(column) in NON_SORTABLE_COLUMNS) {
            return
        }
        if (!comparatorChooser.setSortKeys(listOf(SortKey(column, 0, descending)))) {
            save()
        }
    }

    override fun clear() {
        comparatorChooser.clearComparator()
        save()
    }

    override fun save() {
        sortPersister.saveSortState()
    }

    fun dispose() {
        comparatorChooser.dispose()
    }

    private companion object {
        private const val SORT_CONFIG_PREFIX = "film"
        private val NON_SORTABLE_COLUMNS = setOf(
            FilmColumn.PLAY,
            FilmColumn.SAVE,
            FilmColumn.BOOKMARK,
            FilmColumn.GEO,
        )
    }
}
