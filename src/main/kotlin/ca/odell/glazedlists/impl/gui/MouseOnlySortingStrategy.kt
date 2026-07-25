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
package ca.odell.glazedlists.impl.gui

/**
 * @see ca.odell.glazedlists.gui.AbstractTableComparatorChooser.SINGLE_COLUMN
 * @see ca.odell.glazedlists.gui.AbstractTableComparatorChooser.MULTIPLE_COLUMN_MOUSE
 */
internal class MouseOnlySortingStrategy(
    private val multipleColumnSort: Boolean,
) : SortingStrategy {
    override fun supportsMultipleColumnSorting(): Boolean = multipleColumnSort

    /** Adjusts the sorting state based on receiving the specified clicks. */
    override fun <E : Any> columnClicked(
        sortingState: SortingState<E>,
        column: Int,
        clicks: Int,
        shift: Boolean,
        control: Boolean,
    ) {
        val clickedColumn = sortingState.columns[column]
        if (clickedColumn.comparators.isEmpty()) return

        val recentlyClickedColumns = sortingState.recentlyClickedColumns

        if (clicks == 2) {
            for (sortingColumn in recentlyClickedColumns) {
                sortingColumn.clear()
            }
            recentlyClickedColumns.clear()
        } else if (!multipleColumnSort) {
            for (sortingColumn in recentlyClickedColumns) {
                if (sortingColumn !== clickedColumn) {
                    sortingColumn.clear()
                }
            }
            recentlyClickedColumns.clear()
        }

        val netClicks = 1 + clickedColumn.comparatorIndex * 2 + if (clickedColumn.isReverse) 1 else 0
        clickedColumn.comparatorIndex = (netClicks / 2) % clickedColumn.comparators.size
        clickedColumn.isReverse = netClicks % 2 == 1
        if (!recentlyClickedColumns.contains(clickedColumn)) {
            recentlyClickedColumns.add(clickedColumn)
        }

        sortingState.fireSortingChanged()
    }
}
