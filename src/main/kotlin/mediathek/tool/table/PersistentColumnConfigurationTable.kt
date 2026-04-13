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

import mediathek.config.MVConfig
import java.util.*
import javax.swing.RowSorter.SortKey
import javax.swing.SortOrder

abstract class PersistentColumnConfigurationTable protected constructor(
    maxColumns: Int,
    visibleColumnStore: ColumnVisibilityStore,
    showIconsConfigKey: Optional<MVConfig.Configs>,
    smallSenderIconConfigKey: Optional<MVConfig.Configs>,
    private val columnConfigurationDataConfigKey: MVConfig.Configs,
) : MVTable(maxColumns, visibleColumnStore, showIconsConfigKey, smallSenderIconConfigKey) {

    /**
     * Tabelle das erste Mal initialisieren mit den gespeicherten Daten oder den Standardwerten.
     * Erst die Breite, dann die Reihenfolge.
     */
    fun readColumnConfigurationData() {
        try {
            val keyDataStr = MVConfig.get(columnConfigurationDataConfigKey)
            val configurationData = parseColumnConfigurationData(keyDataStr)
            if (configurationData == null) {
                resetTabelle()
                return
            }

            applyColumnConfigurationData(configurationData)
        } catch (_: Exception) {
            resetTabelle()
        }
    }

    override fun writeTableConfigurationData() {
        super.writeTableConfigurationData()
        MVConfig.add(columnConfigurationDataConfigKey, prepareTableConfigurationData())
    }

    /**
     * Prepare the configuration data.
     * @return the configuration data as string.
     */
    private fun prepareTableConfigurationData(): String {
        val order = IntArray(maxSpalten)
        val widths = IntArray(maxSpalten)
        for (i in 0 until minOf(order.size, model.columnCount)) {
            order[i] = convertColumnIndexToModel(i)
        }

        val tableColumnModel = columnModel
        for (i in 0 until minOf(widths.size, model.columnCount)) {
            widths[i] = tableColumnModel.getColumn(convertColumnIndexToView(i)).width
        }

        val widthData = StringBuilder(widths[0].toString())
        val orderData = StringBuilder(order[0].toString())
        for (i in 1 until widths.size) {
            widthData.append(',').append(widths[i])
            orderData.append(',').append(order[i])
        }

        val sortKeys = rowSorter?.sortKeys
        listeSortKeys = sortKeys
        var sortKeyColumn = ""
        var sortOrder = ""
        if (!sortKeys.isNullOrEmpty()) {
            val sortKey = sortKeys.first()
            sortKeyColumn = sortKey.column.toString()
            sortOrder = if (sortKey.sortOrder == SortOrder.ASCENDING) SORT_ASCENDING else SORT_DESCENDING
        }

        return "$widthData$FELDTRENNER$orderData$FELDTRENNER$sortKeyColumn$FELDTRENNER$sortOrder"
    }

    private fun parseColumnConfigurationData(configurationData: String): ColumnConfigurationData? {
        if (configurationData.isEmpty()) {
            return null
        }

        val parts = configurationData.split(FELDTRENNER, ignoreCase = false, limit = 5)
        if (parts.size != 4) {
            return null
        }

        val widths = parseColumnConfigurationList(parts[0]) ?: return null
        val order = parseColumnOrder(parts[1]) ?: return null
        val sortKey = parseSortKey(parts[2], parts[3])

        return ColumnConfigurationData(widths.toList(), order.toList(), sortKey)
    }

    private fun parseColumnConfigurationList(savedColumns: String): List<Int>? {
        if (maxSpalten.toLong() != countNumberOfColumns(savedColumns)) {
            // dann hat sich die Anzahl der Spalten der Tabelle geändert: Versionswechsel
            return null
        }

        val columnValues = savedColumns.split(",", ignoreCase = false, limit = maxSpalten + 1)
        val columns = ArrayList<Int>(maxSpalten)
        for (value in columnValues) {
            columns.add(value.toIntOrNull() ?: return null)
        }
        return columns
    }

    private fun parseColumnOrder(savedColumns: String): List<Int>? {
        val order = parseColumnConfigurationList(savedColumns) ?: return null
        return if (isValidColumnOrder(order)) order else null
    }

    private fun isValidColumnOrder(columnOrder: List<Int>): Boolean {
        val columnCount = model.columnCount
        if (columnCount > columnOrder.size) {
            return false
        }

        val seenColumns = BooleanArray(columnCount)
        for (i in 0 until columnCount) {
            val column = columnOrder[i]
            if (column !in 0 until columnCount || seenColumns[column]) {
                return false
            }
            seenColumns[column] = true
        }
        return true
    }

    private fun parseSortKey(sortColumn: String, sortOrder: String): SortKey? {
        if (sortColumn.isEmpty() && sortOrder.isEmpty()) {
            return null
        }

        val column = sortColumn.toIntOrNull() ?: return null
        if (column < 0 || column >= model.columnCount || rowSorter == null) {
            return null
        }

        val order = when (sortOrder) {
            SORT_ASCENDING -> SortOrder.ASCENDING
            SORT_DESCENDING -> SortOrder.DESCENDING
            else -> return null
        }

        return SortKey(column, order)
    }

    private fun applyColumnConfigurationData(configurationData: ColumnConfigurationData) {
        copyToArray(configurationData.widths, breite)
        copyToArray(configurationData.order, reihe)
        val savedSortKeys = configurationData.sortKey?.let(::listOf).orEmpty()

        listeSortKeys = null
        setSpaltenEinAus(breite)
        setSpalten()
        applySortKeys(savedSortKeys)

        calculateRowHeight()
    }

    private fun applySortKeys(sortKeys: List<SortKey>) {
        val tableRowSorter = rowSorter ?: return
        try {
            tableRowSorter.sortKeys = sortKeys
            listeSortKeys = sortKeys
        } catch (_: IllegalArgumentException) {
            listeSortKeys = emptyList<SortKey>()
        }
    }

    private data class ColumnConfigurationData(
        val widths: List<Int>,
        val order: List<Int>,
        val sortKey: SortKey?,
    )

    companion object {
        private const val FELDTRENNER = "|"
        private const val SORT_ASCENDING = "ASCENDING"
        private const val SORT_DESCENDING = "DESCENDING"

        /**
         * Count the number of saved columns within the string.
         * Counts the number of comma separated entries.
         * @param s The string to be processed.
         * @return The number of columns included.
         */
        @JvmStatic
        fun countNumberOfColumns(s: String): Long = s.count { it == ',' }.toLong() + 1

        private fun copyToArray(source: List<Int>, target: IntArray) {
            for (i in target.indices) {
                target[i] = source[i]
            }
        }
    }
}
