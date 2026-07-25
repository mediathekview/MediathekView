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

package mediathek.gui.dialogEinstellungen.blacklist

import mediathek.daten.DatenFilm
import mediathek.daten.blacklist.BlacklistRule
import mediathek.daten.blacklist.CompiledBlacklistMatcher
import mediathek.daten.blacklist.ListeBlacklist
import javax.swing.table.AbstractTableModel

class BlacklistRuleTableModel(
    private val blacklist: ListeBlacklist,
) : AbstractTableModel() {
    private var filteredCounts = IntArray(0)

    override fun getRowCount(): Int =
        blacklist.size

    override fun getColumnCount(): Int =
        COLUMN_COUNT

    override fun getValueAt(rowIndex: Int, columnIndex: Int): Any {
        val rule = blacklist[rowIndex]
        return when (columnIndex) {
            BLACKLIST_ACTIVE -> rule.active
            BLACKLIST_SENDER -> rule.sender
            BLACKLIST_THEMA -> rule.thema
            BLACKLIST_TITEL -> rule.titel
            BLACKLIST_THEMA_TITEL -> rule.topicTitle
            BLACKLIST_FILTERED -> getFilteredCount(rowIndex)
            else -> error("Unexpected value: $columnIndex")
        }
    }

    override fun getColumnName(column: Int): String =
        when (column) {
            BLACKLIST_ACTIVE -> "aktiv"
            BLACKLIST_SENDER -> "Sender"
            BLACKLIST_THEMA -> "Thema"
            BLACKLIST_TITEL -> "Titel"
            BLACKLIST_THEMA_TITEL -> "Thema-Titel"
            BLACKLIST_FILTERED -> "gefiltert"
            else -> error("Unexpected value: $column")
        }

    override fun getColumnClass(columnIndex: Int): Class<*> =
        when (columnIndex) {
            BLACKLIST_ACTIVE -> Boolean::class.javaObjectType
            BLACKLIST_FILTERED -> Int::class.javaObjectType
            else -> String::class.java
        }

    fun ruleRemoved(modelIndex: Int) {
        fireTableRowsDeleted(modelIndex, modelIndex)
    }

    fun rulesChanged() {
        fireTableDataChanged()
    }

    fun ruleInserted(rowIndex: Int) {
        fireTableRowsInserted(rowIndex, rowIndex)
    }

    fun ruleUpdated(modelIndex: Int) {
        fireTableRowsUpdated(modelIndex, modelIndex)
    }

    fun calculateFilteredCounts(films: List<DatenFilm>): IntArray {
        val rules = blacklistSnapshot()
        return if (rules.isEmpty()) {
            IntArray(0)
        } else {
            CompiledBlacklistMatcher(rules).countMatchesByRule(films)
        }
    }

    fun applyFilteredCounts(counts: IntArray) {
        filteredCounts = counts
        if (rowCount > 0) {
            fireTableRowsUpdated(0, rowCount - 1)
        }
    }

    fun getRule(fromModelIndex: Int): BlacklistRule {
        val rule = blacklist[fromModelIndex]
        return BlacklistRule(
            rule.sender,
            rule.thema,
            rule.titel,
            rule.topicTitle,
            rule.active,
        )
    }

    fun hasZeroFilteredCount(modelIndex: Int): Boolean =
        getFilteredCount(modelIndex) == 0

    fun hasNoFilteredFilms(modelIndex: Int): Boolean =
        hasZeroFilteredCount(modelIndex)

    private fun getFilteredCount(rowIndex: Int): Int =
        filteredCounts.getOrElse(rowIndex) { 0 }

    private fun blacklistSnapshot(): List<BlacklistRule> =
        synchronized(blacklist) {
            blacklist.toList()
        }

    companion object {
        internal const val BLACKLIST_ACTIVE = 0
        internal const val BLACKLIST_SENDER = 1
        internal const val BLACKLIST_THEMA = 2
        internal const val BLACKLIST_TITEL = 3
        internal const val BLACKLIST_THEMA_TITEL = 4
        internal const val BLACKLIST_FILTERED = 5
        internal const val COLUMN_COUNT = 6
    }
}
