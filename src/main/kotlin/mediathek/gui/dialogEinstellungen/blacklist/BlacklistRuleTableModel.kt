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
    private val filmsSupplier: () -> List<DatenFilm>,
) : AbstractTableModel() {
    private var filteredCounts = IntArray(0)

    init {
        updateFilteredCounts()
    }

    override fun getRowCount(): Int =
        blacklist.size

    override fun getColumnCount(): Int =
        COLUMN_COUNT

    override fun getValueAt(rowIndex: Int, columnIndex: Int): Any {
        val rule = blacklist[rowIndex]
        return when (columnIndex) {
            BLACKLIST_SENDER -> rule.sender
            BLACKLIST_THEMA -> rule.thema
            BLACKLIST_TITEL -> rule.titel
            BLACKLIST_THEMA_TITEL -> rule.thema_titel
            BLACKLIST_FILTERED -> getFilteredCount(rowIndex)
            else -> error("Unexpected value: $columnIndex")
        }
    }

    override fun getColumnName(column: Int): String =
        when (column) {
            BLACKLIST_SENDER -> "Sender"
            BLACKLIST_THEMA -> "Thema"
            BLACKLIST_TITEL -> "Titel"
            BLACKLIST_THEMA_TITEL -> "Thema-Titel"
            BLACKLIST_FILTERED -> "gefiltert"
            else -> error("Unexpected value: $column")
        }

    override fun getColumnClass(columnIndex: Int): Class<*> =
        when (columnIndex) {
            BLACKLIST_FILTERED -> Int::class.javaObjectType
            else -> String::class.java
        }

    fun removeRow(modelIndex: Int) {
        blacklist.removeAt(modelIndex)
        updateFilteredCounts()
        fireTableRowsDeleted(modelIndex, modelIndex)
    }

    fun removeRules(rules: List<BlacklistRule>) {
        blacklist.remove(rules)
        updateFilteredCounts()
        fireTableDataChanged()
    }

    fun removeAll() {
        blacklist.clear()
        updateFilteredCounts()
        fireTableDataChanged()
    }

    fun addRule(rule: BlacklistRule) {
        val rowIndex = blacklist.size
        blacklist.add(rule)
        updateFilteredCounts()
        fireTableRowsInserted(rowIndex, rowIndex)
    }

    fun contains(rule: BlacklistRule): Boolean =
        blacklist.contains(rule)

    fun updateRule(modelIndex: Int, updatedRule: BlacklistRule) {
        val rule = blacklist[modelIndex]
        rule.sender = updatedRule.sender
        rule.thema = updatedRule.thema
        rule.titel = updatedRule.titel
        rule.thema_titel = updatedRule.thema_titel

        blacklist.filterListAndNotifyListeners()
        updateFilteredCounts()
        fireTableRowsUpdated(modelIndex, modelIndex)
    }

    fun refreshFilteredCounts() {
        updateFilteredCounts()
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
            rule.thema_titel,
        )
    }

    fun hasNoFilteredFilms(modelIndex: Int): Boolean =
        getFilteredCount(modelIndex) == 0

    private fun getFilteredCount(rowIndex: Int): Int =
        filteredCounts.getOrElse(rowIndex) { 0 }

    private fun updateFilteredCounts() {
        val rules = blacklistSnapshot()
        filteredCounts = if (rules.isEmpty()) {
            IntArray(0)
        } else {
            CompiledBlacklistMatcher(rules).countMatchesByRule(filmsSupplier())
        }
    }

    private fun blacklistSnapshot(): List<BlacklistRule> =
        synchronized(blacklist) {
            blacklist.toList()
        }

    private companion object {
        private const val BLACKLIST_SENDER = 0
        private const val BLACKLIST_THEMA = 1
        private const val BLACKLIST_TITEL = 2
        private const val BLACKLIST_THEMA_TITEL = 3
        private const val BLACKLIST_FILTERED = 4
        private const val COLUMN_COUNT = 5
    }
}
