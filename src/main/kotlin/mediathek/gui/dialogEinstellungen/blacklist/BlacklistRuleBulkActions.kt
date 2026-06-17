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

import mediathek.daten.blacklist.ListeBlacklist

internal object BlacklistRuleBulkActions {
    fun deactivateActiveRulesWithZeroFilteredCount(
        blacklist: ListeBlacklist,
        tableModel: BlacklistRuleTableModel,
    ): List<Int> {
        val changedRows = mutableListOf<Int>()
        for (modelIndex in 0 until tableModel.rowCount) {
            val rule = tableModel.getRule(modelIndex)
            if (rule.active && tableModel.hasZeroFilteredCount(modelIndex)) {
                if (blacklist.replaceAtIfUniqueWithoutNotification(modelIndex, rule.copy(active = false))) {
                    changedRows += modelIndex
                }
            }
        }
        return changedRows
    }

    fun removeRulesWithZeroFilteredCount(
        blacklist: ListeBlacklist,
        tableModel: BlacklistRuleTableModel,
    ): Boolean {
        val rules = rulesWithZeroFilteredCount(tableModel)
        return rules.isNotEmpty() && blacklist.removeAllWithoutNotification(rules)
    }

    private fun rulesWithZeroFilteredCount(tableModel: BlacklistRuleTableModel) =
        (0 until tableModel.rowCount)
            .filter(tableModel::hasZeroFilteredCount)
            .map(tableModel::getRule)
}
