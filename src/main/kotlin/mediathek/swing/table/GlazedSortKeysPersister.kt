/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.swing.table

import ca.odell.glazedlists.gui.AbstractTableComparatorChooser.SortKey
import ca.odell.glazedlists.swing.TableComparatorChooser
import kotlinx.serialization.SerialName
import kotlinx.serialization.Serializable
import kotlinx.serialization.builtins.ListSerializer
import kotlinx.serialization.json.Json
import mediathek.config.application.ApplicationConfiguration
import org.apache.logging.log4j.LogManager

/**
 * Persist and restore the sort-keys from a GlazedLists TableComparatorChooser.
 */
class GlazedSortKeysPersister<E : Any>(
    private val configPrefix: String,
    private val chooser: TableComparatorChooser<E>
) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()

    fun saveSortState() {
        val infos = chooser.sortKeys.map { key ->
            SortKeyInfo(
                column = key.column,
                comparatorIndex = key.comparatorIndex,
                descending = key.reverse,
            )
        }
        try {
            val json = serializer.encodeToString(ListSerializer(SortKeyInfo.serializer()), infos)
            applicationConfiguration.setGlazedTableSortKeys(configPrefix, json)
        } catch (ex: Exception) {
            LOG.error("Failed to save sort keys", ex)
        }
    }

    fun restoreSortState() {
        try {
            val json = applicationConfiguration.getGlazedTableSortKeys(configPrefix)
            if (json.isBlank()) return

            val infos = serializer.decodeFromString(ListSerializer(SortKeyInfo.serializer()), json)
            chooser.setSortKeys(infos.map { info -> SortKey(info.column, info.comparatorIndex, info.descending) })
        } catch (_: NoSuchElementException) {
        } catch (ex: Exception) {
            LOG.error("Failed to restore sort keys", ex)
        }
    }

    @Serializable
    data class SortKeyInfo(
        @SerialName("column")
        val column: Int = 0,
        @SerialName("comparatorIndex")
        val comparatorIndex: Int = 0,
        @SerialName("descending")
        val descending: Boolean = false
    )

    companion object {
        private val LOG = LogManager.getLogger()
        private val serializer = Json {
            ignoreUnknownKeys = true
        }
    }
}
