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

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.config.application.ApplicationConfiguration
import javax.swing.JTable
import javax.swing.table.TableColumn

internal class BlacklistRuleTableColumnSettings(
    private val table: JTable,
    private val stateStore: BlacklistRuleTableColumnStateStore = ApplicationBlacklistRuleTableColumnStateStore,
) {
    private val allColumns = mutableListOf<TableColumn>()

    init {
        table.autoResizeMode = JTable.AUTO_RESIZE_OFF
        setDefaultColumnWidths()
        captureColumns()
    }

    fun restore() {
        val rawState = stateStore.read().takeIf(String::isNotBlank) ?: return
        val state = runCatching { TABLE_STATE_JSON.decodeFromString<BlacklistRuleTableColumnState>(rawState) }
            .getOrNull()
            ?: return

        val restoredColumns = allColumns.map { column ->
            val saved = state.columns.firstOrNull { it.id == column.identifier.toString() }
            RestoredColumnState(
                column = column,
                width = saved?.width?.takeIf { it > 0 } ?: column.preferredWidth,
                position = saved?.position ?: column.modelIndex,
            )
        }

        while (table.columnModel.columnCount > 0) {
            table.columnModel.removeColumn(table.columnModel.getColumn(0))
        }

        restoredColumns
            .sortedWith(compareBy<RestoredColumnState> { it.position }.thenBy { it.column.modelIndex })
            .forEach { stateColumn ->
                table.columnModel.addColumn(stateColumn.column)
                stateColumn.column.preferredWidth = stateColumn.width
                stateColumn.column.width = stateColumn.width
            }
    }

    fun save() {
        val state = BlacklistRuleTableColumnState(
            columns = allColumns.map { column ->
                val viewIndex = currentViewIndex(column.modelIndex)
                BlacklistRuleColumnState(
                    id = column.identifier.toString(),
                    width = currentWidth(column, viewIndex),
                    position = if (viewIndex >= 0) viewIndex else column.modelIndex,
                )
            },
        )
        stateStore.write(TABLE_STATE_JSON.encodeToString(BlacklistRuleTableColumnState.serializer(), state))
    }

    private fun setDefaultColumnWidths() {
        for (index in 0 until table.columnModel.columnCount) {
            val column = table.columnModel.getColumn(index)
            column.identifier = table.model.getColumnName(column.modelIndex)
            column.preferredWidth = defaultColumnWidth(column.modelIndex)
        }
    }

    private fun captureColumns() {
        for (index in 0 until table.columnModel.columnCount) {
            allColumns += table.columnModel.getColumn(index)
        }
    }

    private fun currentViewIndex(modelIndex: Int): Int =
        (0 until table.columnModel.columnCount)
            .firstOrNull { table.columnModel.getColumn(it).modelIndex == modelIndex }
            ?: -1

    private fun currentWidth(column: TableColumn, viewIndex: Int): Int =
        viewIndex.takeIf { it >= 0 }
            ?.let { table.columnModel.getColumn(it).width }
            ?: column.width.takeIf { it > 0 }
            ?: column.preferredWidth

    private fun defaultColumnWidth(modelIndex: Int): Int =
        when (modelIndex) {
            BlacklistRuleTableModel.BLACKLIST_ACTIVE -> 70
            BlacklistRuleTableModel.BLACKLIST_SENDER -> 120
            BlacklistRuleTableModel.BLACKLIST_THEMA -> 280
            BlacklistRuleTableModel.BLACKLIST_TITEL -> 220
            BlacklistRuleTableModel.BLACKLIST_THEMA_TITEL -> 220
            BlacklistRuleTableModel.BLACKLIST_FILTERED -> 90
            else -> 120
        }

    private data class RestoredColumnState(
        val column: TableColumn,
        val width: Int,
        val position: Int,
    )

    private companion object {
        private val TABLE_STATE_JSON = Json {
            ignoreUnknownKeys = true
            encodeDefaults = true
        }
    }
}

internal interface BlacklistRuleTableColumnStateStore {
    fun read(): String
    fun write(state: String)
}

private object ApplicationBlacklistRuleTableColumnStateStore : BlacklistRuleTableColumnStateStore {
    override fun read(): String = ApplicationConfiguration.getInstance().blacklistTableColumnConfiguration

    override fun write(state: String) {
        ApplicationConfiguration.getInstance().blacklistTableColumnConfiguration = state
    }
}

@Serializable
private data class BlacklistRuleTableColumnState(
    val columns: List<BlacklistRuleColumnState> = emptyList(),
)

@Serializable
private data class BlacklistRuleColumnState(
    val id: String,
    val width: Int? = null,
    val position: Int? = null,
)
