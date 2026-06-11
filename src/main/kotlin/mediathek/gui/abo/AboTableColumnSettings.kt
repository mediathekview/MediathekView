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

package mediathek.gui.abo

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.abo.DatenAbo
import mediathek.tool.JsonStringUtils
import org.apache.logging.log4j.LogManager
import java.util.regex.Pattern
import javax.swing.JCheckBoxMenuItem
import javax.swing.JMenuItem
import javax.swing.JPopupMenu
import javax.swing.table.TableColumn

class AboTableColumnSettings(
    private val table: AboTable,
    private val clearSorting: () -> Unit,
) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private val allColumns = mutableListOf<TableColumn>()
    private val settings = mutableListOf<ColumnSetting>()

    init {
        val columnModel = table.columnModel
        for (index in 0 until columnModel.columnCount) {
            val column = columnModel.getColumn(index)
            column.identifier = table.model.getColumnName(index)
            allColumns += column
            settings += ColumnSetting(
                id = column.identifier.toString(),
                position = index,
                width = defaultWidth(index),
                visible = index !in DEFAULT_HIDDEN_COLUMNS,
            )
            column.preferredWidth = defaultWidth(index)
        }
    }

    fun load() {
        val persistedSettings = try {
            parseColumnSettingsJson(applicationConfiguration.getTableColumnSettings(CONFIG_PREFIX))
        } catch (_: NoSuchElementException) {
            emptyList()
        }
        for (persistedSetting in persistedSettings) {
            val existing = settings.firstOrNull { setting -> setting.id == persistedSetting.id }
            if (existing != null) {
                existing.position = persistedSetting.position
                existing.width = persistedSetting.width
                existing.visible = persistedSetting.visible
            }
        }

        rebuildColumnModel()
    }

    fun save() {
        val columnModel = table.columnModel
        for (setting in settings) {
            val column = allColumns.firstOrNull { tableColumn -> tableColumn.identifier.toString() == setting.id } ?: continue
            val visible = isInModel(column)
            setting.visible = visible
            if (visible) {
                setting.position = columnModel.getColumnIndex(setting.id)
                setting.width = column.width
            }
        }

        try {
            applicationConfiguration.setTableColumnSettings(CONFIG_PREFIX, toColumnSettingsJson(settings))
            table.saveDisplaySettings()
        } catch (ex: RuntimeException) {
            logger.error("Failed to save abo table column settings", ex)
        }
    }

    fun installContextMenu() {
        val popup = JPopupMenu()
        for (column in allColumns) {
            val columnName = column.identifier.toString()
            val setting = settings.firstOrNull { it.id == columnName } ?: continue
            val item = JCheckBoxMenuItem(columnName, setting.visible)
            item.addActionListener {
                setting.visible = item.isSelected
                if (item.isSelected) {
                    showColumn(column, setting.position)
                } else {
                    hideColumn(column)
                }
                save()
            }
            popup.add(item)
        }

        popup.addSeparator()
        val showSenderIconsItem = JCheckBoxMenuItem("Sendericons anzeigen", table.showSenderIcons())
        showSenderIconsItem.addActionListener {
            table.setShowSenderIcons(showSenderIconsItem.isSelected)
            save()
        }
        popup.add(showSenderIconsItem)

        val smallSenderIconsItem = JCheckBoxMenuItem("Kleine Sendericons anzeigen", table.getUseSmallSenderIcons())
        smallSenderIconsItem.addActionListener {
            table.setUseSmallSenderIcons(smallSenderIconsItem.isSelected)
            save()
        }
        popup.add(smallSenderIconsItem)

        popup.addSeparator()
        val resetSortingItem = JMenuItem("Sortierschlüssel zurücksetzen")
        resetSortingItem.addActionListener { clearSorting() }
        popup.add(resetSortingItem)

        table.tableHeader.componentPopupMenu = popup
    }

    private fun rebuildColumnModel() {
        val columnModel = table.columnModel
        while (columnModel.columnCount > 0) {
            columnModel.removeColumn(columnModel.getColumn(0))
        }

        settings.asSequence()
            .filter { setting -> setting.visible }
            .sortedBy { setting -> setting.position }
            .forEach { setting ->
                allColumns.firstOrNull { column -> column.identifier.toString() == setting.id }?.let { column ->
                    columnModel.addColumn(column)
                    column.preferredWidth = setting.width
                }
            }
    }

    private fun showColumn(column: TableColumn, preferredPosition: Int) {
        if (isInModel(column)) {
            return
        }

        val columnModel = table.columnModel
        columnModel.addColumn(column)
        val lastIndex = columnModel.columnCount - 1
        columnModel.moveColumn(lastIndex, preferredPosition.coerceIn(0, lastIndex))
    }

    private fun hideColumn(column: TableColumn) {
        if (isInModel(column)) {
            table.columnModel.removeColumn(column)
        }
    }

    private fun isInModel(column: TableColumn): Boolean {
        val columnModel = table.columnModel
        for (index in 0 until columnModel.columnCount) {
            if (columnModel.getColumn(index) == column) {
                return true
            }
        }
        return false
    }

    private fun defaultWidth(modelIndex: Int): Int =
        when (modelIndex) {
            DatenAbo.ABO_EINGESCHALTET,
            DatenAbo.ABO_MIN,
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY,
            DatenAbo.ABO_FILM_COUNT,
            -> 75

            DatenAbo.ABO_DOWN_DATUM,
            DatenAbo.ABO_SENDER,
            -> 100

            else -> 200
        }

    private fun parseColumnSettingsJson(json: String?): List<ColumnSetting> {
        val trimmed = json?.trim().orEmpty()
        if (!trimmed.startsWith("[") || !trimmed.endsWith("]")) {
            return emptyList()
        }

        val parsedSettings = mutableListOf<ColumnSetting>()
        var index = 1
        while (index < trimmed.length - 1) {
            if (trimmed[index].isWhitespace() || trimmed[index] == ',') {
                index++
                continue
            }
            if (trimmed[index] != '{') {
                index++
                continue
            }

            val objectStart = index
            var depth = 0
            var inString = false
            var escaping = false
            var objectEnd = -1
            while (index < trimmed.length - 1) {
                val character = trimmed[index]
                if (escaping) {
                    escaping = false
                    index++
                    continue
                }
                if (character == '\\' && inString) {
                    escaping = true
                    index++
                    continue
                }
                if (character == '"') {
                    inString = !inString
                    index++
                    continue
                }
                if (inString) {
                    index++
                    continue
                }

                when (character) {
                    '{' -> depth++
                    '}' -> {
                        depth--
                        if (depth == 0) {
                            objectEnd = index
                            break
                        }
                    }
                }
                index++
            }

            if (objectEnd < 0) {
                break
            }
            parseColumnSettingObject(trimmed.substring(objectStart, objectEnd + 1))?.let(parsedSettings::add)
            index = objectEnd + 1
        }
        return parsedSettings
    }

    private fun parseColumnSettingObject(objectJson: String): ColumnSetting? {
        val id = extractJsonStringValue(objectJson, "id")
        val position = extractInt(POSITION_PATTERN, objectJson)
        val width = extractInt(WIDTH_PATTERN, objectJson)
        val visible = extractBoolean(VISIBLE_PATTERN, objectJson)

        if (id == null || position == null || width == null || visible == null) {
            return null
        }
        return ColumnSetting(id, position, width, visible)
    }

    private fun toColumnSettingsJson(settings: List<ColumnSetting>): String =
        buildString {
            append('[')
            settings.forEachIndexed { index, setting ->
                if (index > 0) {
                    append(',')
                }
                append('{')
                append("\"id\":\"")
                append(JsonStringUtils.escapeJsonString(setting.id))
                append('"')
                append(",\"position\":")
                append(setting.position)
                append(",\"width\":")
                append(setting.width)
                append(",\"visible\":")
                append(setting.visible)
                append('}')
            }
            append(']')
        }

    private fun extractJsonStringValue(json: String, key: String): String? {
        val fieldName = "\"$key\""
        val keyIndex = json.indexOf(fieldName)
        if (keyIndex < 0) {
            return null
        }

        val colonIndex = json.indexOf(':', keyIndex + fieldName.length)
        if (colonIndex < 0) {
            return null
        }

        var valueStart = colonIndex + 1
        while (valueStart < json.length && json[valueStart].isWhitespace()) {
            valueStart++
        }
        return JsonStringUtils.parseQuotedJsonString(json, valueStart)?.value
    }

    private fun extractInt(pattern: Pattern, json: String): Int? {
        val matcher = pattern.matcher(json)
        if (!matcher.find()) {
            return null
        }
        return matcher.group(1).toIntOrNull()
    }

    private fun extractBoolean(pattern: Pattern, json: String): Boolean? {
        val matcher = pattern.matcher(json)
        if (!matcher.find()) {
            return null
        }
        return matcher.group(1).toBoolean()
    }

    private data class ColumnSetting(
        var id: String,
        var position: Int,
        var width: Int,
        var visible: Boolean,
    )

    private companion object {
        private const val CONFIG_PREFIX = "abo"
        private val DEFAULT_HIDDEN_COLUMNS = setOf(
            DatenAbo.ABO_ZIELPFAD,
            DatenAbo.ABO_PSET,
            DatenAbo.ABO_MINDESTDAUER,
            DatenAbo.ABO_MIN,
            DatenAbo.ABO_DOWN_DATUM,
        )
        private val POSITION_PATTERN: Pattern = Pattern.compile("\"position\"\\s*:\\s*(-?\\d+)")
        private val WIDTH_PATTERN: Pattern = Pattern.compile("\"width\"\\s*:\\s*(-?\\d+)")
        private val VISIBLE_PATTERN: Pattern = Pattern.compile("\"visible\"\\s*:\\s*(true|false)")
        private val logger = LogManager.getLogger()
    }
}
