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

package mediathek.gui.bookmark

import ca.odell.glazedlists.swing.TableComparatorChooser
import mediathek.swing.IconUtils
import mediathek.swing.IconizedCheckBoxMenuItem
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.JsonStringUtils
import mediathek.tool.withLock
import org.apache.commons.configuration2.sync.LockMode
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import org.kordamp.ikonli.materialdesign2.MaterialDesignE
import org.kordamp.ikonli.materialdesign2.MaterialDesignN
import java.util.regex.Pattern
import javax.swing.JCheckBoxMenuItem
import javax.swing.JMenuItem
import javax.swing.JPopupMenu
import javax.swing.JTable
import javax.swing.table.TableColumn
import javax.swing.table.TableColumnModel

open class BookmarkTableColumnSettingsManager<E>(
    protected val table: JTable,
    private val configPrefix: String,
    protected val comparatorChooser: TableComparatorChooser<E>?,
) {
    protected val allColumns = mutableListOf<TableColumn>()
    protected val lastSettings = mutableListOf<ColumnSetting>()

    init {
        val columnModel = table.columnModel
        for (i in 0 until columnModel.columnCount) {
            val column = columnModel.getColumn(i)
            allColumns += column
            lastSettings += ColumnSetting(
                id = column.identifier.toString(),
                position = i,
                width = column.width,
                visible = true,
            )
        }
    }

    private fun createMenuItem(columnName: String, visible: Boolean): JCheckBoxMenuItem =
        when {
            columnName.equals("Gesehen", ignoreCase = true) ->
                IconizedCheckBoxMenuItem(IconUtils.of(MaterialDesignE.EYE), visible)

            columnName.equals("Notiz", ignoreCase = true) ->
                IconizedCheckBoxMenuItem(IconUtils.of(MaterialDesignN.NOTE), visible)

            else -> JCheckBoxMenuItem(columnName, visible)
        }

    fun load() {
        try {
            val config = ApplicationConfiguration.getConfiguration()
            val fileSettings = config.withLock(LockMode.READ) {
                parseColumnSettingsJson(getString(configPrefix + COLUMN_SETTINGS))
            }

            for (fileSetting in fileSettings) {
                val existing = lastSettings.firstOrNull { it.id == fileSetting.id }
                if (existing != null) {
                    existing.position = fileSetting.position
                    existing.width = fileSetting.width
                    existing.visible = fileSetting.visible
                } else {
                    lastSettings += ColumnSetting(
                        fileSetting.id,
                        fileSetting.position,
                        fileSetting.width,
                        fileSetting.visible,
                    )
                }
            }

            val validIds = allColumns.map { it.identifier.toString() }
            lastSettings.removeIf { it.id !in validIds }
        } catch (ex: Exception) {
            log.error("Failed to load column settings.", ex)
        }

        val columnModel = table.columnModel
        while (columnModel.columnCount > 0) {
            columnModel.removeColumn(columnModel.getColumn(0))
        }
        lastSettings.asSequence()
            .filter { it.visible }
            .sortedBy { it.position }
            .forEach { setting ->
                allColumns.firstOrNull { it.identifier.toString() == setting.id }?.let { column ->
                    columnModel.addColumn(column)
                    column.preferredWidth = setting.width
                }
            }
    }

    fun save() {
        val columnModel = table.columnModel
        for (lastSetting in lastSettings) {
            val column = allColumns.firstOrNull { it.identifier.toString() == lastSetting.id } ?: continue
            val visible = isInModel(column)
            lastSetting.visible = visible
            if (visible) {
                lastSetting.position = columnModel.getColumnIndex(lastSetting.id)
                lastSetting.width = column.width
            }
        }

        val config = ApplicationConfiguration.getConfiguration()
        try {
            config.withLock(LockMode.WRITE) {
                val output = toColumnSettingsJson(lastSettings)
                setProperty(configPrefix + COLUMN_SETTINGS, output)
            }
        } catch (ex: Exception) {
            log.error("Failed to save column settings.", ex)
        }
    }

    fun installContextMenu() {
        load()
        val popup = JPopupMenu()

        for (column in allColumns) {
            val columnName = column.identifier.toString()
            val columnSetting = lastSettings.firstOrNull { it.id == columnName }
            val visible = columnSetting?.visible ?: true
            val item = createMenuItem(columnName, visible)
            item.addActionListener {
                val columnModel: TableColumnModel = table.columnModel
                columnSetting?.visible = item.isSelected
                if (item.isSelected) {
                    if (!isInModel(column)) {
                        columnModel.addColumn(column)
                        val lastIndex = columnModel.columnCount - 1
                        val target = columnSetting?.position ?: lastIndex
                        columnModel.moveColumn(lastIndex, target.coerceIn(0, lastIndex))
                    }
                } else if (isInModel(column)) {
                    val index = columnModel.getColumnIndex(columnName)
                    columnSetting?.position = index
                    columnModel.removeColumn(column)
                }
                save()
            }
            popup.add(item)
        }
        popup.addSeparator()
        val resetSortingItem = JMenuItem("Sortierschlüssel zurücksetzen")
        resetSortingItem.addActionListener { comparatorChooser?.clearComparator() }
        popup.add(resetSortingItem)

        table.tableHeader.componentPopupMenu = popup
    }

    protected fun isInModel(column: TableColumn): Boolean {
        val columnModel = table.columnModel
        for (i in 0 until columnModel.columnCount) {
            if (columnModel.getColumn(i) == column) {
                return true
            }
        }
        return false
    }

    private fun parseColumnSettingsJson(json: String?): List<ColumnSetting> {
        val result = mutableListOf<ColumnSetting>()
        if (json == null) {
            return result
        }

        val trimmed = json.trim()
        if (!trimmed.startsWith("[") || !trimmed.endsWith("]")) {
            return result
        }

        var i = 1
        while (i < trimmed.length - 1) {
            val current = trimmed[i]
            if (current.isWhitespace() || current == ',') {
                i++
                continue
            }
            if (current != '{') {
                i++
                continue
            }

            val objectStart = i
            var depth = 0
            var inString = false
            var escaping = false
            var objectEnd = -1
            while (i < trimmed.length - 1) {
                val character = trimmed[i]
                if (escaping) {
                    escaping = false
                    i++
                    continue
                }
                if (character == '\\' && inString) {
                    escaping = true
                    i++
                    continue
                }
                if (character == '"') {
                    inString = !inString
                    i++
                    continue
                }
                if (inString) {
                    i++
                    continue
                }

                when (character) {
                    '{' -> depth++
                    '}' -> {
                        depth--
                        if (depth == 0) {
                            objectEnd = i
                            break
                        }
                    }
                }
                i++
            }

            if (objectEnd < 0) {
                break
            }

            val objectJson = trimmed.substring(objectStart, objectEnd + 1)
            parseColumnSettingObject(objectJson)?.let(result::add)
            i = objectEnd + 1
        }
        return result
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
        return try {
            matcher.group(1).toInt()
        } catch (_: NumberFormatException) {
            null
        }
    }

    private fun extractBoolean(pattern: Pattern, json: String): Boolean? {
        val matcher = pattern.matcher(json)
        if (!matcher.find()) {
            return null
        }
        return matcher.group(1).toBoolean()
    }

    class ColumnSetting(
        var id: String = "",
        var position: Int = 0,
        var width: Int = 0,
        var visible: Boolean = false,
    )

    private companion object {
        val log: Logger = LogManager.getLogger()
        const val COLUMN_SETTINGS = ".colummn-settings"
        val POSITION_PATTERN: Pattern = Pattern.compile("\"position\"\\s*:\\s*(-?\\d+)")
        val WIDTH_PATTERN: Pattern = Pattern.compile("\"width\"\\s*:\\s*(-?\\d+)")
        val VISIBLE_PATTERN: Pattern = Pattern.compile("\"visible\"\\s*:\\s*(true|false)")
    }
}
