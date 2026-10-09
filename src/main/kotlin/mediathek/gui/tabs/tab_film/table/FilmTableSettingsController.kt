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

import kotlinx.serialization.Serializable
import kotlinx.serialization.json.Json
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.FontSizeChangedEvent
import mediathek.tool.MessageBus
import mediathek.tool.models.FilmColumn
import net.engio.mbassy.listener.Handler
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.*
import javax.swing.table.TableColumn

internal class FilmTableSettingsController(
    private val table: JTable,
    private val sorting: FilmTableSorting,
    private val appearance: FilmTableAppearance,
) {
    private val configuration = ApplicationConfiguration.getInstance()
    private val allColumns = (0 until table.columnModel.columnCount)
        .map { table.columnModel.getColumn(it).apply { identifier = FilmColumn.fromIndex(modelIndex).name } }
    private val visibleWidths = DEFAULT_WIDTHS.copyOf()
    private val headerListener = HeaderPopupListener()
    private var disposed = false

    init {
        table.autoResizeMode = JTable.AUTO_RESIZE_OFF
        table.tableHeader.addMouseListener(headerListener)
        MessageBus.messageBus.subscribe(this)
        restoreState()
        calculateRowHeight()
    }

    fun saveState() {
        if (disposed) {
            return
        }
        captureVisibleWidths()
        val columns = allColumns.map { column ->
            val viewIndex = viewIndex(column.modelIndex)
            FilmColumnState(
                id = FilmColumn.fromIndex(column.modelIndex).name,
                width = visibleWidths[column.modelIndex],
                position = viewIndex.takeIf { it >= 0 } ?: column.modelIndex,
                visible = column.maxWidth > 0,
            )
        }
        configuration.setTableColumnSettings(CONFIG_PREFIX, STATE_JSON.encodeToString(FilmTableState(columns)))
        configuration.filmTableShowSenderIcons = appearance.showSenderIcons
        configuration.filmTableUseSmallSenderIcons = appearance.useSmallSenderIcons
        configuration.filmTableLineBreak = appearance.lineBreak
        sorting.save()
    }

    fun dispose() {
        if (disposed) {
            return
        }
        saveState()
        disposed = true
        table.tableHeader.removeMouseListener(headerListener)
        MessageBus.messageBus.unsubscribe(this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleFontSizeChanged(event: FontSizeChangedEvent) {
        SwingUtilities.invokeLater(::calculateRowHeight)
    }

    private fun restoreState() {
        val storedState = configuration.getTableColumnSettings(CONFIG_PREFIX)
        val state = storedState.takeIf(String::isNotBlank)
            ?.let { raw -> runCatching { STATE_JSON.decodeFromString<FilmTableState>(raw) }.getOrNull() }

        if (state != null) {
            applyState(state)
            return
        }

        val legacyState = parseLegacyState(configuration.filmTableColumnConfiguration)
        if (legacyState != null) {
            applyState(legacyState.tableState)
            legacyState.sort?.let { sort -> sorting.restoreLegacySort(sort.column, sort.descending) }
            saveState()
        } else {
            applyDefaults()
        }
    }

    private fun applyState(state: FilmTableState) {
        val statesById = state.columns.associateBy(FilmColumnState::id)
        for (column in allColumns) {
            val saved = statesById[FilmColumn.fromIndex(column.modelIndex).name]
            visibleWidths[column.modelIndex] = saved?.width?.takeIf { it > 0 } ?: DEFAULT_WIDTHS[column.modelIndex]
        }

        val orderedColumns = allColumns.sortedWith(
            compareBy<TableColumn> { statesById[FilmColumn.fromIndex(it.modelIndex).name]?.position ?: it.modelIndex }
                .thenBy(TableColumn::getModelIndex),
        )
        orderedColumns.forEachIndexed { targetIndex, column ->
            val currentIndex = viewIndex(column.modelIndex)
            if (currentIndex >= 0 && currentIndex != targetIndex) {
                table.columnModel.moveColumn(currentIndex, targetIndex)
            }
        }
        for (column in allColumns) {
            val visible = statesById[FilmColumn.fromIndex(column.modelIndex).name]?.visible ?: true
            applyColumnVisibility(column.modelIndex, visible)
        }
        refreshTable()
    }

    private fun applyDefaults(clearSorting: Boolean = false) {
        allColumns.sortedBy(TableColumn::getModelIndex).forEachIndexed { targetIndex, column ->
            val currentIndex = viewIndex(column.modelIndex)
            if (currentIndex != targetIndex) {
                table.columnModel.moveColumn(currentIndex, targetIndex)
            }
        }
        for (column in allColumns) {
            visibleWidths[column.modelIndex] = DEFAULT_WIDTHS[column.modelIndex]
            applyColumnVisibility(column.modelIndex, true)
        }
        refreshTable()
        if (clearSorting) {
            sorting.clear()
        }
    }

    private fun applyColumnVisibility(modelIndex: Int, visible: Boolean) {
        val column = allColumns.first { it.modelIndex == modelIndex }
        if (visible) {
            val width = visibleWidths[modelIndex].coerceAtLeast(10)
            column.minWidth = 10
            column.maxWidth = 3000
            column.preferredWidth = width
            column.width = width
        } else {
            if (column.width > 0) {
                visibleWidths[modelIndex] = column.width
            }
            column.minWidth = 0
            column.maxWidth = 0
            column.preferredWidth = 0
            column.width = 0
        }
    }

    private fun setColumnsVisible(columns: Collection<FilmColumn>, visible: Boolean) {
        columns.forEach { column -> applyColumnVisibility(column.index, visible) }
        refreshTable()
    }

    private fun refreshTable() {
        table.revalidate()
        table.repaint()
    }

    private fun captureVisibleWidths() {
        for (column in allColumns) {
            if (column.maxWidth > 0 && column.width > 0) {
                visibleWidths[column.modelIndex] = column.width
            }
        }
    }

    private fun calculateRowHeight() {
        val lineHeight = table.getFontMetrics(table.font).height
        val textHeight = if (appearance.lineBreak) lineHeight * 3 else lineHeight
        val iconHeight = when {
            !appearance.showSenderIcons -> Konstanten.TABLE_DEFAULT_ROW_HEIGHT
            appearance.useSmallSenderIcons -> Konstanten.TABLE_DEFAULT_ROW_HEIGHT
            else -> Konstanten.TABLE_DEFAULT_LARGE_ICON_ROW_HEIGHT
        }
        table.rowHeight = maxOf(textHeight, iconHeight)
    }

    private fun viewIndex(modelIndex: Int): Int =
        (0 until table.columnModel.columnCount).firstOrNull {
            table.columnModel.getColumn(it).modelIndex == modelIndex
        } ?: -1

    private fun parseLegacyState(raw: String): LegacyState? {
        val parts = raw.split('|')
        if (parts.size != 4) {
            return null
        }
        val widths = parts[0].split(',').map { it.toIntOrNull() ?: return null }
        val order = parts[1].split(',').map { it.toIntOrNull() ?: return null }
        if (widths.size != LEGACY_COLUMN_COUNT || order.size != LEGACY_COLUMN_COUNT) {
            return null
        }
        val visibleOrder = order.take(FilmColumn.entries.size)
        if (visibleOrder.toSet() != FilmColumn.entries.indices.toSet()) {
            return null
        }
        val positions = visibleOrder.withIndex().associate { (position, column) -> column to position }
        val columns = FilmColumn.entries.map { column ->
            FilmColumnState(
                id = column.name,
                width = widths[column.index].takeIf { it > 0 } ?: DEFAULT_WIDTHS[column.index],
                position = positions.getValue(column.index),
                visible = widths[column.index] > 0,
            )
        }
        val sortColumn = parts[2].toIntOrNull()
        val sort = sortColumn
            ?.takeIf { it in FilmColumn.entries.indices }
            ?.let { LegacySort(it, parts[3] == "DESCENDING") }
        return LegacyState(FilmTableState(columns), sort)
    }

    private inner class HeaderPopupListener : MouseAdapter() {
        override fun mousePressed(event: MouseEvent) = showPopupIfNecessary(event)
        override fun mouseReleased(event: MouseEvent) = showPopupIfNecessary(event)

        private fun showPopupIfNecessary(event: MouseEvent) {
            if (!event.isPopupTrigger) {
                return
            }
            createPopup().show(event.component, event.x, event.y)
        }

        private fun createPopup(): JPopupMenu = JPopupMenu().apply {
            FilmColumn.entries.filterNot { it in BUTTON_COLUMNS }.forEach { column ->
                add(JCheckBoxMenuItem(column.title(), allColumns[column.index].maxWidth > 0).apply {
                    addActionListener { setColumnsVisible(listOf(column), isSelected) }
                })
            }
            addSeparator()
            add(JCheckBoxMenuItem("Buttons anzeigen", allColumns[FilmColumn.PLAY.index].maxWidth > 0).apply {
                addActionListener { setColumnsVisible(BUTTON_COLUMNS, isSelected) }
            })
            addSeparator()
            add(JCheckBoxMenuItem("Sendericons anzeigen", appearance.showSenderIcons).apply {
                addActionListener {
                    appearance.showSenderIcons = isSelected
                    calculateRowHeight()
                    table.repaint()
                }
            })
            add(JCheckBoxMenuItem("Kleine Sendericons anzeigen", appearance.useSmallSenderIcons).apply {
                isEnabled = appearance.showSenderIcons
                addActionListener {
                    appearance.useSmallSenderIcons = isSelected
                    calculateRowHeight()
                    table.repaint()
                }
            })
            add(JCheckBoxMenuItem("Zeilen umbrechen", appearance.lineBreak).apply {
                addActionListener {
                    appearance.lineBreak = isSelected
                    calculateRowHeight()
                    table.repaint()
                }
            })
            addSeparator()
            add(JMenuItem("Spalten zurücksetzen").apply {
                addActionListener {
                    applyDefaults(clearSorting = true)
                    calculateRowHeight()
                }
            })
        }
    }

    private data class LegacyState(val tableState: FilmTableState, val sort: LegacySort?)
    private data class LegacySort(val column: Int, val descending: Boolean)

    private companion object {
        private const val CONFIG_PREFIX = "film"
        private const val LEGACY_COLUMN_COUNT = 17
        private val STATE_JSON = Json { ignoreUnknownKeys = true; encodeDefaults = true }
        private val BUTTON_COLUMNS = setOf(FilmColumn.PLAY, FilmColumn.SAVE, FilmColumn.BOOKMARK)
        private val DEFAULT_WIDTHS = intArrayOf(75, 100, 200, 300, 20, 20, 20, 100, 100, 100, 100, 50, 50, 100, 500)
    }
}

@Serializable
private data class FilmTableState(val columns: List<FilmColumnState> = emptyList())

@Serializable
private data class FilmColumnState(
    val id: String,
    val width: Int,
    val position: Int,
    val visible: Boolean,
)
