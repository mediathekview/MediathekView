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

import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.gui.messages.FontSizeChangedEvent
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import java.awt.Color
import javax.swing.JTable
import javax.swing.RowSorter
import javax.swing.SwingUtilities
import javax.swing.UIManager
import javax.swing.plaf.UIResource
import javax.swing.table.AbstractTableModel
import javax.swing.table.TableColumnModel

abstract class MVTable protected constructor(
    @JvmField protected val maxSpalten: Int,
    @JvmField protected val spaltenAnzeigen: ColumnVisibilityStore,
    protected val showIconsConfigKey: java.util.Optional<MVConfig.Configs>,
    protected val smallSenderIconConfigKey: java.util.Optional<MVConfig.Configs>,
) : JTable() {

    @JvmField
    protected val breite = IntArray(maxSpalten) { -1 }

    @JvmField
    protected val reihe = IntArray(maxSpalten) { -1 }

    private var useSmallSenderIconsState = false
    protected var listeSortKeys: List<out RowSorter.SortKey>? = null

    /**
     * Unmodified JTable used to calculate the row height. Reference only.
     */
    private val probe = JTable()
    private var selectedRowsSnapshot = IntArray(0)
    private var showSenderIcon = false
    private var lineBreak = true

    init {
        spaltenAnzeigen.fill(true)

        autoCreateRowSorter = true
        autoResizeMode = AUTO_RESIZE_OFF

        showIconsConfigKey.ifPresent { showSenderIcon = MVConfig.get(it).toBoolean() }
        smallSenderIconConfigKey.ifPresent { useSmallSenderIconsState = MVConfig.get(it).toBoolean() }

        calculateRowHeight()
        MessageBus.messageBus.subscribe(this)
    }

    fun getUseSmallSenderIcons(): Boolean = useSmallSenderIconsState

    fun setUseSmallSenderIcons(useSmallSenderIcons: Boolean) {
        useSmallSenderIconsState = useSmallSenderIcons
    }

    protected fun defaultRowBackground(row: Int): Color {
        if (row % 2 != 0) {
            UIManager.getColor("Table.alternateRowColor")?.let { return it }
        }

        val background = background
        if (background !is UIResource) {
            return background
        }

        return UIManager.getColor("Table.background") ?: background
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleFontSizeChanged(event: FontSizeChangedEvent) {
        SwingUtilities.invokeLater(::calculateRowHeight)
    }

    fun showSenderIcons(): Boolean = showSenderIcon

    fun setShowIcon(newVal: Boolean) {
        showSenderIcon = newVal
    }

    fun isLineBreak(): Boolean = lineBreak

    fun setLineBreak(lineBreak: Boolean) {
        this.lineBreak = lineBreak
    }

    /**
     * Return a fictious size of a multi-line text area.
     */
    private fun sizeArea(): Int {
        val lineHeight = getFontMetrics(font).height
        return if (lineBreak) {
            lineHeight * 3
        } else {
            lineHeight
        }
    }

    /**
     * Calculate the row height in a table based on icon display, etc.
     */
    open fun calculateRowHeight() {
        val minimumHeight = if (showSenderIcon) {
            if (useSmallSenderIconsState) {
                maxOf(Konstanten.TABLE_DEFAULT_ROW_HEIGHT, probe.rowHeight)
            } else {
                maxOf(Konstanten.TABLE_DEFAULT_LARGE_ICON_ROW_HEIGHT, probe.rowHeight)
            }
        } else {
            Konstanten.TABLE_DEFAULT_ROW_HEIGHT
        }

        rowHeight = maxOf(minimumHeight, sizeArea())
    }

    private fun isColumnVisible(index: Int): Boolean = spaltenAnzeigen.isVisible(index)

    protected fun setSpaltenEinAus(columnWidths: IntArray) {
        for (index in 0 until spaltenAnzeigen.length()) {
            spaltenAnzeigen.setVisible(index, columnWidths[index] > 0)
        }
    }

    fun fireTableDataChanged(setSpalten: Boolean) {
        if (setSpalten) {
            saveSelectedTableRows()
        }

        (model as AbstractTableModel).fireTableDataChanged()

        if (setSpalten) {
            restoreSelectedTableRows()
        }
    }

    fun scrollToSelection() {
        val rowCount = rowCount
        if (rowCount <= 0) {
            return
        }

        var selectedRow = selectedRow
        if (selectedRow == -1) {
            selectedRow = 0
            selectionModel.setSelectionInterval(0, 0)
        }
        if (selectedRow >= rowCount) {
            selectedRow = rowCount - 1
        }

        scrollToIndexDelegate(selectedRow)
    }

    protected fun scrollToIndexDelegate(index: Int) {
        scrollRectToVisible(getCellRect(index, 0, true))
    }

    protected open fun saveSelectedTableRows() {
        selectedRowsSnapshot = selectedRows
    }

    protected open fun restoreSelectedTableRows() {
        if (selectedRowsSnapshot.isEmpty()) {
            return
        }

        val visibleRow = if (selectedRowsSnapshot.size == 1) {
            val selectedRow = selectedRowsSnapshot[0]
            selectionModel.setSelectionInterval(selectedRow, selectedRow)
            selectedRow
        } else {
            selectionModel.valueIsAdjusting = true
            try {
                for (selectedRow in selectedRowsSnapshot) {
                    if (selectedRow < rowCount) {
                        addRowSelectionInterval(selectedRow, selectedRow)
                    }
                }
            } finally {
                selectionModel.valueIsAdjusting = false
            }
            selectedRowsSnapshot[0]
        }

        scrollToIndexDelegate(visibleRow)
        requestFocusInWindow()
    }

    protected fun changeTableModelColumnWidths() {
        val tableColumnModel: TableColumnModel = columnModel
        for (index in 0 until minOf(breite.size, columnCount)) {
            val column = tableColumnModel.getColumn(convertColumnIndexToView(index))
            if (breite[index] == 0) {
                column.minWidth = 0
                column.preferredWidth = 0
                column.maxWidth = 0
            } else {
                column.minWidth = 10
                column.maxWidth = 3000
                column.preferredWidth = breite[index]
            }
        }
    }

    protected fun changeInternalColumnWidths() {
        for (index in 0 until minOf(breite.size, columnCount)) {
            when {
                !isColumnVisible(index) -> breite[index] = 0
                breite[index] == 0 -> breite[index] = 100
            }
        }
    }

    fun spaltenEinAus() {
        getSpalten()
        changeInternalColumnWidths()
        changeTableModelColumnWidths()
        validate()
    }

    open fun getSpalten() {
        saveSelectedTableRows()

        val modelColumnCount = model.columnCount
        for (index in 0 until minOf(reihe.size, modelColumnCount)) {
            reihe[index] = convertColumnIndexToModel(index)
        }

        val tableColumnModel = columnModel
        for (index in 0 until minOf(breite.size, modelColumnCount)) {
            breite[index] = tableColumnModel.getColumn(convertColumnIndexToView(index)).width
        }

        listeSortKeys = rowSorter?.sortKeys
    }

    open fun setSpalten() {
        try {
            changeInternalColumnWidths()

            val tableColumnModel: TableColumnModel = columnModel
            changeTableModelColumnWidths()

            for (index in 0 until minOf(reihe.size, columnCount)) {
                tableColumnModel.moveColumn(convertColumnIndexToView(reihe[index]), index)
            }

            val savedSortKeys = listeSortKeys
            if (!savedSortKeys.isNullOrEmpty()) {
                rowSorter?.sortKeys = savedSortKeys
            }

            restoreSelectedTableRows()
            validate()
        } catch (exception: Exception) {
            logger.error("setSpalten", exception)
        }
    }

    /**
     * Perform common reset steps for all subclasses.
     */
    open fun resetTabelle() {
        listeSortKeys = null

        rowSorter?.sortKeys = null
        rowSorter = null
        autoCreateRowSorter = true
        spaltenAusschalten()
        setSpaltenEinAus(breite)
        setSpalten()
        calculateRowHeight()
    }

    protected abstract fun spaltenAusschalten()

    /**
     * Write table display preferences to config.
     */
    open fun writeTableConfigurationData() {
        showIconsConfigKey.ifPresent { MVConfig.add(it, showSenderIcon.toString()) }
        smallSenderIconConfigKey.ifPresent { MVConfig.add(it, useSmallSenderIconsState.toString()) }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }
}
