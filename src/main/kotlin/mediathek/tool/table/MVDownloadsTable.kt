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

import mediathek.audiothek.ui.table.TriStateTableRowSorter
import mediathek.controller.DownloadColumn
import mediathek.controller.starter.DownloadServices
import mediathek.daten.DatenDownload
import mediathek.tool.models.TModelDownload
import org.apache.logging.log4j.LogManager
import java.awt.Cursor
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable
import java.awt.dnd.DragSource
import java.awt.event.MouseEvent
import javax.activation.DataHandler
import javax.swing.DropMode
import javax.swing.JComponent
import javax.swing.JTable
import javax.swing.JTable.DropLocation as TableDropLocation
import javax.swing.TransferHandler
import javax.swing.table.TableModel

private val logger = LogManager.getLogger()

class MVDownloadsTable(
    private val downloads: DownloadServices,
) : PersistentColumnConfigurationTable(
    DownloadColumn.COUNT,
    DownloadColumn.visibilityStore(),
    TableConfigurationStores.DOWNLOAD,
) {
    private var sorter: DownloadsRowSorter? = null

    init {
        autoCreateRowSorter = false
        addPropertyChangeListener("model") { event ->
            val newModel = event.newValue as? TableModel ?: return@addPropertyChangeListener
            val currentSorter = sorter
            if (currentSorter == null) {
                val createdSorter = DownloadsRowSorter(newModel)
                sorter = createdSorter
                rowSorter = createdSorter
            } else {
                currentSorter.model = newModel
            }
        }
        setupDragAndDrop()
        model = TModelDownload()
    }

    override fun getToolTipText(event: MouseEvent): String? {
        val point = event.point
        val viewColumn = columnAtPoint(point)
        if (viewColumn < 0) {
            return super.getToolTipText(event)
        }
        val modelColumn = DownloadColumn.fromIndex(convertColumnIndexToModel(viewColumn))

        if (modelColumn != DownloadColumn.TITLE) {
            return super.getToolTipText(event)
        }

        val viewRow = rowAtPoint(point)
        val component = prepareRenderer(getCellRenderer(viewRow, viewColumn), viewRow, viewColumn)
        val bounds = getCellRect(viewRow, viewColumn, false)

        return try {
            if (component.preferredSize.width > bounds.width) {
                val modelRowIndex = convertRowIndexToModel(viewRow)
                val download = model.getValueAt(modelRowIndex, DownloadColumn.REF.index) as DatenDownload
                download.film?.title.orEmpty()
            } else {
                null
            }
        } catch (_: RuntimeException) {
            null
        }
    }

    private fun setupDragAndDrop() {
        dragEnabled = true
        dropMode = DropMode.INSERT_ROWS
        transferHandler = TableRowTransferHandlerDownload(this)
    }

    override fun resetTabelle() {
        for (column in 0 until maxSpalten) {
            resetDownloadsTab(column)
        }

        listeSortKeys = null

        rowSorter?.sortKeys = null
        spaltenAusschalten()
        setSpaltenEinAus(breite)
        setSpalten()
        calculateRowHeight()
    }

    private fun resetDownloadsTab(column: Int) {
        reihe[column] = column
        breite[column] = when (DownloadColumn.fromIndex(column)) {
            DownloadColumn.NUMBER,
            DownloadColumn.FILM_NUMBER,
                -> 75

            DownloadColumn.BUTTON_START,
            DownloadColumn.BUTTON_DELETE,
            DownloadColumn.PROGRAM_RESTART,
            DownloadColumn.DOWNLOAD_MANAGER,
            DownloadColumn.INTERRUPTED,
            DownloadColumn.SPOTLIGHT,
            DownloadColumn.SUBTITLE,
            DownloadColumn.INFO_FILE,
            DownloadColumn.HIGH_QUALITY,
            DownloadColumn.SUBTITLE_AVAILABLE,
                -> 50

            DownloadColumn.TITLE -> 250

            DownloadColumn.ABO,
            DownloadColumn.TOPIC,
                -> 150

            DownloadColumn.DATE,
            DownloadColumn.TIME,
            DownloadColumn.SIZE,
            DownloadColumn.BANDWIDTH,
            DownloadColumn.SENDER,
            DownloadColumn.PROGRESS,
            DownloadColumn.REMAINING_TIME,
            DownloadColumn.DURATION,
            DownloadColumn.GEO,
                -> 100

            else -> 200
        }
    }

    /**
     * Don't know exactly why this is actually called or needed, but it sorts the list of downloads
     * to be the same as the view order of this table.
     */
    @Synchronized
    fun sortDownloadListByTableRows() {
        val tableModel = model
        val downloadsInTableOrder = ArrayList<DatenDownload>()

        for (row in 0 until rowCount) {
            val download = tableModel.getValueAt(convertRowIndexToModel(row), DownloadColumn.REF.index) as DatenDownload
            downloadsInTableOrder.add(download)
        }
        downloads.reorderQueueToMatch(downloadsInTableOrder)
    }

    override fun spaltenAusschalten() {
        for (column in 0 until maxSpalten) {
            when (DownloadColumn.fromIndex(column)) {
                DownloadColumn.FILM_URL,
                DownloadColumn.RTMP_URL,
                DownloadColumn.SUBTITLE_URL,
                DownloadColumn.PROGRAM,
                DownloadColumn.PROGRAM_INVOCATION,
                DownloadColumn.PROGRAM_INVOCATION_ARRAY,
                DownloadColumn.PROGRAM_RESTART,
                DownloadColumn.DOWNLOAD_MANAGER,
                DownloadColumn.TARGET_FILE_NAME,
                DownloadColumn.TARGET_PATH,
                DownloadColumn.TYPE,
                DownloadColumn.SOURCE,
                DownloadColumn.DEFERRED,
                DownloadColumn.HISTORY_URL,
                DownloadColumn.REF,
                DownloadColumn.SPOTLIGHT,
                DownloadColumn.INFO_FILE,
                DownloadColumn.SUBTITLE,
                DownloadColumn.INTERRUPTED,
                    -> breite[column] = 0

                else -> Unit
            }
        }
    }

    private inner class TableRowTransferHandlerDownload(
        private val table: JTable,
    ) : TransferHandler() {
        private val localObjectFlavor = DataFlavor(Int::class.javaObjectType, "Integer Row Index")
        private var transferredRows: IntArray? = null

        override fun createTransferable(component: JComponent): Transferable {
            check(component == table)
            transferredRows = table.selectedRows
            return DataHandler(table.selectedRow, localObjectFlavor.mimeType)
        }

        override fun canImport(info: TransferSupport): Boolean {
            return try {
                val canImport = info.component == table && info.isDrop && info.isDataFlavorSupported(localObjectFlavor)
                table.cursor = if (canImport) DragSource.DefaultMoveDrop else DragSource.DefaultMoveNoDrop
                canImport
            } catch (exception: Exception) {
                logger.error("canImport", exception)
                true
            }
        }

        override fun getSourceActions(component: JComponent): Int = COPY_OR_MOVE

        override fun importData(info: TransferSupport): Boolean {
            return try {
                val target = info.component as JTable
                val dropLocation = info.dropLocation as TableDropLocation
                var index = dropLocation.row
                val max = table.model.rowCount
                if (index !in 0..max) {
                    index = max
                }
                target.cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)

                val rows = transferredRows ?: return false
                reorder(index, rows)
                transferredRows = null
                true
            } catch (exception: Exception) {
                logger.error("importData", exception)
                false
            }
        }

        private fun reorder(targetIndex: Int, rowsFrom: IntArray) {
            saveSelectedTableRows()

            val tableModel = model as TModelDownload

            sortDownloadListByTableRows()

            var insertionIndex = targetIndex
            val downloadsToMove = ArrayList<DatenDownload>()
            for (row in rowsFrom) {
                if (insertionIndex > row) {
                    --insertionIndex
                }

                val download = tableModel.getValueAt(convertRowIndexToModel(row), DownloadColumn.REF.index) as DatenDownload
                downloadsToMove.add(download)
            }

            downloads.moveDownloadsTo(insertionIndex, downloadsToMove)
            rowSorter?.sortKeys = null
            restoreSelectedTableRows()
        }

        override fun exportDone(source: JComponent, data: Transferable, action: Int) {
            if (action == MOVE) {
                table.cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
            }
        }
    }

    private class DownloadsRowSorter(model: TableModel) : TriStateTableRowSorter<TableModel>(model) {
        override fun setModel(model: TableModel) {
            super.setModel(model)
            configureSortableColumns()
        }

        override fun setSortKeys(sortKeys: MutableList<out SortKey>?) {
            super.setSortKeys(sortKeys?.filter { isSortable(it.column) })
        }

        private fun configureSortableColumns() {
            setSortable(DownloadColumn.BUTTON_START.index, false)
            setSortable(DownloadColumn.BUTTON_DELETE.index, false)
        }
    }

}
