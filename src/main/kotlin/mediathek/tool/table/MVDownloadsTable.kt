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
import mediathek.config.Daten
import mediathek.config.MVConfig
import mediathek.daten.DatenDownload
import mediathek.daten.DownloadColumns
import mediathek.gui.messages.DownloadQueueRankChangedEvent
import mediathek.tool.MessageBus
import mediathek.tool.models.TModelDownload
import org.apache.logging.log4j.LogManager
import java.awt.Cursor
import java.awt.datatransfer.DataFlavor
import java.awt.datatransfer.Transferable
import java.awt.dnd.DragSource
import java.awt.event.MouseEvent
import java.util.*
import javax.activation.DataHandler
import javax.swing.DropMode
import javax.swing.JComponent
import javax.swing.JTable
import javax.swing.TransferHandler
import javax.swing.table.TableModel

private val logger = LogManager.getLogger()

class MVDownloadsTable : PersistentColumnConfigurationTable(
    DownloadColumns.COUNT,
    DownloadColumns.visibilityStore(),
    Optional.of(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN),
    Optional.of(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_KLEIN),
    MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS,
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
        val modelColumnIndex = convertColumnIndexToModel(viewColumn)

        if (modelColumnIndex != DownloadColumns.TITLE) {
            return super.getToolTipText(event)
        }

        val viewRow = rowAtPoint(point)
        val component = prepareRenderer(getCellRenderer(viewRow, viewColumn), viewRow, viewColumn)
        val bounds = getCellRect(viewRow, viewColumn, false)

        return try {
            if (component.preferredSize.width > bounds.width) {
                val modelRowIndex = convertRowIndexToModel(viewRow)
                val download = model.getValueAt(modelRowIndex, DownloadColumns.REF) as DatenDownload
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
        breite[column] = when (column) {
            DownloadColumns.NR,
            DownloadColumns.FILM_NR,
                -> 75

            DownloadColumns.BUTTON_START,
            DownloadColumns.BUTTON_DELETE,
            DownloadColumns.PROGRAM_RESTART,
            DownloadColumns.DOWNLOAD_MANAGER,
            DownloadColumns.INTERRUPTED,
            DownloadColumns.SPOTLIGHT,
            DownloadColumns.SUBTITLE,
            DownloadColumns.INFO_FILE,
            DownloadColumns.HIGH_QUALITY,
            DownloadColumns.SUBTITLE_AVAILABLE,
                -> 50

            DownloadColumns.TITLE -> 250

            DownloadColumns.ABO,
            DownloadColumns.TOPIC,
                -> 150

            DownloadColumns.DATE,
            DownloadColumns.TIME,
            DownloadColumns.SIZE,
            DownloadColumns.BANDWIDTH,
            DownloadColumns.SENDER,
            DownloadColumns.PROGRESS,
            DownloadColumns.REMAINING_TIME,
            DownloadColumns.DURATION,
            DownloadColumns.GEO,
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
        val downloads = Daten.getInstance().listeDownloads

        for (row in 0 until rowCount) {
            val download = tableModel.getValueAt(convertRowIndexToModel(row), DownloadColumns.REF) as DatenDownload
            downloads.remove(download)
            downloads.add(download)
        }
    }

    override fun spaltenAusschalten() {
        for (column in 0 until maxSpalten) {
            when (column) {
                DownloadColumns.FILM_URL,
                DownloadColumns.RTMP_URL,
                DownloadColumns.SUBTITLE_URL,
                DownloadColumns.PROGRAM,
                DownloadColumns.PROGRAM_INVOCATION,
                DownloadColumns.PROGRAM_INVOCATION_ARRAY,
                DownloadColumns.PROGRAM_RESTART,
                DownloadColumns.DOWNLOAD_MANAGER,
                DownloadColumns.TARGET_FILE_NAME,
                DownloadColumns.TARGET_PATH,
                DownloadColumns.TYPE,
                DownloadColumns.SOURCE,
                DownloadColumns.DEFERRED,
                DownloadColumns.HISTORY_URL,
                DownloadColumns.REF,
                DownloadColumns.SPOTLIGHT,
                DownloadColumns.INFO_FILE,
                DownloadColumns.SUBTITLE,
                DownloadColumns.INTERRUPTED,
                    -> breite[column] = 0
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
                val dropLocation = info.dropLocation as JTable.DropLocation
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

            val daten = Daten.getInstance()
            val tableModel = model as TModelDownload

            sortDownloadListByTableRows()

            var insertionIndex = targetIndex
            val downloadsToMove = ArrayList<DatenDownload>()
            for (row in rowsFrom) {
                if (insertionIndex > row) {
                    --insertionIndex
                }

                val download = tableModel.getValueAt(convertRowIndexToModel(row), DownloadColumns.REF) as DatenDownload
                downloadsToMove.add(download)
                daten.listeDownloads.remove(download)
            }

            daten.listeDownloads.addAll(insertionIndex, downloadsToMove)
            rowSorter?.sortKeys = null
            restoreSelectedTableRows()

            MessageBus.messageBus.publishAsync(DownloadQueueRankChangedEvent())
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
            setSortable(DownloadColumns.BUTTON_START, false)
            setSortable(DownloadColumns.BUTTON_DELETE, false)
        }
    }

}
