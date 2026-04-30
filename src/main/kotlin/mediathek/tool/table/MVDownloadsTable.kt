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

import mediathek.config.Daten
import mediathek.config.MVConfig
import mediathek.daten.DatenDownload
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

private val logger = LogManager.getLogger()

class MVDownloadsTable : PersistentColumnConfigurationTable(
    DatenDownload.MAX_ELEM,
    DatenDownload.getColumnVisibilityStore(),
    Optional.of(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN),
    Optional.of(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_KLEIN),
    MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS,
) {
    init {
        setupDragAndDrop()
        model = TModelDownload()
    }

    override fun getToolTipText(event: MouseEvent): String? {
        val point = event.point
        val viewColumn = columnAtPoint(point)
        val modelColumnIndex = convertColumnIndexToModel(viewColumn)

        if (modelColumnIndex != DatenDownload.DOWNLOAD_TITEL) {
            return super.getToolTipText(event)
        }

        val viewRow = rowAtPoint(point)
        val component = prepareRenderer(getCellRenderer(viewRow, viewColumn), viewRow, viewColumn)
        val bounds = getCellRect(viewRow, viewColumn, false)

        return try {
            if (component.preferredSize.width > bounds.width) {
                val modelRowIndex = convertRowIndexToModel(viewRow)
                val download = model.getValueAt(modelRowIndex, DatenDownload.DOWNLOAD_REF) as DatenDownload
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

        super.resetTabelle()
    }

    private fun resetDownloadsTab(column: Int) {
        reihe[column] = column
        breite[column] = when (column) {
            DatenDownload.DOWNLOAD_NR,
            DatenDownload.DOWNLOAD_FILM_NR,
                -> 75

            DatenDownload.DOWNLOAD_BUTTON_START,
            DatenDownload.DOWNLOAD_BUTTON_DEL,
            DatenDownload.DOWNLOAD_PROGRAMM_RESTART,
            DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER,
            DatenDownload.DOWNLOAD_UNTERBROCHEN,
            DatenDownload.DOWNLOAD_SPOTLIGHT,
            DatenDownload.DOWNLOAD_SUBTITLE,
            DatenDownload.DOWNLOAD_INFODATEI,
            DatenDownload.DOWNLOAD_HD,
            DatenDownload.DOWNLOAD_UT,
                -> 50

            DatenDownload.DOWNLOAD_TITEL -> 250

            DatenDownload.DOWNLOAD_ABO,
            DatenDownload.DOWNLOAD_THEMA,
                -> 150

            DatenDownload.DOWNLOAD_DATUM,
            DatenDownload.DOWNLOAD_ZEIT,
            DatenDownload.DOWNLOAD_GROESSE,
            DatenDownload.DOWNLOAD_BANDBREITE,
            DatenDownload.DOWNLOAD_SENDER,
            DatenDownload.DOWNLOAD_PROGRESS,
            DatenDownload.DOWNLOAD_RESTZEIT,
            DatenDownload.DOWNLOAD_DAUER,
            DatenDownload.DOWNLOAD_GEO,
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
            val download = tableModel.getValueAt(convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF) as DatenDownload
            downloads.remove(download)
            downloads.add(download)
        }
    }

    override fun spaltenAusschalten() {
        for (column in 0 until maxSpalten) {
            when (column) {
                DatenDownload.DOWNLOAD_FILM_URL,
                DatenDownload.DOWNLOAD_URL_RTMP,
                DatenDownload.DOWNLOAD_URL_SUBTITLE,
                DatenDownload.DOWNLOAD_PROGRAMM,
                DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF,
                DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY,
                DatenDownload.DOWNLOAD_PROGRAMM_RESTART,
                DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER,
                DatenDownload.DOWNLOAD_ZIEL_DATEINAME,
                DatenDownload.DOWNLOAD_ZIEL_PFAD,
                DatenDownload.DOWNLOAD_ART,
                DatenDownload.DOWNLOAD_QUELLE,
                DatenDownload.DOWNLOAD_ZURUECKGESTELLT,
                DatenDownload.DOWNLOAD_HISTORY_URL,
                DatenDownload.DOWNLOAD_REF,
                DatenDownload.DOWNLOAD_SPOTLIGHT,
                DatenDownload.DOWNLOAD_INFODATEI,
                DatenDownload.DOWNLOAD_SUBTITLE,
                DatenDownload.DOWNLOAD_UNTERBROCHEN,
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

                val download = tableModel.getValueAt(convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF) as DatenDownload
                downloadsToMove.add(download)
                daten.listeDownloads.remove(download)
            }

            daten.listeDownloads.addAll(insertionIndex, downloadsToMove)
            rowSorter?.sortKeys = null
            rowSorter = null
            autoCreateRowSorter = true
            restoreSelectedTableRows()

            MessageBus.messageBus.publishAsync(DownloadQueueRankChangedEvent())
        }

        override fun exportDone(source: JComponent, data: Transferable, action: Int) {
            if (action == MOVE) {
                table.cursor = Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR)
            }
        }
    }

}
