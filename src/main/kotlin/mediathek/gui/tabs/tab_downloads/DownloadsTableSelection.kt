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

package mediathek.gui.tabs.tab_downloads

import mediathek.controller.DownloadColumns
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.tool.NoSelectionErrorDialog
import mediathek.tool.table.MVDownloadsTable
import org.apache.logging.log4j.LogManager
import java.awt.Component
import java.util.*

class DownloadsTableSelection(
    private val table: MVDownloadsTable,
    private val parent: Component
) {
    private val logger = LogManager.getLogger(DownloadsTableSelection::class.java)

    fun selectedDownloadsForLookup(): List<DatenDownload> {
        val selectedDownloads = mutableListOf<DatenDownload>()
        val viewRowCount = table.rowCount
        val tableModel = table.model
        val modelRowCount = tableModel.rowCount

        for (row in table.selectedRows) {
            if (row < 0 || row >= viewRowCount) {
                continue
            }
            try {
                val modelRow = table.convertRowIndexToModel(row)
                if (modelRow < 0 || modelRow >= modelRowCount) {
                    continue
                }
                downloadAtModelRow(modelRow)?.let(selectedDownloads::add)
            } catch (ex: RuntimeException) {
                logger.debug("Could not resolve selected download for row {}", row, ex)
            }
        }

        return selectedDownloads
    }

    fun selectedDownloadsOrShowError(): ArrayList<DatenDownload> {
        val rows = table.selectedRows
        if (rows.isEmpty()) {
            NoSelectionErrorDialog.show(parent)
            return ArrayList()
        }

        return rows.mapTo(ArrayList()) { row ->
            downloadAtViewRow(row)
        }
    }

    fun selectedDownloadOrShowError(): DatenDownload? {
        val row = table.selectedRow
        if (row == -1) {
            NoSelectionErrorDialog.show(parent)
            return null
        }
        return downloadAtViewRow(row)
    }

    fun currentlySelectedFilm(): Optional<DatenFilm> =
        try {
            val row = table.selectedRow
            if (row == -1) {
                Optional.empty()
            } else {
                Optional.ofNullable(downloadAtViewRow(row).film)
            }
        } catch (_: Exception) {
            Optional.empty()
        }

    fun selectedFilmsOrShowError(): List<DatenFilm> {
        val rows = table.selectedRows
        if (rows.isEmpty()) {
            NoSelectionErrorDialog.show(parent)
            return emptyList()
        }

        val films = ArrayList<DatenFilm>()
        for (row in rows) {
            downloadAtViewRow(row).film?.let(films::add)
        }
        return films
    }

    private fun downloadAtViewRow(row: Int): DatenDownload =
        downloadAtModelRow(table.convertRowIndexToModel(row)) as DatenDownload

    private fun downloadAtModelRow(row: Int): DatenDownload? =
        table.model.getValueAt(row, DownloadColumns.REF) as? DatenDownload
}
