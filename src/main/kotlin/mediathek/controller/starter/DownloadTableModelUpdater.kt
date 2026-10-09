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

package mediathek.controller.starter

import mediathek.controller.DownloadColumn
import mediathek.daten.DatenDownload
import mediathek.tool.models.TModelDownload

internal object DownloadTableModelUpdater {
    fun reload(model: TModelDownload, downloads: Iterable<DatenDownload>, filter: DownloadListFilter) {
        model.rowCount = 0
        model.dataVector.clear()
        for (download in downloads) {
            if (download.matches(filter)) {
                model.addRow(createModelRow(download))
            }
        }
    }

    fun updateProgress(model: TModelDownload) {
        for ((row, item) in model.dataVector.withIndex()) {
            val download = item[DownloadColumn.REF.index] as DatenDownload
            if (download.runtime.runState?.isRunning == true) {
                model.setValueAt(download.textRestzeit, row, DownloadColumn.REMAINING_TIME.index)
                model.setValueAt(download.textBandbreite, row, DownloadColumn.BANDWIDTH.index)
                model.setValueAt(progressText(download), row, DownloadColumn.PROGRESS.index)
                model.setValueAt(download.runtime.filmSize, row, DownloadColumn.SIZE.index)
            }
        }
    }

    private fun DatenDownload.matches(filter: DownloadListFilter): Boolean {
        if (isDeferred) {
            return false
        }

        val isAbo = isFromAbo
        if (filter.onlyAbos && !isAbo) {
            return false
        }
        if (filter.onlyDownloads && isAbo) {
            return false
        }

        val notStarted = notStarted()
        if (filter.onlyNotStarted && !notStarted) {
            return false
        }
        if (filter.onlyStarted && notStarted) {
            return false
        }

        if (filter.onlyWaiting && !isWaiting) {
            return false
        }
        if (filter.onlyRun && !running()) {
            return false
        }
        if (filter.onlyFinished && !isFinished) {
            return false
        }
        return true
    }

    private fun createModelRow(download: DatenDownload): Array<Any> =
        Array(DownloadColumn.COUNT) { index -> modelValue(download, index) }

    private fun modelValue(download: DatenDownload, index: Int): Any =
        modelValue(download, DownloadColumn.fromIndex(index))

    private fun modelValue(download: DatenDownload, column: DownloadColumn): Any =
        when {
            column == DownloadColumn.NUMBER -> download.nr
            column == DownloadColumn.FILM_NUMBER -> download.film?.filmNr ?: 0
            column in hiddenBooleanColumns -> ""
            column == DownloadColumn.DATE -> download.datumFilm
            column == DownloadColumn.REMAINING_TIME -> download.textRestzeit
            column == DownloadColumn.BANDWIDTH -> download.textBandbreite
            column == DownloadColumn.PROGRESS -> progressText(download)
            column == DownloadColumn.SIZE -> download.runtime.filmSize
            column == DownloadColumn.GEO -> download.geo
            column == DownloadColumn.REF -> download
            column != DownloadColumn.URL && !DownloadColumn.isVisible(column.index) -> ""
            else -> visibleModelValue(download, column)
        }

    private fun visibleModelValue(download: DatenDownload, column: DownloadColumn): Any =
        when (column) {
            DownloadColumn.ABO -> download.aboName
            DownloadColumn.SENDER -> download.sender
            DownloadColumn.TOPIC -> download.topic
            DownloadColumn.TITLE -> download.title
            DownloadColumn.BUTTON_START,
            DownloadColumn.BUTTON_DELETE,
            -> ""

            DownloadColumn.TIME -> download.time
            DownloadColumn.DURATION -> download.duration
            DownloadColumn.HIGH_QUALITY -> download.film?.isHighQuality == true
            DownloadColumn.SUBTITLE_AVAILABLE -> download.film?.hasSubtitle() == true
            DownloadColumn.FILM_URL -> download.filmUrl
            DownloadColumn.HISTORY_URL -> download.historyUrl
            DownloadColumn.URL -> download.downloadUrl
            DownloadColumn.RTMP_URL -> download.rtmpUrl
            DownloadColumn.SUBTITLE_URL -> download.subtitleUrl
            DownloadColumn.PROGRAM_SET -> download.programSetName
            DownloadColumn.PROGRAM -> download.programName
            DownloadColumn.PROGRAM_INVOCATION -> download.programInvocation
            DownloadColumn.PROGRAM_INVOCATION_ARRAY -> download.programInvocationArray
            DownloadColumn.PROGRAM_RESTART -> download.isRestart
            DownloadColumn.TARGET_FILE_NAME -> download.targetFileName
            DownloadColumn.TARGET_PATH -> download.targetPath
            DownloadColumn.TARGET_PATH_FILE_NAME -> download.targetPathFileName
            DownloadColumn.TYPE -> download.art.legacyId.toString()
            DownloadColumn.SOURCE -> download.quelle.legacyId.toString()
            DownloadColumn.DEFERRED -> download.isDeferred
            DownloadColumn.INFO_FILE -> download.isInfoFile
            DownloadColumn.SPOTLIGHT -> download.isSpotlight
            DownloadColumn.SUBTITLE -> download.isSubtitle
            DownloadColumn.DOWNLOAD_MANAGER -> download.isDownloadManager
            else -> ""
        }

    private fun progressText(download: DatenDownload): String {
        val state = download.runtime.runState ?: return ""
        if (state.percent in 2 until DownloadRunState.PROGRESS_FERTIG) {
            return "${state.percent / 10.0}%".padStart(5, '0')
        }
        return DownloadProgressText.getTextProgress(download.isDownloadManager, state)
    }

    private val hiddenBooleanColumns = setOf(
        DownloadColumn.PROGRAM_RESTART,
        DownloadColumn.INTERRUPTED,
        DownloadColumn.SPOTLIGHT,
        DownloadColumn.INFO_FILE,
        DownloadColumn.SUBTITLE,
        DownloadColumn.DEFERRED,
        DownloadColumn.DOWNLOAD_MANAGER,
    )
}
