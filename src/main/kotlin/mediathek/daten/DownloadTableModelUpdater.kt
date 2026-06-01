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

package mediathek.daten

import mediathek.controller.starter.DownloadProgressText
import mediathek.controller.starter.DownloadRunState
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
            val download = item[DownloadColumns.REF] as DatenDownload
            if (download.runtime.runState?.isRunning == true) {
                model.setValueAt(download.textRestzeit, row, DownloadColumns.REMAINING_TIME)
                model.setValueAt(download.textBandbreite, row, DownloadColumns.BANDWIDTH)
                model.setValueAt(progressText(download), row, DownloadColumns.PROGRESS)
                model.setValueAt(download.runtime.filmSize, row, DownloadColumns.SIZE)
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

    private fun createModelRow(download: DatenDownload): Array<Any?> {
        val row = arrayOfNulls<Any>(DownloadColumns.COUNT)
        for (index in 0 until DownloadColumns.COUNT) {
            row[index] = modelValue(download, index)
        }
        return row
    }

    private fun modelValue(download: DatenDownload, index: Int): Any? =
        when {
            index == DownloadColumns.NR -> download.nr
            index == DownloadColumns.FILM_NR -> download.film?.filmNr ?: 0
            index in hiddenBooleanColumns -> ""
            index == DownloadColumns.DATE -> download.datumFilm
            index == DownloadColumns.REMAINING_TIME -> download.textRestzeit
            index == DownloadColumns.BANDWIDTH -> download.textBandbreite
            index == DownloadColumns.PROGRESS -> progressText(download)
            index == DownloadColumns.SIZE -> download.runtime.filmSize
            index == DownloadColumns.GEO -> download.geo
            index == DownloadColumns.REF -> download
            index != DownloadColumns.URL && !DownloadColumns.isVisible(index) -> ""
            else -> visibleModelValue(download, index)
        }

    private fun visibleModelValue(download: DatenDownload, index: Int): Any? =
        when (index) {
            DownloadColumns.ABO -> download.aboName
            DownloadColumns.SENDER -> download.sender
            DownloadColumns.TOPIC -> download.topic
            DownloadColumns.TITLE -> download.title
            DownloadColumns.BUTTON_START,
            DownloadColumns.BUTTON_DELETE,
            -> ""

            DownloadColumns.TIME -> download.time
            DownloadColumns.DURATION -> download.duration
            DownloadColumns.HIGH_QUALITY -> download.film?.isHighQuality == true
            DownloadColumns.SUBTITLE_AVAILABLE -> download.film?.hasSubtitle() == true
            DownloadColumns.FILM_URL -> download.filmUrl
            DownloadColumns.HISTORY_URL -> download.historyUrl
            DownloadColumns.URL -> download.downloadUrl
            DownloadColumns.RTMP_URL -> download.rtmpUrl
            DownloadColumns.SUBTITLE_URL -> download.subtitleUrl
            DownloadColumns.PROGRAM_SET -> download.programSetName
            DownloadColumns.PROGRAM -> download.programName
            DownloadColumns.PROGRAM_INVOCATION -> download.programInvocation
            DownloadColumns.PROGRAM_INVOCATION_ARRAY -> download.programInvocationArray
            DownloadColumns.PROGRAM_RESTART -> download.isRestart
            DownloadColumns.TARGET_FILE_NAME -> download.targetFileName
            DownloadColumns.TARGET_PATH -> download.targetPath
            DownloadColumns.TARGET_PATH_FILE_NAME -> download.targetPathFileName
            DownloadColumns.TYPE -> download.art.legacyId.toString()
            DownloadColumns.SOURCE -> download.quelle.legacyId.toString()
            DownloadColumns.DEFERRED -> download.isDeferred
            DownloadColumns.INFO_FILE -> download.isInfoFile
            DownloadColumns.SPOTLIGHT -> download.isSpotlight
            DownloadColumns.SUBTITLE -> download.isSubtitle
            DownloadColumns.DOWNLOAD_MANAGER -> download.isDownloadManager
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
        DownloadColumns.PROGRAM_RESTART,
        DownloadColumns.INTERRUPTED,
        DownloadColumns.SPOTLIGHT,
        DownloadColumns.INFO_FILE,
        DownloadColumns.SUBTITLE,
        DownloadColumns.DEFERRED,
        DownloadColumns.DOWNLOAD_MANAGER,
    )
}
