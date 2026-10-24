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

import mediathek.i18n.UiMessages
object DownloadProgressText {
    fun getTextProgress(isDownloadManager: Boolean, state: DownloadRunState?): String {
        if (state == null) {
            return ""
        }

        return when (state.percent) {
            DownloadRunState.PROGRESS_NICHT_GESTARTET -> ""
            DownloadRunState.PROGRESS_WARTEN -> if (isDownloadManager) UiMessages.text("downloads.progress.external") else UiMessages.text("downloads.progress.waiting")
            DownloadRunState.PROGRESS_GESTARTET -> if (isDownloadManager) UiMessages.text("downloads.progress.sent") else UiMessages.text("downloads.progress.started")
            DownloadRunState.PROGRESS_FERTIG -> if (state.isError) {
                if (isDownloadManager) UiMessages.text("downloads.progress.externalFailed") else UiMessages.text("downloads.progress.failed")
            } else {
                if (isDownloadManager) UiMessages.text("downloads.progress.externalFinished") else UiMessages.text("downloads.progress.finished")
            }

            else -> when {
                isDownloadManager -> UiMessages.text("downloads.progress.external")
                state.percent in 2 until DownloadRunState.PROGRESS_FERTIG -> "${state.percent / 10.0}%"
                else -> ""
            }
        }
    }
}
