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

object DownloadProgressText {
    @JvmStatic
    fun getTextProgress(isDownloadManager: Boolean, state: DownloadRunState?): String {
        if (state == null) {
            return ""
        }

        return when (state.percent) {
            DownloadRunState.PROGRESS_NICHT_GESTARTET -> ""
            DownloadRunState.PROGRESS_WARTEN -> if (isDownloadManager) "extern" else "warten"
            DownloadRunState.PROGRESS_GESTARTET -> if (isDownloadManager) "extern:gesendet" else "gestartet"
            DownloadRunState.PROGRESS_FERTIG -> if (state.isError) {
                if (isDownloadManager) "extern:fehler" else "fehlerhaft"
            } else {
                if (isDownloadManager) "extern:fertig" else "fertig"
            }

            else -> when {
                isDownloadManager -> "extern"
                state.percent in 2 until DownloadRunState.PROGRESS_FERTIG -> "${state.percent / 10.0}%"
                else -> ""
            }
        }
    }
}
