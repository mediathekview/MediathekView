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

package mediathek.filmlisten

import java.util.*

data class FilmListLoadProgress(
    val senderUrl: String,
    val text: String,
    val max: Int,
    val progress: Int,
    val failed: Boolean,
) {
    companion object {
        fun completed(failed: Boolean): FilmListLoadProgress =
            FilmListLoadProgress("", "", 100, 100, failed)

        fun started(senderUrl: String): FilmListLoadProgress =
            FilmListLoadProgress(senderUrl, "", PROGRESS_MAX, 0, failed = false)

        fun downloading(senderUrl: String, progress: Int): FilmListLoadProgress =
            FilmListLoadProgress(senderUrl, DOWNLOAD_TEXT, PROGRESS_MAX, progress, failed = false)

        fun finished(senderUrl: String, progress: Int): FilmListLoadProgress =
            FilmListLoadProgress(senderUrl, "", PROGRESS_MAX, progress, failed = false)

        private const val PROGRESS_MAX = 100
        private const val DOWNLOAD_TEXT = "Download"
    }
}

interface FilmListLoadListener : EventListener {
    fun loadStarted(progress: FilmListLoadProgress) {
    }

    fun loadProgress(progress: FilmListLoadProgress) {
    }

    fun loadFinished(progress: FilmListLoadProgress) {
    }

    fun firstLoadFinished(progress: FilmListLoadProgress) {
    }
}
