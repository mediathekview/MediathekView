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

package mediathek.controller

import mediathek.tool.table.ColumnVisibilityStore

object DownloadColumns {
    const val NR = 0
    const val FILM_NR = 1
    const val ABO = 2
    const val SENDER = 3
    const val TOPIC = 4
    const val TITLE = 5
    const val BUTTON_START = 6
    const val BUTTON_DELETE = 7
    const val PROGRESS = 8
    const val REMAINING_TIME = 9
    const val BANDWIDTH = 10
    const val SIZE = 11
    const val DATE = 12
    const val TIME = 13
    const val DURATION = 14
    const val HIGH_QUALITY = 15
    const val SUBTITLE_AVAILABLE = 16
    const val INTERRUPTED = 17
    const val GEO = 18
    const val FILM_URL = 19
    const val HISTORY_URL = 20
    const val URL = 21
    const val RTMP_URL = 22
    const val SUBTITLE_URL = 23
    const val PROGRAM_SET = 24
    const val PROGRAM = 25
    const val PROGRAM_INVOCATION = 26
    const val PROGRAM_INVOCATION_ARRAY = 27
    const val PROGRAM_RESTART = 28
    const val TARGET_FILE_NAME = 29
    const val TARGET_PATH = 30
    const val TARGET_PATH_FILE_NAME = 31
    const val TYPE = 32
    const val SOURCE = 33
    const val DEFERRED = 34
    const val INFO_FILE = 35
    const val SPOTLIGHT = 36
    const val SUBTITLE = 37
    const val DOWNLOAD_MANAGER = 38
    const val REF = 39
    const val COUNT = 40

    private val columnVisibilityStore = ColumnVisibilityStore.create(COUNT)

    fun isVisible(index: Int): Boolean =
        columnVisibilityStore.isVisible(index)

    fun visibilityStore(): ColumnVisibilityStore =
        columnVisibilityStore
}
