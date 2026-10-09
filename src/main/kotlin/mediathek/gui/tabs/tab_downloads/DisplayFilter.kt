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

data class DisplayFilter(
    val onlyAbos: Boolean,
    val onlyDownloads: Boolean
) {
    fun onlyAbos(): Boolean = onlyAbos

    fun onlyDownloads(): Boolean = onlyDownloads

    fun selectedItem(): String = when {
        onlyDownloads -> DOWNLOADS_ONLY
        onlyAbos -> ABOS_ONLY
        else -> ALL
    }

    companion object {
        const val ALL = "alle"
        const val DOWNLOADS_ONLY = "nur Downloads"
        const val ABOS_ONLY = "nur Abos"

        fun all(): DisplayFilter = DisplayFilter(onlyAbos = false, onlyDownloads = false)

        fun from(selectedItem: Any?): DisplayFilter = when (selectedItem?.toString() ?: ALL) {
            DOWNLOADS_ONLY -> DisplayFilter(onlyAbos = false, onlyDownloads = true)
            ABOS_ONLY -> DisplayFilter(onlyAbos = true, onlyDownloads = false)
            else -> all()
        }
    }
}
