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

data class ViewFilter(
    val onlyNotStarted: Boolean,
    val onlyStarted: Boolean,
    val onlyWaiting: Boolean,
    val onlyFinished: Boolean,
    val onlyRun: Boolean
) {
    fun onlyNotStarted(): Boolean = onlyNotStarted

    fun onlyStarted(): Boolean = onlyStarted

    fun onlyWaiting(): Boolean = onlyWaiting

    fun onlyFinished(): Boolean = onlyFinished

    fun onlyRun(): Boolean = onlyRun

    fun selectedItem(): String = when {
        onlyNotStarted -> NOT_STARTED
        onlyStarted -> STARTED
        onlyWaiting -> WAITING
        onlyFinished -> FINISHED_ONLY
        onlyRun -> RUN_ONLY
        else -> ALL
    }

    companion object {
        const val ALL = "alle"
        const val NOT_STARTED = "nicht gestartet"
        const val STARTED = "gestartet"
        const val WAITING = "nur wartende"
        const val RUN_ONLY = "nur laufende"
        const val FINISHED_ONLY = "nur abgeschlossene"

        fun all(): ViewFilter = ViewFilter(
            onlyNotStarted = false,
            onlyStarted = false,
            onlyWaiting = false,
            onlyFinished = false,
            onlyRun = false,
        )

        fun from(selectedItem: Any?): ViewFilter = when (selectedItem?.toString() ?: ALL) {
            NOT_STARTED -> all().copy(onlyNotStarted = true)
            STARTED -> all().copy(onlyStarted = true)
            WAITING -> all().copy(onlyWaiting = true)
            FINISHED_ONLY -> all().copy(onlyFinished = true)
            RUN_ONLY -> all().copy(onlyRun = true)
            else -> all()
        }
    }
}
