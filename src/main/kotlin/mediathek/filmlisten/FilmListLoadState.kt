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

import java.util.concurrent.atomic.AtomicReference

internal class FilmListLoadState {
    private val phase = AtomicReference(FilmListLoadPhase.IDLE)

    val isRunning: Boolean
        get() = phase.get() != FilmListLoadPhase.IDLE

    fun tryBegin(): Boolean = phase.compareAndSet(FilmListLoadPhase.IDLE, FilmListLoadPhase.IMPORTING)

    fun startPostLoad() {
        check(phase.compareAndSet(FilmListLoadPhase.IMPORTING, FilmListLoadPhase.POST_PROCESSING)) {
            "Post-load work can only start after a filmlist import has started."
        }
    }

    fun finish() {
        phase.set(FilmListLoadPhase.IDLE)
    }

    private enum class FilmListLoadPhase {
        IDLE,
        IMPORTING,
        POST_PROCESSING,
    }
}
