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

import mediathek.controller.MVBandwidthCountingInputStream
import java.time.LocalDateTime

class DownloadRunState {
    @JvmField
    var status: StartStatus = StartStatus.INITIALIZED

    @JvmField
    var startcounter: Int = 0

    /**
     * Prozess des Download
     */
    @JvmField
    var process: Process? = null

    /**
     * Prozent fertiggestellt: -1=nix, 999=99,9%
     */
    @JvmField
    var percent: Int = -1

    /**
     * Downloadbandbreite: bytes per second
     */
    @JvmField
    var bandbreite: Long = -1

    @Volatile
    @JvmField
    var stoppen: Boolean = false

    @JvmField
    var countRestarted: Int = 0

    @JvmField
    var startTime: LocalDateTime? = null

    @JvmField
    var restSekunden: Long = -1

    @JvmField
    var mVBandwidthCountingInputStream: MVBandwidthCountingInputStream? = null

    val isFinished: Boolean
        get() = status == StartStatus.FINISHED

    val isError: Boolean
        get() = status == StartStatus.ERROR

    fun markRunning() {
        status = StartStatus.RUNNING
    }

    fun markFinished() {
        status = StartStatus.FINISHED
    }

    fun markError() {
        status = StartStatus.ERROR
    }

    fun markCompletedProgress() {
        restSekunden = -1
        percent = PROGRESS_FERTIG
    }

    fun requestStop() {
        stoppen = true
    }

    fun incrementStartCounter() {
        startcounter++
    }

    fun updateProgress(progress: Int) {
        percent = progress
    }

    fun updateBandwidth(bytesPerSecond: Long) {
        bandbreite = bytesPerSecond
    }

    fun updateRemainingSeconds(seconds: Long) {
        restSekunden = seconds
    }

    companion object {
        const val PROGRESS_NICHT_GESTARTET: Int = -1
        const val PROGRESS_WARTEN: Int = 0
        const val PROGRESS_GESTARTET: Int = 1
        const val PROGRESS_FERTIG: Int = 1000
    }
}
