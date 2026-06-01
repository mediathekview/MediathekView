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

import mediathek.controller.starter.DownloadRunState
import mediathek.controller.starter.StartStatus
import mediathek.tool.BandwidthFormatter
import kotlin.math.roundToLong

internal object DownloadRuntimeText {
    fun remainingTime(start: DownloadRunState?): String =
        if (start?.status == StartStatus.RUNNING && start.restSekunden > 0) {
            formatTimeRemaining(start.restSekunden)
        } else {
            ""
        }

    fun bandwidth(start: DownloadRunState?): String =
        if (start?.status?.isAtLeast(StartStatus.RUNNING) == true) {
            BandwidthFormatter.format(start.bandbreite)
        } else {
            ""
        }

    fun formatTimeRemaining(seconds: Long): String {
        if (seconds > 300) {
            return "${(seconds / 60.0).roundToLong()} Min."
        }

        val limits = intArrayOf(230, 170, 110, 60, 30, 20, 10)
        val labels = arrayOf("5 Min.", "4 Min.", "3 Min.", "2 Min.", "1 Min.", "30 s", "20 s")

        for (index in limits.indices) {
            if (seconds > limits[index]) {
                return labels[index]
            }
        }

        return "10 s"
    }
}
