/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.mac

import mediathek.tool.threads.IndicatorThread
import java.awt.Taskbar
import java.util.concurrent.TimeUnit

/**
 * This thread will update the percentage drawn on the dock icon on OS X.
 */
internal class OsxIndicatorThread : IndicatorThread() {
    private var oldPercentage = 0
    override fun run() {
        val taskbar = Taskbar.getTaskbar()
        taskbar.setProgressValue(0)
        try {
            while (!isInterrupted) {
                val percentage = calculateOverallPercentage().toInt()

                //update in 1pct steps...
                //if icon was already drawn, don´ do it again
                if (oldPercentage != percentage) {
                    taskbar.setProgressValue(percentage)
                }
                oldPercentage = percentage
                TimeUnit.MILLISECONDS.sleep(500)
            }
        } catch (_: Exception) {
        } finally {
            //reset the application dock icon
            taskbar.setProgressValue(-1)
            taskbar.setIconBadge(null)
            oldPercentage = 0
        }
    }

    init {
        name = "OsxIndicatorThread"
    }
}