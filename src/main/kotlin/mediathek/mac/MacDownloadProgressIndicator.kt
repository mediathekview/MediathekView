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

package mediathek.mac

import mediathek.gui.progress.ThreadedDownloadProgressIndicator
import java.awt.Taskbar

class MacDownloadProgressIndicator : ThreadedDownloadProgressIndicator(::OsxIndicatorThread) {
    private val powerManager = OsxPowerManager()

    override fun onDownloadStarted(activeDownloadCount: Int) {
        powerManager.disablePowerManagement()
        setDownloadsBadge(activeDownloadCount)
    }

    override fun onDownloadFinished(activeDownloadCount: Int) {
        if (activeDownloadCount == 0) {
            powerManager.enablePowerManagement()
        }
        setDownloadsBadge(activeDownloadCount)
    }

    override fun close() {
        super.close()
        powerManager.close()
        setDownloadsBadge(0)
    }

    private fun setDownloadsBadge(numDownloads: Int) {
        if (Taskbar.isTaskbarSupported()) {
            val taskbar = Taskbar.getTaskbar()
            if (taskbar.isSupported(Taskbar.Feature.ICON_BADGE_NUMBER)) {
                taskbar.setIconBadge(if (numDownloads > 0) numDownloads.toString() else "")
            }
        }
    }
}
