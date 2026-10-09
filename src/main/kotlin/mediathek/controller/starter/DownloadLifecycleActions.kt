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

import mediathek.daten.DatenDownload
import mediathek.gui.messages.RestartDownloadEvent
import mediathek.tool.MessageBus

object DownloadLifecycleActions {
    fun reset(download: DatenDownload) {
        download.runtime.reset()
    }

    fun defer(download: DatenDownload) {
        if (download.runtime.runState?.status?.isAfter(StartStatus.INITIALIZED) == true) {
            return
        }
        download.isDeferred = true
        reset(download)
    }

    fun clearDeferred(download: DatenDownload) {
        download.isDeferred = false
    }

    fun markInterrupted(download: DatenDownload) {
        download.isInterruptedFlag = true
    }

    fun restartInterrupted(download: DatenDownload) {
        download.isInterruptedFlag = false
        MessageBus.messageBus.publishAsync(RestartDownloadEvent())
    }
}
