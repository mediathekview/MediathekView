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

import mediathek.controller.history.FilmSeenHistoryController
import mediathek.daten.DatenDownload
import mediathek.gui.messages.FilmsDownloadStartedEvent
import mediathek.gui.messages.StartEvent
import mediathek.tool.MessageBus

object DownloadStartActions {
    fun start(download: DatenDownload) {
        startAll(listOf(download))
    }

    fun startAll(downloads: Iterable<DatenDownload>) {
        val downloadsToStart = downloads.toList()
        FilmSeenHistoryController().use { historyController ->
            for (download in downloadsToStart) {
                download.runtime.startRun()
                historyController.markSeen(download.film)
            }
        }

        MessageBus.messageBus.publishAsync(StartEvent())

        val films = downloadsToStart.mapNotNull(DatenDownload::film).distinctBy { film -> film.sha256 }
        if (films.isNotEmpty()) {
            // Dispatch synchronously so subscribers enqueue their durable work before shutdown can begin.
            MessageBus.messageBus.publish(FilmsDownloadStartedEvent(films))
        }
    }
}
