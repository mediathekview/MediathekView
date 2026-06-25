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

package mediathek.mainwindow

import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.swing.SwingDispatcher
import java.util.function.Supplier

class FilmlistProgressPresenter(
    private val uiDispatcher: SwingDispatcher,
    private val progressFactory: Supplier<StatusBarProgressHandle>,
) : ListenerFilmeLaden(), AutoCloseable {
    private val progressLock = Any()
    private var handle: StatusBarProgressHandle? = null
    private var pendingProgress: ProgressSnapshot? = null
    private var progressDispatchScheduled = false

    override fun start(event: ListenerFilmeLadenEvent) {
        uiDispatcher.dispatch {
            closeCurrent()
            clearPendingProgress()
            handle = progressFactory.get()
        }
    }

    override fun progress(event: ListenerFilmeLadenEvent) {
        val shouldScheduleDispatch = synchronized(progressLock) {
            pendingProgress = ProgressSnapshot(event.text, event.max, event.progress)
            if (progressDispatchScheduled) {
                false
            } else {
                progressDispatchScheduled = true
                true
            }
        }

        if (shouldScheduleDispatch) {
            uiDispatcher.dispatch(::applyPendingProgress)
        }
    }

    override fun fertig(event: ListenerFilmeLadenEvent) {
        close()
    }

    override fun close() {
        uiDispatcher.dispatch(::closeCurrent)
    }

    private fun applyPendingProgress() {
        val progress = synchronized(progressLock) {
            progressDispatchScheduled = false
            pendingProgress.also { pendingProgress = null }
        } ?: return
        val current = handle ?: return
        val progressBar = current.progressBar()
        if (progress.max == 0 || progress.progress == progress.max) {
            progressBar.isIndeterminate = true
        } else {
            progressBar.isIndeterminate = false
            progressBar.minimum = 0
            progressBar.maximum = progress.max
            progressBar.value = progress.progress
        }
        current.label().text = progress.text
    }

    private fun closeCurrent() {
        clearPendingProgress()
        handle?.close()
        handle = null
    }

    private fun clearPendingProgress() {
        synchronized(progressLock) {
            pendingProgress = null
            progressDispatchScheduled = false
        }
    }

    private data class ProgressSnapshot(
        val text: String,
        val max: Int,
        val progress: Int,
    )
}
