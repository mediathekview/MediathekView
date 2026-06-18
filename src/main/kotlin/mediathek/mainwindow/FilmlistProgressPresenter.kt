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
    private var handle: StatusBarProgressHandle? = null

    override fun start(event: ListenerFilmeLadenEvent) {
        uiDispatcher.dispatch {
            closeCurrent()
            handle = progressFactory.get()
        }
    }

    override fun progress(event: ListenerFilmeLadenEvent) {
        uiDispatcher.dispatch {
            val current = handle ?: return@dispatch
            val progressBar = current.progressBar()
            if (event.max == 0 || event.progress == event.max) {
                progressBar.isIndeterminate = true
            } else {
                progressBar.isIndeterminate = false
                progressBar.minimum = 0
                progressBar.maximum = event.max
                progressBar.value = event.progress
            }
            current.label().text = event.text
        }
    }

    override fun fertig(event: ListenerFilmeLadenEvent) {
        close()
    }

    override fun close() {
        uiDispatcher.dispatch(::closeCurrent)
    }

    private fun closeCurrent() {
        handle?.close()
        handle = null
    }
}
