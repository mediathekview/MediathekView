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

package mediathek.gui.tabs.actions

import mediathek.controller.history.SeenHistoryController
import mediathek.daten.DatenFilm
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import java.util.function.Supplier
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.KeyStroke

class MarkFilmAsSeenAction(
    private val selectedFilms: Supplier<List<DatenFilm>>,
) : AbstractAction("Filme als gesehen markieren") {
    init {
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_DOWN_MASK))
    }

    override fun actionPerformed(event: ActionEvent?) {
        SeenHistoryController().use { controller ->
            controller.markSeen(selectedFilms.get())
        }
    }
}

class MarkFilmAsUnseenAction(
    private val selectedFilms: Supplier<List<DatenFilm>>,
) : AbstractAction("Filme als ungesehen markieren") {
    init {
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK))
    }

    override fun actionPerformed(event: ActionEvent?) {
        SeenHistoryController().use { controller ->
            controller.markUnseen(selectedFilms.get())
        }
    }
}

class MarkSingleFilmAsSeenAction(
    private val selectedFilm: Supplier<DatenFilm?>,
) : AbstractAction("Film als gesehen markieren") {
    override fun actionPerformed(event: ActionEvent?) {
        val film = selectedFilm.get() ?: return
        SeenHistoryController().use { controller ->
            controller.markSeen(film)
        }
    }
}

class MarkSingleFilmAsUnseenAction(
    private val selectedFilm: Supplier<DatenFilm?>,
) : AbstractAction("Film als ungesehen markieren") {
    override fun actionPerformed(event: ActionEvent?) {
        val film = selectedFilm.get() ?: return
        SeenHistoryController().use { controller ->
            controller.markUnseen(film)
        }
    }
}

fun hasBeenSeenInHistory(film: DatenFilm): Boolean =
    SeenHistoryController().use { controller ->
        controller.hasBeenSeen(film)
    }
