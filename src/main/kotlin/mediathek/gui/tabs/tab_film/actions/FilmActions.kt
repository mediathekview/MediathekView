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

package mediathek.gui.tabs.tab_film.actions

import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.FilmResolution
import mediathek.swing.IconUtils
import mediathek.tool.GuiFunktionen
import org.apache.commons.lang3.SystemUtils
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.materialdesign2.MaterialDesignF
import java.awt.KeyEventDispatcher
import java.awt.event.ActionEvent
import java.awt.event.KeyEvent
import java.util.*
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.KeyStroke

interface FilmActionHost {
    fun saveFilm(pSet: DatenPset?)
    fun selectedFilms(): List<DatenFilm>
    fun updateBookmarkListAndRefresh(films: List<DatenFilm>)
    fun currentlySelectedFilm(): Optional<DatenFilm>
    fun toggleFilterDialogVisibility()
}

class ToggleFilterDialogVisibilityAction(private val host: FilmActionHost) : AbstractAction() {
    init {
        putValue(NAME, "Filterdialog anzeigen")
        putValue(SHORT_DESCRIPTION, "Filter anzeigen")
        putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.FILTER))
        putValue(ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F12, 0))
    }

    override fun actionPerformed(e: ActionEvent?) {
        host.toggleFilterDialogVisibility()
    }
}

internal class ActionAcceleratorDispatcher(
    private val action: Action,
    private val isShortcutScopeActive: () -> Boolean = { true },
) : KeyEventDispatcher {
    override fun dispatchKeyEvent(event: KeyEvent): Boolean {
        val accelerator = action.getValue(Action.ACCELERATOR_KEY) as? KeyStroke ?: return false
        if (!isShortcutScopeActive() ||
            !action.isEnabled ||
            event.id != KeyEvent.KEY_PRESSED ||
            KeyStroke.getKeyStrokeForEvent(event) != accelerator
        ) {
            return false
        }

        action.actionPerformed(
            ActionEvent(
                event.source,
                ActionEvent.ACTION_PERFORMED,
                action.getValue(Action.ACTION_COMMAND_KEY) as? String,
            )
        )
        return true
    }
}

class SaveFilmAction(private val host: FilmActionHost) : AbstractAction() {
    init {
        putValue(SHORT_DESCRIPTION, "Film downloaden")
        putValue(NAME, "Film downloaden")
        putValue(SMALL_ICON, IconUtils.toolbarIcon(FontAwesomeSolid.DOWNLOAD))
        val keyStroke = if (SystemUtils.IS_OS_MAC_OSX) {
            KeyStroke.getKeyStroke(KeyEvent.VK_F7, GuiFunktionen.getPlatformControlKey())
        } else {
            KeyStroke.getKeyStroke(KeyEvent.VK_D, GuiFunktionen.getPlatformControlKey())
        }
        putValue(ACCELERATOR_KEY, keyStroke)
    }

    override fun actionPerformed(e: ActionEvent?) {
        host.saveFilm(null)
    }
}

class BookmarkAddFilmAction(private val host: FilmActionHost) : AbstractAction() {
    init {
        val keyStroke = if (SystemUtils.IS_OS_MAC_OSX) {
            KeyStroke.getKeyStroke(KeyEvent.VK_F8, GuiFunktionen.getPlatformControlKey())
        } else {
            KeyStroke.getKeyStroke(KeyEvent.VK_B, GuiFunktionen.getPlatformControlKey())
        }
        putValue(ACCELERATOR_KEY, keyStroke)
        putValue(SHORT_DESCRIPTION, "Ausgewählte Filme in der Merkliste speichern")
        putValue(NAME, "Ausgewählte Filme merken")
        putValue(SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignF.FILE_DOCUMENT_PLUS))
    }

    override fun actionPerformed(e: ActionEvent?) {
        val selectedFilms = host.selectedFilms()
        if (selectedFilms.isNotEmpty()) {
            val filmsToBookmark = selectedFilms.parallelStream()
                .filter { !it.isBookmarked }
                .filter { !it.isLivestream }
                .toList()
            if (filmsToBookmark.isNotEmpty()) {
                host.updateBookmarkListAndRefresh(filmsToBookmark)
            }
        }
    }
}

class BookmarkRemoveFilmAction(private val host: FilmActionHost) : AbstractAction() {
    init {
        putValue(SHORT_DESCRIPTION, "Ausgewählte Filme aus der Merkliste löschen")
        putValue(NAME, "Ausgewählte Filme aus der Merkliste löschen")
        putValue(SMALL_ICON, IconUtils.toolbarIcon(MaterialDesignF.FILE_DOCUMENT_MINUS))
    }

    override fun actionPerformed(e: ActionEvent?) {
        val selectedFilms = host.selectedFilms()
        if (selectedFilms.isNotEmpty()) {
            val filmsToUnbookmark = selectedFilms.parallelStream()
                .filter(DatenFilm::isBookmarked)
                .toList()
            if (filmsToUnbookmark.isNotEmpty()) {
                host.updateBookmarkListAndRefresh(filmsToUnbookmark)
            }
        }
    }
}

class CopyUrlToClipboardAction(
    private val host: FilmActionHost,
    private val resolution: FilmResolution.Enum
) : AbstractAction() {
    override fun actionPerformed(e: ActionEvent?) {
        host.currentlySelectedFilm().ifPresent { film ->
            GuiFunktionen.copyToClipboard(film.getUrlFuerAufloesung(resolution))
        }
    }
}
