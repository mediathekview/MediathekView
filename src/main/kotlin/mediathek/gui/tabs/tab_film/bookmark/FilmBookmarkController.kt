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

package mediathek.gui.tabs.tab_film.bookmark

import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.gui.bookmark.BookmarkDialog
import mediathek.mainwindow.MediathekGui

class FilmBookmarkController(private val host: Host) {
    private var bookmarkDialog: BookmarkDialog? = null

    interface Host {
        fun mediathekGui(): MediathekGui
        fun repaintOwner()
    }

    fun updateBookmarkListAndRefresh(filmList: List<DatenFilm>) {
        val bookmarkList = Daten.getInstance().listeBookmarkList
        bookmarkList.checkAndBookmarkMovies(filmList)
        bookmarkList.saveToFile()
        host.repaintOwner()
    }

    fun showManageBookmarkWindow() {
        val dialog = bookmarkDialog ?: BookmarkDialog(host.mediathekGui()).also { bookmarkDialog = it }
        dialog.isVisible = true
    }

    fun getBookmarkDialog(): BookmarkDialog? = bookmarkDialog
}
