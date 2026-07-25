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

import mediathek.controller.starter.DownloadServices
import mediathek.daten.DatenFilm
import mediathek.daten.ProgramSetRepository
import mediathek.gui.bookmark.BookmarkDialog
import mediathek.gui.bookmark.BookmarkServices
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.JFrame

class FilmBookmarkController(private val host: Host) {
    private var bookmarkDialog: BookmarkDialog? = null

    interface Host {
        fun ownerFrame(): JFrame
        fun bookmarks(): BookmarkServices
        fun programSets(): ProgramSetRepository
        fun downloads(): DownloadServices
        fun addDownloads(films: List<DatenFilm>)
        fun editFilmDescription(film: DatenFilm)
        fun repaintOwner()
    }

    fun updateBookmarkListAndRefresh(filmList: List<DatenFilm>) {
        val bookmarkList = host.bookmarks().list
        bookmarkList.checkAndBookmarkMovies(filmList)
        bookmarkList.saveToFile()
        host.repaintOwner()
    }

    fun showManageBookmarkWindow() {
        val dialog = bookmarkDialog?.takeIf { !it.isDisposed && it.isDisplayable } ?: BookmarkDialog(
            host.ownerFrame(),
            host.bookmarks(),
            host.programSets(),
            host.downloads(),
            host::addDownloads,
            host::editFilmDescription,
            host::repaintOwner,
        ).also { createdDialog ->
            bookmarkDialog = createdDialog
            createdDialog.addWindowListener(
                object : WindowAdapter() {
                    override fun windowClosed(event: WindowEvent) {
                        if (bookmarkDialog === createdDialog) bookmarkDialog = null
                    }
                },
            )
        }
        dialog.isVisible = true
    }

    fun getBookmarkDialog(): BookmarkDialog? = bookmarkDialog?.takeIf { !it.isDisposed && it.isDisplayable }
}
