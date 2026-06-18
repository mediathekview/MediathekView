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

import mediathek.gui.bookmark.BookmarkDialog
import javax.swing.JFrame

interface MainWindowHandle {
    fun ownerFrame(): JFrame
}

interface FilmBookmarkHost : MainWindowHandle {
    val bookmarkDialog: BookmarkDialog?

    fun showManageBookmarkWindow()

    fun resetFilterDialogPosition()

    fun repaintFilmTab()
}

interface DownloadControlHost {
    fun stopAllWaitingDownloads()
}

interface FilmListLoadHost : MainWindowQuitHost {
    fun showStatusBarProgress(): StatusBarProgressHandle

    fun setFilmIndexingActionsEnabled(enabled: Boolean)
}

interface MainWindowQuitHost : MainWindowHandle {
    fun quitApplication(): Boolean
}

interface LookAndFeelHost {
    fun setupAlternatingRowColors()
}

interface SettingsDialogHost : MainWindowHandle, LookAndFeelHost {
    fun repaintMainWindow()

    fun refreshSystemTray()

    fun supportsAutomaticMenuTabSwitching(): Boolean
}

interface TrayHost : MainWindowQuitHost {
    fun showMainWindow()

    fun toggleMainWindowVisibility()

    fun refreshSystemTray()
}
