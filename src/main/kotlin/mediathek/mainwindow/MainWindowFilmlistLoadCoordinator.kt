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

import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmeLaden
import mediathek.gui.dialog.LoadFilmListDialog
import mediathek.tool.FilmListUpdateType
import javax.swing.JFrame

class MainWindowFilmlistLoadCoordinator(
    private val owner: JFrame,
    private val filmCatalog: FilmCatalog,
    private val filmListLoader: FilmeLaden,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val statusBarController: MainWindowStatusBarController,
) : AutoCloseable {
    private var startupFilmlistLoader: StartupFilmlistLoader? = null

    /**
     * Read a local filmlist or load a new one in auto mode.
     */
    fun loadStartupFilmlist() {
        statusBarController.installStartupProgress()
        startupFilmlistLoader = StartupFilmlistLoader(
            filmCatalog,
            filmListLoader,
            abos,
            blacklist,
            statusBarController.startupProgressLabel,
            statusBarController.startupProgressBar,
            ::finishStartupFilmlistLoad,
        )
        startupFilmlistLoader?.start()
    }

    fun performFilmListLoadOperation(manualMode: Boolean) {
        if (manualMode || FilmListUpdateType.MANUAL.isConfigured()) {
            LoadFilmListDialog(owner, filmCatalog, filmListLoader).isVisible = true
        } else {
            filmListLoader.loadFilmlist("", false)
        }
    }

    private fun finishStartupFilmlistLoad(remoteUpdateStarted: Boolean, failed: Boolean) {
        try {
            if (!remoteUpdateStarted) {
                filmListLoader.completeStartupFilmListLoad(failed)
            }
        } finally {
            statusBarController.uninstallStartupProgress()
        }
    }

    override fun close() {
        startupFilmlistLoader?.close()
        startupFilmlistLoader = null
    }
}
