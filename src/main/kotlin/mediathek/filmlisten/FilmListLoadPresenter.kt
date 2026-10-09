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

package mediathek.filmlisten

import org.apache.logging.log4j.LogManager
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.JProgressBar

internal interface FilmListProgressHandle : AutoCloseable {
    fun label(): JLabel

    fun progressBar(): JProgressBar

    override fun close()
}

interface FilmListIndexingHost {
    fun ownerFrame(): JFrame

    fun quitApplication(): Boolean

    fun setFilmIndexingActionsEnabled(enabled: Boolean)
}

internal data class FilmListStatusBarWidgets(
    val handle: FilmListProgressHandle,
    val host: FilmListIndexingHost?,
) {
    val label: JLabel
        get() = handle.label()

    val progressBar: JProgressBar
        get() = handle.progressBar()
}

internal interface FilmListLoadPresenter : FilmListImportFeedback {
    fun showLoadFailedDialog()

    suspend fun <T> withStatusBarWidgets(block: suspend (FilmListStatusBarWidgets) -> T): T
}

internal class PresenterFilmListImportFeedback(
    private val currentPresenter: () -> FilmListImportFeedback,
) : FilmListImportFeedback {
    override fun showNoUpdateAvailable(showDialogs: Boolean) {
        currentPresenter().showNoUpdateAvailable(showDialogs)
    }

    override fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean) {
        currentPresenter().showExceptionMessage(message, ex, showDialogs)
    }
}

internal object NoOpFilmListLoadPresenter : FilmListLoadPresenter {
    override fun showNoUpdateAvailable(showDialogs: Boolean) {
        logger.info(NO_UPDATE_AVAILABLE)
    }

    override fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean) {
    }

    override fun showLoadFailedDialog() {
    }

    override suspend fun <T> withStatusBarWidgets(block: suspend (FilmListStatusBarWidgets) -> T): T =
        block(FilmListStatusBarWidgets(NoStatusBarProgressHandle(), host = null))

    private class NoStatusBarProgressHandle : FilmListProgressHandle {
        private val label = JLabel()
        private val progressBar = JProgressBar()

        override fun label(): JLabel = label

        override fun progressBar(): JProgressBar = progressBar

        override fun close() {
        }
    }

    private val logger = LogManager.getLogger(NoOpFilmListLoadPresenter::class.java)
    private const val NO_UPDATE_AVAILABLE = "Es ist keine aktuellere Filmliste verfügbar."
}
