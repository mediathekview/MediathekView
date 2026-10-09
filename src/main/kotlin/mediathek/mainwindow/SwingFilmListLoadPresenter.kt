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

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.config.CommandLineOptions
import mediathek.config.Konstanten
import mediathek.filmlisten.*
import mediathek.tool.SwingErrorDialog
import org.apache.logging.log4j.LogManager
import java.awt.GraphicsEnvironment
import javax.swing.JOptionPane
import javax.swing.SwingUtilities

internal class SwingFilmListLoadPresenter(
    @Volatile private var host: FilmListLoadHost? = null,
) : FilmListLoadPresenter {
    fun setHost(host: FilmListLoadHost?) {
        this.host = host
    }

    override fun showNoUpdateAvailable(showDialogs: Boolean) {
        val dialogHost = dialogHost()
        if (showDialogs && dialogHost != null) {
            runOnSwing {
                JOptionPane.showMessageDialog(
                    dialogHost.ownerFrame(),
                    NO_UPDATE_AVAILABLE,
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE,
                )
            }
        } else {
            logger.info(NO_UPDATE_AVAILABLE)
        }
    }

    override fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean) {
        val dialogHost = dialogHost()
        if (showDialogs && dialogHost != null) {
            runOnSwing {
                SwingErrorDialog.showExceptionMessage(dialogHost.ownerFrame(), message, ex)
            }
        }
    }

    override fun showLoadFailedDialog() {
        val dialogHost = dialogHost() ?: return
        runOnSwing {
            JOptionPane.showMessageDialog(
                dialogHost.ownerFrame(),
                "Das Laden der Filmliste hat nicht geklappt!",
                Konstanten.PROGRAMMNAME,
                JOptionPane.ERROR_MESSAGE,
            )
        }
    }

    override suspend fun <T> withStatusBarWidgets(block: suspend (FilmListStatusBarWidgets) -> T): T {
        val statusHost = host ?: return NoOpFilmListLoadPresenter.withStatusBarWidgets(block)

        val widgets = withContext(Dispatchers.Swing) {
            val handle = statusHost.showStatusBarProgress()
            FilmListStatusBarWidgets(
                handle = StatusBarProgressHandleAdapter(handle),
                host = FilmListLoadHostAdapter(statusHost),
            )
        }
        try {
            return block(widgets)
        } finally {
            withContext(Dispatchers.Swing) {
                widgets.handle.close()
            }
        }
    }

    private fun dialogHost(): FilmListLoadHost? {
        val currentHost = host ?: return null
        if (CommandLineOptions.isDownloadAndQuit() || GraphicsEnvironment.isHeadless()) {
            return null
        }
        return currentHost
    }

    private fun runOnSwing(action: () -> Unit) {
        SwingUtilities.invokeLater(action)
    }

    private class StatusBarProgressHandleAdapter(
        private val delegate: StatusBarProgressHandle,
    ) : FilmListProgressHandle {
        override fun label() = delegate.label()

        override fun progressBar() = delegate.progressBar()

        override fun close() {
            delegate.close()
        }
    }

    private class FilmListLoadHostAdapter(
        private val delegate: FilmListLoadHost,
    ) : FilmListIndexingHost {
        override fun ownerFrame() = delegate.ownerFrame()

        override fun quitApplication() = delegate.quitApplication()

        override fun setFilmIndexingActionsEnabled(enabled: Boolean) {
            delegate.setFilmIndexingActionsEnabled(enabled)
        }
    }

    private companion object {
        private val logger = LogManager.getLogger(SwingFilmListLoadPresenter::class.java)
        private const val NO_UPDATE_AVAILABLE = "Es ist keine aktuellere Filmliste verfügbar."
    }
}
