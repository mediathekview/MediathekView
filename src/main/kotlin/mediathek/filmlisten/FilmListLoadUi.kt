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

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.config.CommandLineOptions
import mediathek.config.Konstanten
import mediathek.mainwindow.FilmListLoadHost
import mediathek.mainwindow.StatusBarProgressHandle
import mediathek.tool.SwingErrorDialog
import org.apache.logging.log4j.LogManager
import java.awt.GraphicsEnvironment
import javax.swing.JLabel
import javax.swing.JOptionPane
import javax.swing.JProgressBar

internal data class FilmListStatusBarWidgets(
    val handle: StatusBarProgressHandle,
    val attachedToStatusBar: Boolean,
    val host: FilmListLoadHost?,
) {
    val label: JLabel
        get() = handle.label()

    val progressBar: JProgressBar
        get() = handle.progressBar()
}

internal class FilmListLoadUi(
    private val scope: CoroutineScope,
) : FilmListImportFeedback {
    @Volatile
    private var host: FilmListLoadHost? = null

    fun setHost(host: FilmListLoadHost?) {
        this.host = host
    }

    val currentHost: FilmListLoadHost?
        get() = host

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

    fun showLoadFailedDialog() {
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

    suspend fun attachStatusBarWidgets(host: FilmListLoadHost?): FilmListStatusBarWidgets {
        if (host != null) {
            return withContext(Dispatchers.Swing) {
                FilmListStatusBarWidgets(
                    handle = host.showStatusBarProgress(),
                    attachedToStatusBar = true,
                    host = host,
                )
            }
        }
        return FilmListStatusBarWidgets(NoStatusBarProgressHandle(), attachedToStatusBar = false, host = null)
    }

    suspend fun detachStatusBarWidgets(widgets: FilmListStatusBarWidgets) {
        if (widgets.attachedToStatusBar) {
            withContext(Dispatchers.Swing) {
                widgets.handle.close()
            }
        } else {
            widgets.handle.close()
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
        scope.launch(Dispatchers.Swing) {
            action()
        }
    }

    private class NoStatusBarProgressHandle : StatusBarProgressHandle {
        private val label = JLabel()
        private val progressBar = JProgressBar()

        override fun label(): JLabel = label

        override fun progressBar(): JProgressBar = progressBar

        override fun close() {
        }
    }

    private companion object {
        private val logger = LogManager.getLogger(FilmListLoadUi::class.java)
        private const val NO_UPDATE_AVAILABLE = "Es ist keine aktuellere Filmliste verfügbar."
    }
}
