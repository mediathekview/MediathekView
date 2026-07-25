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

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.filmlisten.*
import mediathek.filmlisten.reader.FilmListReader
import mediathek.gui.messages.FilmListReadStartEvent
import mediathek.gui.messages.FilmListReadStopEvent
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.JLabel
import javax.swing.JProgressBar
import kotlin.coroutines.cancellation.CancellationException

fun interface StartupFilmlistLoadCompletion {
    fun complete()
}

class StartupFilmlistLoader(
    private val filmCatalog: FilmCatalog,
    private val filmListLoader: FilmListLoadCoordinator,
    progressLabel: JLabel,
    progressBar: JProgressBar,
    private val completion: StartupFilmlistLoadCompletion,
) : AutoCloseable {
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val closed = AtomicBoolean(false)
    private val startupPresenter = StartupFilmListLoadPresenter(progressLabel, progressBar)

    fun start() {
        scope.launch {
            var completionHandle: FilmListLoadHandle? = null
            var failed = false
            try {
                readStartupFilmlist()
                val remoteUpdate = startRemoteFilmlistUpdateIfNeeded()
                completionHandle = if (remoteUpdate.started) {
                    remoteUpdate
                } else {
                    filmListLoader.startStartupPostLoad(failed = false, startupPresenter)
                }
            } catch (ex: CancellationException) {
                failed = true
                throw ex
            } catch (ex: Exception) {
                logger.error("startFilmlistLoad()", ex)
                failed = true
            } finally {
                if (!closed.get()) {
                    val handle = completionHandle ?: filmListLoader.startStartupPostLoad(failed, startupPresenter)
                    awaitStartupCompletion(handle)
                    withContext(NonCancellable + Dispatchers.Swing) {
                        completion.complete()
                    }
                }
            }
        }
    }

    private fun readStartupFilmlist() {
        logger.trace("Reading local filmlist")
        MessageBus.messageBus.publishAsync(FilmListReadStartEvent())

        try {
            FilmListReader().use { reader ->
                val loadNumDays = ApplicationConfiguration.getInstance().filmListLoadNumDays
                reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), filmCatalog.allFilms, loadNumDays)
            }
        } finally {
            MessageBus.messageBus.publishAsync(FilmListReadStopEvent())
        }
    }

    private fun startRemoteFilmlistUpdateIfNeeded(): FilmListLoadHandle {
        logger.trace("Check for filmlist updates")
        return filmListLoader.startAutomaticStartupUpdate()
    }

    private suspend fun awaitStartupCompletion(handle: FilmListLoadHandle) {
        if (!handle.started) {
            return
        }

        try {
            handle.completion.await()
        } catch (ex: CancellationException) {
            throw ex
        } catch (ex: Exception) {
            logger.error("startup filmlist completion", ex)
        }
    }

    override fun close() {
        if (closed.compareAndSet(false, true)) {
            scope.cancel()
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}

private class StartupFilmListLoadPresenter(
    private val label: JLabel,
    private val progressBar: JProgressBar,
) : FilmListLoadPresenter {
    override fun showNoUpdateAvailable(showDialogs: Boolean) {
    }

    override fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean) {
    }

    override fun showLoadFailedDialog() {
    }

    override suspend fun <T> withStatusBarWidgets(block: suspend (FilmListStatusBarWidgets) -> T): T =
        block(FilmListStatusBarWidgets(StartupProgressHandle(label, progressBar), host = null))
}

private class StartupProgressHandle(
    private val label: JLabel,
    private val progressBar: JProgressBar,
) : FilmListProgressHandle {
    override fun label(): JLabel = label

    override fun progressBar(): JProgressBar = progressBar

    override fun close() {
    }
}
