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

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.NonCancellable
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.config.StandardLocations
import mediathek.filmlisten.FilmlistPostLoadTasks
import mediathek.filmlisten.reader.FilmListReader
import mediathek.gui.messages.FilmListReadStartEvent
import mediathek.gui.messages.FilmListReadStopEvent
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import javax.swing.JLabel
import javax.swing.JProgressBar
import kotlin.coroutines.cancellation.CancellationException

fun interface StartupFilmlistLoadCompletion {
    fun complete(remoteUpdateStarted: Boolean, failed: Boolean)
}

class StartupFilmlistLoader(
    private val daten: Daten,
    private val progressLabel: JLabel,
    private val progressBar: JProgressBar,
    private val completion: StartupFilmlistLoadCompletion,
) {
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)

    fun start() {
        scope.launch {
            var remoteUpdateStarted = false
            var failed = false
            try {
                readStartupFilmlist()
                remoteUpdateStarted = startRemoteFilmlistUpdateIfNeeded()
                if (!remoteUpdateStarted) {
                    runPostLoadTasks()
                }
            } catch (ex: CancellationException) {
                failed = true
                throw ex
            } catch (ex: Exception) {
                logger.error("loadFilmlist()", ex)
                failed = true
            } finally {
                withContext(NonCancellable + Dispatchers.Swing) {
                    completion.complete(remoteUpdateStarted, failed)
                }
            }
        }
    }

    private fun readStartupFilmlist() {
        logger.trace("Reading local filmlist")
        MessageBus.messageBus.publishAsync(FilmListReadStartEvent())

        try {
            FilmListReader().use { reader ->
                val loadNumDays = ApplicationConfiguration.getConfiguration()
                    .getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0)
                reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), daten.listeFilme, loadNumDays)
            }
        } finally {
            MessageBus.messageBus.publishAsync(FilmListReadStopEvent())
        }
    }

    private fun startRemoteFilmlistUpdateIfNeeded(): Boolean {
        logger.trace("Check for filmlist updates")
        return daten.filmeLaden.startAutomaticStartupUpdateIfNeeded()
    }

    private suspend fun runPostLoadTasks() =
        FilmlistPostLoadTasks(daten, progressLabel, progressBar).run(writeFilmList = false)

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
