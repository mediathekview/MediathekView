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

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import org.apache.logging.log4j.LogManager
import kotlin.coroutines.cancellation.CancellationException

internal class FilmListPostLoadRunner(
    private val daten: Daten,
    private val scope: CoroutineScope,
    private val ui: FilmListLoadUi,
) {
    fun start(
        writeFilmList: Boolean,
        widgets: FilmListStatusBarWidgets,
        notifyFinished: (ListenerFilmeLadenEvent) -> Unit,
    ) {
        scope.launch {
            var completionEvent: ListenerFilmeLadenEvent? = null
            try {
                buildPostLoadWorkerChain(writeFilmList, widgets)
                completionEvent = ListenerFilmeLadenEvent("", "", 100, 100, false)
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                logger.error("Post-load filmlist work failed", ex)
                completionEvent = ListenerFilmeLadenEvent("", "", 100, 100, true)
            } finally {
                withContext(NonCancellable) {
                    try {
                        completionEvent?.let { event ->
                            withContext(Dispatchers.Swing) {
                                notifyFinished(event)
                            }
                        }
                    } finally {
                        ui.detachStatusBarWidgets(widgets)
                    }
                }
            }
        }
    }

    private suspend fun buildPostLoadWorkerChain(writeFilmList: Boolean, widgets: FilmListStatusBarWidgets) =
        FilmlistPostLoadTasks(daten, widgets.label, widgets.progressBar, widgets.host).run(writeFilmList)

    private companion object {
        private val logger = LogManager.getLogger(FilmListPostLoadRunner::class.java)
    }
}
