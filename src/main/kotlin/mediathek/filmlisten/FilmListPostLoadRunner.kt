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
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import org.apache.logging.log4j.LogManager
import kotlin.coroutines.cancellation.CancellationException

internal class FilmListPostLoadRunner(
    private val filmCatalog: FilmCatalog,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val scope: CoroutineScope,
) {
    fun start(
        persistFilmList: Boolean,
        presenter: FilmListLoadPresenter,
        notifyFinished: (FilmListLoadProgress) -> Unit,
    ) {
        scope.launch {
            var completionProgress: FilmListLoadProgress? = null
            try {
                presenter.withStatusBarWidgets { widgets ->
                    buildPostLoadWorkerChain(persistFilmList, widgets)
                }
                completionProgress = FilmListLoadProgress.completed(failed = false)
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                logger.error("Post-load filmlist work failed", ex)
                completionProgress = FilmListLoadProgress.completed(failed = true)
            } finally {
                withContext(NonCancellable) {
                    completionProgress?.let { progress ->
                        withContext(Dispatchers.Swing) {
                            notifyFinished(progress)
                        }
                    }
                }
            }
        }
    }

    private suspend fun buildPostLoadWorkerChain(persistFilmList: Boolean, widgets: FilmListStatusBarWidgets) =
        FilmlistPostLoadTasks(
            filmCatalog,
            abos,
            blacklist,
            widgets.label,
            widgets.progressBar,
            widgets.host,
        ).run(persistFilmList)

    private companion object {
        private val logger = LogManager.getLogger(FilmListPostLoadRunner::class.java)
    }
}
