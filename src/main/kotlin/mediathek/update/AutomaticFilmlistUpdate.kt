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

package mediathek.update

import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import org.apache.logging.log4j.LogManager
import java.io.Closeable
import kotlin.time.Duration.Companion.hours

/**
 * Perform one filmlist update automatically every 12 hours if program is running long enough.
 */
class AutomaticFilmlistUpdate(
    private val updateAction: Runnable,
) : Closeable {
    private val job = SupervisorJob()
    private val scope = CoroutineScope(job + Dispatchers.Default + CoroutineExceptionHandler { _, ex ->
        logger.error("Automatic film list update failed", ex)
    })
    private var updateJob: Job? = null

    fun start() {
        logger.debug("AutomaticFilmlistUpdate Started.")
        updateJob?.cancel()
        updateJob = scope.launch {
            delay(UPDATE_INTERVAL)
            while (isActive) {
                reloadFilmList()
                delay(UPDATE_INTERVAL)
            }
        }
    }

    private suspend fun reloadFilmList() {
        logger.debug("Automatic FilmList load started.")
        withContext(Dispatchers.Swing) {
            updateAction.run()
        }
        logger.debug("Automatic FilmList load finished.")
    }

    override fun close() {
        job.cancel()
        logger.debug("AutomaticFilmlistUpdate closed.")
    }

    private companion object {
        private val logger = LogManager.getLogger(AutomaticFilmlistUpdate::class.java)
        private val UPDATE_INTERVAL = 12.hours
    }
}
