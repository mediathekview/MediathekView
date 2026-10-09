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

import mediathek.config.CommandLineOptions
import mediathek.tool.timer.TimerPool
import org.apache.logging.log4j.LogManager
import java.util.concurrent.ForkJoinPool
import java.util.concurrent.TimeUnit

class MainWindowShutdownRuntime {
    fun shutdownTimerPool() {
        logger.trace("Entering shutdownTimerPool()")

        try {
            val taskList = TimerPool.shutdown(500, TimeUnit.MILLISECONDS)
            if (CommandLineOptions.isDebugModeEnabled() && taskList.isNotEmpty()) {
                logger.trace("timerPool taskList was not empty: {}", taskList.toString())
            }
        } catch (exception: InterruptedException) {
            Thread.currentThread().interrupt()
            logger.error("timerPool shutdown exception", exception)
        }

        logger.trace("Leaving shutdownTimerPool()")
    }

    fun waitForCommonPoolToComplete() {
        logger.trace("Entering waitForCommonPoolToComplete()")

        val pool = ForkJoinPool.commonPool()
        if (!pool.awaitQuiescence(COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS.toLong(), TimeUnit.SECONDS)) {
            logger.warn(
                "Common pool did not become quiescent within {} seconds. Continuing shutdown.",
                COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS,
            )
        }

        logger.trace("Leaving waitForCommonPoolToComplete()")
    }

    private companion object {
        private val logger = LogManager.getLogger(MainWindowShutdownRuntime::class.java)
        private const val COMMON_POOL_SHUTDOWN_TIMEOUT_SECONDS = 5
    }
}
