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

import org.apache.logging.log4j.LogManager

fun interface ShutdownEdtRunner {
    fun run(description: String, action: Runnable)
}

enum class ShutdownThread {
    EDT,
    BACKGROUND,
}

data class ShutdownStep(
    val description: String,
    val thread: ShutdownThread,
    val action: Runnable,
)

class ShutdownCoordinator(
    private val edtRunner: ShutdownEdtRunner,
) {
    private val steps = mutableListOf<ShutdownStep>()

    fun register(step: ShutdownStep): ShutdownCoordinator {
        steps += step
        return this
    }

    fun shutdown() {
        steps.forEach(::runStep)
    }

    private fun runStep(step: ShutdownStep) {
        logger.trace(step.description)
        try {
            when (step.thread) {
                ShutdownThread.EDT -> edtRunner.run(step.description, step.action)
                ShutdownThread.BACKGROUND -> step.action.run()
            }
        } catch (ex: RuntimeException) {
            logger.error("Shutdown step failed: {}", step.description, ex)
        }
    }

    companion object {
        private val logger = LogManager.getLogger(ShutdownCoordinator::class.java)
    }
}
