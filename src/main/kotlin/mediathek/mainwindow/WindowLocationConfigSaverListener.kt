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
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import mediathek.config.application.ApplicationConfiguration
import java.awt.Point
import java.awt.event.ComponentAdapter
import java.awt.event.ComponentEvent
import javax.swing.JFrame

internal class WindowLocationConfigSaverListener : ComponentAdapter() {
    private val configScope = CoroutineScope(SupervisorJob() + Dispatchers.IO.limitedParallelism(1))

    override fun componentResized(event: ComponentEvent) {
        val mainWindow = event.component as JFrame
        val dimensions = mainWindow.size

        if (dimensions.width < DEFAULT_WIDTH && dimensions.height < DEFAULT_HEIGHT) {
            mainWindow.setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT)
        }

        val resizedState = ResizedState(
            isMaximized = mainWindow.isMaximized,
            width = dimensions.width,
            height = dimensions.height,
        )
        configScope.launch { saveResizedState(resizedState) }
    }

    override fun componentMoved(event: ComponentEvent) {
        val mainWindow = event.component as JFrame
        val movedState = MovedState(
            isMaximized = mainWindow.isMaximized,
            location = mainWindow.location,
        )
        configScope.launch { saveMovedState(movedState) }
    }

    private fun saveResizedState(state: ResizedState) {
        ApplicationConfiguration.getInstance().setMainWindowResizedState(state.isMaximized, state.width, state.height)
    }

    private fun saveMovedState(state: MovedState) {
        ApplicationConfiguration.getInstance()
            .setMainWindowMovedState(state.isMaximized, state.location.x, state.location.y)
    }

    private val JFrame.isMaximized: Boolean
        get() = extendedState and JFrame.MAXIMIZED_BOTH == JFrame.MAXIMIZED_BOTH

    private data class ResizedState(
        val isMaximized: Boolean,
        val width: Int,
        val height: Int,
    )

    private data class MovedState(
        val isMaximized: Boolean,
        val location: Point,
    )

    private companion object {
        const val DEFAULT_WIDTH = 640
        const val DEFAULT_HEIGHT = 480
    }
}
