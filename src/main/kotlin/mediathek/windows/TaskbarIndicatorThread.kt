/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.windows

import mediathek.tool.threads.IndicatorThread
import org.apache.logging.log4j.LogManager
import java.awt.Taskbar
import java.lang.foreign.*
import java.lang.invoke.MethodHandle
import java.util.concurrent.TimeUnit
import javax.swing.JFrame

internal class TaskbarIndicatorThread(parent: MediathekGuiWindows) : IndicatorThread() {
    private val parent: JFrame
    private val setThreadExecutionState: MethodHandle?

    private fun disableStandby() {
        val res = setThreadExecutionState?.invoke(WinFlags.ES_CONTINUOUS or WinFlags.ES_SYSTEM_REQUIRED) ?: 0
        if (res as Int == 0) {
            logger.error("disableStandby() failed!")
        }
    }

    private fun enableStandby() {
        val res = setThreadExecutionState?.invoke(WinFlags.ES_CONTINUOUS) ?: 0
        if (res as Int == 0) {
            logger.error("enableStandby() failed!")
        }
    }

    override fun run() {
        val taskbar = Taskbar.getTaskbar()
        try {
            while (!isInterrupted) {
                val percentage = calculateOverallPercentage().toInt()
                taskbar.setWindowProgressValue(parent, percentage)
                taskbar.setWindowProgressState(parent, Taskbar.State.NORMAL)
                disableStandby()
                TimeUnit.MILLISECONDS.sleep(500)
            }
        } catch (_: InterruptedException) {
        } finally {
            //when we are finished, stop progress
            taskbar.setWindowProgressState(parent, Taskbar.State.OFF)
            enableStandby()
        }
    }

    companion object {
        private val logger = LogManager.getLogger()
    }

    init {
        name = "TaskbarIndicatorThread"
        this.parent = parent

        Linker.nativeLinker().let { linker ->
            val kernel32 = SymbolLookup.libraryLookup("kernel32.dll", Arena.global())
            setThreadExecutionState = linker.downcallHandle(
                kernel32.find("SetThreadExecutionState").orElseThrow(),
                FunctionDescriptor.of(ValueLayout.JAVA_INT, ValueLayout.JAVA_INT)
            )
        }
    }
}