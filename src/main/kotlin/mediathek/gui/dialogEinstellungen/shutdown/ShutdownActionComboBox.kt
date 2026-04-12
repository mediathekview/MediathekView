/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.dialogEinstellungen.shutdown

import org.apache.logging.log4j.LogManager
import java.io.BufferedReader
import java.io.InputStreamReader
import javax.swing.DefaultComboBoxModel
import javax.swing.JComboBox

class ShutdownActionComboBox : JComboBox<String>() {
    init {
        model = DefaultComboBoxModel(ACTIONS.toTypedArray())
        selectedItem = readCurrentAction()

        addActionListener {
            (selectedItem as? String)?.let(::writeAction)
        }
    }

    private fun readCurrentAction(): String {
        try {
            val process = ProcessBuilder(COMMAND, "read", DOMAIN, KEY)
                .redirectErrorStream(true)
                .start()

            InputStreamReader(process.inputStream).use { inputStreamReader ->
                BufferedReader(inputStreamReader).use { reader ->
                    val line = reader.readLine()?.trim()?.lowercase()
                    if (line != null && line in ACTIONS) {
                        return line
                    }
                }
            }
        } catch (_: Exception) {
        }

        return "shutdown"
    }

    private fun writeAction(action: String) {
        try {
            ProcessBuilder(COMMAND, "write", DOMAIN, KEY, "-string", action)
                .inheritIO()
                .start()
                .waitFor()
        } catch (e: InterruptedException) {
            Thread.currentThread().interrupt()
            logger.error("Interrupted while writing shutdown action", e)
        } catch (e: Exception) {
            logger.error("Failed to write shutdown action", e)
        }
    }

    private companion object {
        const val DOMAIN = "org.mediathekview.mv_shutdown_helper"
        const val KEY = "shutdownAction"
        const val COMMAND = "/usr/bin/defaults"
        val ACTIONS = listOf("shutdown", "sleep", "restart")
        val logger = LogManager.getLogger(ShutdownActionComboBox::class.java)
    }
}
