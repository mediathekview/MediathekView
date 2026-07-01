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

package mediathek.gui.actions

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen
import mediathek.gui.messages.ProgramLocationChangedEvent
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import java.awt.Desktop
import java.awt.Frame
import java.io.File
import javax.swing.JOptionPane

object OpenPlayerAction {
    private val logger = LogManager.getLogger(OpenPlayerAction::class.java)

    fun filmAbspielen(parent: Frame, datei: String) {
        var success = false
        if (datei.isEmpty()) {
            return
        }

        val file = File(datei)
        if (!file.exists()) {
            JOptionPane.showMessageDialog(
                parent,
                "Film existiert noch nicht!",
                "Fehler",
                JOptionPane.ERROR_MESSAGE,
            )
            return
        }

        try {
            val program = configuredPlayer()
            if (program.isNotEmpty()) {
                openWithProgram(program, file)
                success = true
            } else {
                success = openWithDesktop(file)
            }
        } catch (ex: Exception) {
            try {
                success = false
                val program = resolvePlayerProgram(parent)
                openWithProgram(program, file)
                ApplicationConfiguration.getInstance().videoPlayerProgram = program
                publishProgramLocationChanged()
                success = true
            } catch (_: Exception) {
                logger.error("Ordner öffnen: {}", datei, ex)
            }
        } finally {
            if (!success) {
                ApplicationConfiguration.getInstance().videoPlayerProgram = ""
                publishProgramLocationChanged()
                showPlayerOpenError(parent)
            }
        }
    }

    private fun configuredPlayer(): String = ApplicationConfiguration.getInstance().videoPlayerProgram

    private fun openWithProgram(program: String, file: File) {
        Runtime.getRuntime().exec(arrayOf(program, file.absolutePath))
    }

    private fun openWithDesktop(file: File): Boolean {
        if (!Desktop.isDesktopSupported()) {
            return false
        }

        val desktop = Desktop.getDesktop()
        if (!desktop.isSupported(Desktop.Action.OPEN)) {
            return false
        }

        desktop.open(file)
        return true
    }

    private fun resolvePlayerProgram(parent: Frame): String {
        val text = "Ein Videoplayer zum Abspielen wird nicht gefunden. Videoplayer selbst auswählen."
        return DialogProgrammOrdnerOeffnen.showDialog(parent, "", "Videoplayer suchen", text).orElse("")
    }

    private fun publishProgramLocationChanged() {
        MessageBus.messageBus.publishAsync(ProgramLocationChangedEvent())
    }

    private fun showPlayerOpenError(parent: Frame) {
        JOptionPane.showMessageDialog(
            parent,
            "Kann den Videoplayer nicht öffnen!",
            "Fehler",
            JOptionPane.ERROR_MESSAGE,
        )
    }
}
