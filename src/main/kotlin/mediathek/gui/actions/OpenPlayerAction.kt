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

import mediathek.config.MVConfig
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

    @JvmStatic
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
                MVConfig.add(PLAYER_CONFIG, program)
                publishProgramLocationChanged()
                success = true
            } catch (_: Exception) {
                logger.error("Ordner öffnen: {}", datei, ex)
            }
        } finally {
            if (!success) {
                MVConfig.add(PLAYER_CONFIG, "")
                publishProgramLocationChanged()
                showPlayerOpenError(parent)
            }
        }
    }

    private fun configuredPlayer(): String = MVConfig.get(PLAYER_CONFIG)

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
        val text = "\n Ein Videoplayer zum Abspielen wird nicht gefunden.\n Videoplayer selbst auswählen."
        val dialog = DialogProgrammOrdnerOeffnen(parent, true, "", "Videoplayer suchen", text)
        dialog.isVisible = true
        return if (dialog.ok) dialog.ziel else ""
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

    private val PLAYER_CONFIG = MVConfig.Configs.SYSTEM_PLAYER_ABSPIELEN
}
