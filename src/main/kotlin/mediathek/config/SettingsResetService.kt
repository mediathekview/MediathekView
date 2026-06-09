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

package mediathek.config

import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import javax.swing.JOptionPane

object SettingsResetService {
    private val logger = LogManager.getLogger()

    @JvmStatic
    fun moveSettingsDirectoryAside() {
        val source = StandardLocations.getSettingsDirectory()
        try {
            val timestamp = DateTimeFormatter.ofPattern("yyyy.MM.dd__HH.mm.ss")
                .format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()))
            val target = source.resolveSibling("${source.fileName}--$timestamp")

            Files.move(source, target, StandardCopyOption.REPLACE_EXISTING)
            Files.deleteIfExists(source)
        } catch (e: IOException) {
            logger.error("Die Einstellungen konnten nicht zurückgesetzt werden.", e)
            val message = "Die Einstellungen konnten nicht zurückgesetzt werden.\n" +
                "Sie müssen jetzt das Programm beenden und dann den Ordner:\n" +
                StandardLocations.getSettingsDirectory() + '\n' +
                "von Hand löschen und dann das Programm wieder starten.\n\n" +
                "Im Forum erhalten Sie weitere Hilfe."
            JOptionPane.showMessageDialog(null, message, Konstanten.PROGRAMMNAME, JOptionPane.ERROR_MESSAGE)
        }
    }
}
