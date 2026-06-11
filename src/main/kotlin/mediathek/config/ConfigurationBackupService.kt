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
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.time.LocalDate
import java.time.ZoneId

object ConfigurationBackupService {
    private val logger = LogManager.getLogger()

    fun createConfigurationBackupCopies(): Boolean {
        return try {
            deleteSuperfluousConfigurationBackups()
            if (backupCreatedToday(configurationBackupPath(1))) {
                logger.info("Einstellungen wurden heute schon gesichert")
            } else {
                rotateConfigurationBackups()
                moveCurrentConfigurationToBackup()
                logger.info("Einstellungen wurden gesichert")
            }
            true
        } catch (e: IOException) {
            logger.error("Die Einstellungen konnten nicht komplett gesichert werden!", e)
            false
        } finally {
        }
    }

    private fun backupCreatedToday(path: Path): Boolean {
        if (!Files.exists(path)) {
            return false
        }

        val lastModifiedDate = Files.getLastModifiedTime(path)
            .toInstant()
            .atZone(ZoneId.systemDefault())
            .toLocalDate()
        return lastModifiedDate == LocalDate.now()
    }

    private fun rotateConfigurationBackups() {
        for (index in Konstanten.MAX_NUM_BACKUP_FILE_COPIES downTo 2) {
            val source = configurationBackupPath(index - 1)
            if (Files.exists(source)) {
                Files.move(source, configurationBackupPath(index), StandardCopyOption.REPLACE_EXISTING)
            }
        }
    }

    private fun deleteSuperfluousConfigurationBackups() {
        Files.newDirectoryStream(
            StandardLocations.getSettingsDirectory(),
            "${Konstanten.CONFIG_FILE_COPY}*",
        ).use { backupPaths ->
            backupPaths
                .filter { path -> Files.isRegularFile(path) && path.isSuperfluousConfigurationBackup() }
                .forEach { path -> Files.deleteIfExists(path) }
        }
    }

    private fun Path.isSuperfluousConfigurationBackup(): Boolean {
        val copyIndex = fileName.toString()
            .removePrefix(Konstanten.CONFIG_FILE_COPY)
            .toIntOrNull()
        return copyIndex != null && copyIndex > Konstanten.MAX_NUM_BACKUP_FILE_COPIES
    }

    private fun moveCurrentConfigurationToBackup() {
        val configurationPath = StandardLocations.getMediathekXmlFile()
        if (Files.exists(configurationPath)) {
            Files.move(configurationPath, configurationBackupPath(1), StandardCopyOption.REPLACE_EXISTING)
        }
    }

    private fun configurationBackupPath(index: Int): Path =
        StandardLocations.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + index)
}
