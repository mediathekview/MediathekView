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

package mediathek.config.application

import org.apache.commons.configuration2.XMLConfiguration
import org.apache.commons.configuration2.sync.LockMode
import org.apache.logging.log4j.LogManager
import java.nio.file.Path

class ApplicationConfigurationCleanupService(
    private val config: XMLConfiguration,
    private val keyRegistry: ApplicationConfigurationKeyRegistry = AnnotatedApplicationConfigurationKeyRegistry,
) {
    fun cleanup(
        settingsPath: Path,
        backupPath: Path?,
        dryRun: Boolean,
    ): ApplicationConfigurationCleanupStatistics {
        val allKeysBefore = config.withLock(LockMode.READ) {
            getKeys().asSequence().toList()
        }
        val removableKeys = allKeysBefore
            .filterNot(keyRegistry::isValidKey)
            .sorted()

        if (!dryRun) {
            config.withLock(LockMode.WRITE) {
                removableKeys.forEach { key ->
                    logger.info("Removing obsolete application configuration key: {}", key)
                    clearProperty(key)
                }
            }
        } else {
            removableKeys.forEach { key ->
                logger.info("Dry run: obsolete application configuration key would be removed: {}", key)
            }
        }

        val totalKeysAfter = config.withLock(LockMode.READ) {
            getKeys().asSequence().count()
        }

        return ApplicationConfigurationCleanupStatistics(
            settingsPath = settingsPath,
            backupPath = backupPath,
            totalKeysBefore = allKeysBefore.size,
            totalKeysAfter = totalKeysAfter,
            removedKeys = removableKeys,
            dryRun = dryRun,
        )
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
