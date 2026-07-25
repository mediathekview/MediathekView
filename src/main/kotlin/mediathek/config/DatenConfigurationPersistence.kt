/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.config

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.swing.Swing
import kotlinx.coroutines.withContext
import mediathek.SplashScreenLifecycle
import mediathek.controller.*
import mediathek.controller.starter.DownloadServices
import mediathek.daten.ProgramSetRepository
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.gui.bookmark.BookmarkServices
import mediathek.tool.ReplacementRules
import org.apache.logging.log4j.LogManager
import java.nio.file.Files
import java.nio.file.Path
import javax.swing.JOptionPane

@Suppress("LoggingSimilarMessage")
class DatenConfigurationPersistence(
    private val programSets: ProgramSetRepository,
    private val downloads: DownloadServices,
    private val blacklist: BlacklistServices,
    private val abos: AboServices,
    private val bookmarks: BookmarkServices,
    private val replacementRules: ReplacementRules,
) {
    private var backupAlreadyHandled = false

    fun loadAll(): Boolean {
        if (!load(::askForBackupRestore)) {
            logger.info("Weder Konfig noch Backup konnte geladen werden!")
            clearConfiguration()
            return false
        }
        logger.info("Konfig wurde gelesen!")
        MVColor.load()

        return true
    }

    suspend fun loadAllForGui(): Boolean {
        if (!loadForGui()) {
            logger.info("Weder Konfig noch Backup konnte geladen werden!")
            withIoContext {
                clearConfiguration()
            }
            return false
        }

        logger.info("Konfig wurde gelesen!")
        withIoContext {
            MVColor.load()
        }

        return true
    }

    fun saveAll() {
        if (!backupAlreadyHandled) {
            backupAlreadyHandled = ConfigurationBackupService.createConfigurationBackupCopies()
        }

        val configWriter = IoXmlSchreiben(configData())
        configWriter.writeConfigurationFile(StandardLocations.getMediathekXmlFile())
        writeBlacklistRules()
        writeAboRules()
    }

    private fun clearConfiguration() {
        programSets.clear()
        replacementRules.clear()
        abos.list.clearWithoutNotification()
        downloads.clearQueuedDownloads()
        blacklist.rules.clear()
        bookmarks.list.clear()
    }

    private fun load(askForBackupRestore: () -> Boolean): Boolean {
        val xmlFilePath = StandardLocations.getMediathekXmlFile()

        if (Files.exists(xmlFilePath)) {
            if (readConfiguration(xmlFilePath)) {
                return true
            }
            logger.info("Konfig konnte nicht gelesen werden!")
        } else {
            logger.info("Konfig existiert nicht!")
        }

        return loadBackup(askForBackupRestore)
    }

    private suspend fun loadForGui(): Boolean {
        val xmlFilePath = StandardLocations.getMediathekXmlFile()

        if (withIoContext { Files.exists(xmlFilePath) }) {
            if (withIoContext { readConfiguration(xmlFilePath) }) {
                return true
            }
            logger.info("Konfig konnte nicht gelesen werden!")
        } else {
            logger.info("Konfig existiert nicht!")
        }

        return loadBackupForGui()
    }

    private fun askForBackupRestore(): Boolean {
        if (CommandLineOptions.isDownloadAndQuit()) {
            logger.error("CLI download mode does not support interactive backup restore.")
            return false
        }
        val text = """
            Die Einstellungen sind beschädigt und können nicht geladen werden.
            Soll versucht werden diese aus einem Backup wiederherzustellen?
        """.trimIndent()
        val answer = JOptionPane.showConfirmDialog(
            null,
            text,
            Konstanten.PROGRAMMNAME,
            JOptionPane.YES_NO_OPTION,
        )
        return if (answer == JOptionPane.YES_OPTION) {
            true
        } else {
            logger.info("User will kein Backup laden.")
            false
        }
    }

    private fun loadBackup(askForBackupRestore: () -> Boolean): Boolean {
        val backupPaths = mediathekXmlCopyFilePath
        if (backupPaths.isEmpty()) {
            logger.info("Es gibt kein Backup")
            return false
        }

        SplashScreenLifecycle.close()
        logger.info("Es gibt ein Backup")

        if (askForBackupRestore()) {
            for (path in backupPaths) {
                if (loadBackupFile(path)) {
                    return true
                }
            }
        }

        return false
    }

    private suspend fun loadBackupForGui(): Boolean {
        val backupPaths = withIoContext { mediathekXmlCopyFilePath }
        if (backupPaths.isEmpty()) {
            logger.info("Es gibt kein Backup")
            return false
        }

        withContext(Dispatchers.Swing) {
            SplashScreenLifecycle.close()
        }
        logger.info("Es gibt ein Backup")

        val shouldRestoreBackup = withContext(Dispatchers.Swing) {
            askForBackupRestore()
        }
        if (shouldRestoreBackup) {
            for (path in backupPaths) {
                if (withIoContext { loadBackupFile(path) }) {
                    return true
                }
            }
        }

        return false
    }

    private fun readConfiguration(path: Path): Boolean =
        IoXmlLesen(configData()).datenLesen(path)

    private fun loadBackupFile(path: Path): Boolean {
        clearConfiguration()
        logger.info("Versuch Backup zu laden: {}", path.toString())
        if (readConfiguration(path)) {
            logger.info("Backup hat geklappt: {}", path.toString())
            return true
        }
        return false
    }

    private suspend fun <T> withIoContext(block: () -> T): T =
        withContext(Dispatchers.IO) { block() }

    private fun writeBlacklistRules() {
        try {
            BlacklistRuleStorage.write(StandardLocations.getBlacklistRulesFilePath(), blacklist.rules)
        } catch (ex: Exception) {
            logger.error("Failed to write blacklist rules", ex)
        }
    }

    private fun writeAboRules() {
        try {
            AboRuleStorage.write(StandardLocations.getAboRulesFilePath(), abos.list)
        } catch (ex: Exception) {
            logger.error("Failed to write abo rules", ex)
        }
    }

    private fun configData(): XmlConfigData =
        XmlConfigData(
            programSets = programSets.list,
            downloads = downloads,
            blacklistRules = blacklist.rules,
            abos = abos.list,
            replacementRules = replacementRules,
        )

    private companion object {
        private val logger = LogManager.getLogger(DatenConfigurationPersistence::class.java)

        private val mediathekXmlCopyFilePath: List<Path>
            get() = buildList {
                for (copyIndex in 1..Konstanten.MAX_NUM_BACKUP_FILE_COPIES) {
                    val path = StandardLocations.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + copyIndex)
                    if (Files.exists(path)) {
                        add(path)
                    }
                }
            }
    }
}
