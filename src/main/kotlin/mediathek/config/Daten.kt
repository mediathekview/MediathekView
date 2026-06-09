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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import kotlinx.coroutines.*
import mediathek.SplashScreenLifecycle
import mediathek.controller.IoXmlLesen
import mediathek.controller.IoXmlSchreiben
import mediathek.controller.history.AboHistoryController
import mediathek.controller.starter.DownloadStartCoordinator
import mediathek.daten.*
import mediathek.daten.blacklist.ListeBlacklist
import mediathek.filmlisten.FilmeLaden
import mediathek.gui.bookmark.BookmarkDataList
import mediathek.gui.duplicates.FilmStatistics
import mediathek.tool.GermanStringSorter
import mediathek.tool.ReplaceList
import mediathek.tool.SenderListBoxModel
import org.apache.logging.log4j.LogManager
import java.nio.file.Files
import java.nio.file.Path
import java.util.concurrent.ExecutionException
import javax.swing.JOptionPane

class Daten private constructor() {
    private val historyScope = CoroutineScope(SupervisorJob() + Dispatchers.IO)

    val listePset: ListePset = ListePset()
    val duplicateStatistics: EventList<FilmStatistics> = BasicEventList()
    val commonStatistics: EventList<FilmStatistics> = BasicEventList()
    val filmeLaden: FilmeLaden = FilmeLaden(this)

    /**
     * "source" list of all entries, contains everything
     */
    val listeFilme: ListeFilme = ListeFilme()
    val listeDownloads: ListeDownloads = ListeDownloads()
    val listeDownloadsButton: ListeDownloads = ListeDownloads()
    val listeBlacklist: ListeBlacklist = ListeBlacklist()
    val listeBookmarkList: BookmarkDataList = BookmarkDataList(this)
    val listeAbo: ListeAbo = ListeAbo()
    val downloadInfos: DownloadInfos = DownloadInfos()
    val downloadStartCoordinator: DownloadStartCoordinator = DownloadStartCoordinator(this)

    /**
     * "the" final list of films after all filtering is done.
     * Defaults to no lucene index unless changed at startup.
     */
    var listeFilmeNachBlackList: ListeFilme = ListeFilme()

    /**
     * erfolgreich geladene Abos.
     */
    private var erledigteAbos: AboHistoryController? = null
    private var backupAlreadyHandled = false
    private var aboHistoryJob: Deferred<Unit>? = null

    val allSendersList: EventList<String> = SortedList<String>(SenderListBoxModel.providedSenderList).apply {
        setComparator(GermanStringSorter)
    }

    val aboHistoryController: AboHistoryController
        get() = erledigteAbos!!

    fun allesLaden(): Boolean {
        if (!load()) {
            logger.info("Weder Konfig noch Backup konnte geladen werden!")
            clearKonfig()
            return false
        }
        logger.info("Konfig wurde gelesen!")
        MVColor.load()

        return true
    }

    fun launchHistoryDataLoading() {
        logger.trace("launching async history data loading")
        val loadingJob = historyScope.async {
            setAboHistoryList(AboHistoryController())
        }
        loadingJob.invokeOnCompletion { throwable ->
            if (throwable != null) {
                logger.error("launchAboHistoryController", throwable)
            }
        }
        aboHistoryJob = loadingJob
    }

    @Throws(ExecutionException::class, InterruptedException::class)
    fun waitForHistoryDataLoadingToComplete() {
        val runningHistoryLoad = aboHistoryJob ?: return

        try {
            runBlocking {
                runningHistoryLoad.await()
            }
        } catch (exception: InterruptedException) {
            throw exception
        } catch (exception: Throwable) {
            throw ExecutionException(exception)
        } finally {
            if (aboHistoryJob === runningHistoryLoad) {
                aboHistoryJob = null
            }
        }
    }

    private fun setAboHistoryList(controller: AboHistoryController) {
        erledigteAbos = controller
    }

    private fun clearKonfig() {
        listePset.clear()
        ReplaceList.clear()
        listeAbo.clear()
        listeDownloads.clear()
        listeBlacklist.clear()
        listeBookmarkList.clear()
    }

    private fun load(): Boolean {
        val xmlFilePath = StandardLocations.getMediathekXmlFile()

        if (Files.exists(xmlFilePath)) {
            val configReader = IoXmlLesen()
            if (configReader.datenLesen(xmlFilePath)) {
                return true
            }
            logger.info("Konfig konnte nicht gelesen werden!")
        } else {
            logger.info("Konfig existiert nicht!")
        }

        return loadBackup()
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

    private fun loadBackup(): Boolean {
        val backupPaths = mediathekXmlCopyFilePath
        if (backupPaths.isEmpty()) {
            logger.info("Es gibt kein Backup")
            return false
        }

        SplashScreenLifecycle.close()
        logger.info("Es gibt ein Backup")

        if (askForBackupRestore()) {
            for (path in backupPaths) {
                clearKonfig()
                logger.info("Versuch Backup zu laden: {}", path.toString())
                val configReader = IoXmlLesen()
                if (configReader.datenLesen(path)) {
                    logger.info("Backup hat geklappt: {}", path.toString())
                    return true
                }
            }
        }

        return false
    }

    fun allesSpeichern() {
        if (!backupAlreadyHandled) {
            backupAlreadyHandled = ConfigurationBackupService.createConfigurationBackupCopies()
        }

        val configWriter = IoXmlSchreiben()
        configWriter.writeConfigurationFile(StandardLocations.getMediathekXmlFile())
    }

    companion object {
        private val logger = LogManager.getLogger(Daten::class.java)

        private val mediathekXmlCopyFilePath: List<Path>
            get() = buildList {
                for (copyIndex in 1..Konstanten.MAX_NUM_BACKUP_FILE_COPIES) {
                    val path = StandardLocations.getSettingsDirectory().resolve(Konstanten.CONFIG_FILE_COPY + copyIndex)
                    if (Files.exists(path)) {
                        add(path)
                    }
                }
            }

        private val INSTANCE = Daten()

        @JvmStatic
        fun getInstance(): Daten = INSTANCE
    }
}
