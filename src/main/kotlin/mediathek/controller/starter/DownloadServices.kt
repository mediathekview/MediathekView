package mediathek.controller.starter

import mediathek.config.CommandLineOptions
import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.*
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.messages.ButtonStartEvent
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.gui.messages.DownloadQueueRankChangedEvent
import mediathek.gui.messages.StartEvent
import mediathek.mainwindow.MainWindowHandle
import mediathek.tool.MessageBus
import mediathek.tool.datum.DateUtil
import mediathek.tool.models.TModelDownload
import org.apache.logging.log4j.LogManager
import java.time.LocalDate
import java.util.*
import java.util.function.Predicate
import javax.swing.JFrame

data class DownloadProgressSnapshot(
    val bandwidthText: String,
    val activeBytes: Long,
    val totalBytes: Long,
)

class DownloadServices(
    private val filmCatalog: FilmCatalog,
    private val programSets: ProgramSetRepository,
    private val abos: AboServices,
    private val blacklist: BlacklistServices,
    private val showMissingAboProgramSet: (JFrame?) -> Unit,
) {
    private val queue: LinkedList<DatenDownload> = LinkedList()
    private val buttonQueue: LinkedList<DatenDownload> = LinkedList()
    private val info: DownloadInfos = DownloadInfos(::unfinishedDownloads)
    private val starter: DownloadStartCoordinator = DownloadStartCoordinator(this, abos::historyController)

    fun refreshAboDownloads() {
        synchronized(queue) {
            // fehlerhafte und nicht gestartete löschen, wird nicht gemeldet ob was gefunden wurde
            val iterator = queue.iterator()
            while (iterator.hasNext()) {
                val download = iterator.next()
                if (download.isInterrupted) {
                    // guter Rat teuer was da besser wäre??
                    download.setGroesseFromFilm() // bei den Abgebrochenen wird die tatsächliche Dateigröße angezeigt
                    continue
                }
                if (!download.isFromAbo) {
                    continue
                }
                when (download.runtime.runState?.status) {
                    null -> iterator.remove() // noch nicht gestartet
                    StartStatus.ERROR -> DownloadLifecycleActions.reset(download) // fehlerhafte
                    else -> Unit
                }
            }

            queue.forEach { download ->
                DownloadLifecycleActions.clearDeferred(download)
            }
        }
    }

    fun reconnectFilms() {
        logger.info("Filme in Downloads eintragen")
        synchronized(queue) {
            val films = filmCatalog.allFilms
            queue.filter { download -> download.film == null }
                .forEach { download ->
                    download.film = films.getFilmByUrl_klein_hoch_hd(download.downloadUrl)
                    download.setGroesse("")
                }
        }
    }

    fun cleanupFinishedDownloads() {
        var found = false
        synchronized(queue) {
            val iterator = queue.iterator()
            while (iterator.hasNext()) {
                val download = iterator.next()
                when (download.runtime.runState?.status) {
                    StartStatus.FINISHED -> {
                        // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                        iterator.remove()
                        found = true
                    }

                    StartStatus.ERROR -> {
                        // fehlerhafte werden zurückgesetzt
                        DownloadLifecycleActions.reset(download)
                        found = true
                    }

                    else -> Unit
                }
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
        }
    }

    fun cleanupFinishedDownload(download: DatenDownload) {
        var found = false
        synchronized(queue) {
            when (download.runtime.runState?.status) {
                StartStatus.FINISHED -> {
                    // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                    queue.remove(download)
                    found = true
                }

                StartStatus.ERROR -> {
                    // fehlerhafte werden zurückgesetzt
                    DownloadLifecycleActions.reset(download)
                    found = true
                }

                else -> Unit
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
        }
    }

    fun cancelDownloads(downloads: Collection<DatenDownload>?) {
        var found = false
        if (downloads != null) {
            synchronized(queue) {
                for (download in downloads) {
                    if (queue.contains(download)) {
                        // nur dann ist er in der Liste
                        download.runtime.runState?.let { state ->
                            if (state.isBeforeFinished) {
                                state.requestStop()
                            }
                            if (state.isRunning) {
                                DownloadLifecycleActions.markInterrupted(download)
                            }
                        }
                        DownloadLifecycleActions.reset(download)
                        found = true
                    }
                }
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(StartEvent())
        }
    }

    fun deleteDownloads(downloads: Collection<DatenDownload>?) {
        var found = false
        if (downloads != null) {
            synchronized(queue) {
                for (download in downloads) {
                    val state = download.runtime.runState
                    if (state?.isBeforeFinished == true) {
                        state.requestStop()
                    }
                    if (queue.remove(download)) {
                        found = true
                    }
                }
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
        }
    }

    fun advanceDownloads(downloads: List<DatenDownload>) {
        synchronized(queue) {
            for (download in downloads) {
                queue.remove(download)
                queue.addFirst(download)
            }
        }

        MessageBus.messageBus.publishAsync(DownloadQueueRankChangedEvent())
    }

    fun reorderQueueToMatch(downloadsInOrder: List<DatenDownload>) {
        synchronized(queue) {
            for (download in downloadsInOrder) {
                if (queue.remove(download)) {
                    queue.add(download)
                }
            }
        }
    }

    fun moveDownloadsTo(insertionIndex: Int, downloads: List<DatenDownload>) {
        synchronized(queue) {
            for (download in downloads) {
                queue.remove(download)
            }
            queue.addAll(insertionIndex.coerceIn(0, queue.size), downloads)
        }

        MessageBus.messageBus.publishAsync(DownloadQueueRankChangedEvent())
    }

    fun cancelRunningButtonDownloadByFilmUrl(filmUrl: String): Boolean {
        synchronized(buttonQueue) {
            for (download in buttonQueue) {
                val state = download.runtime.runState
                if (download.filmUrl == filmUrl && state?.status == StartStatus.RUNNING) {
                    state.requestStop()
                    DownloadLifecycleActions.reset(download)
                    MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                    return true
                }
            }
        }

        return false
    }

    fun cleanupFinishedButtonDownloads(): Boolean {
        var found = false
        synchronized(buttonQueue) {
            val iterator = buttonQueue.iterator()
            while (iterator.hasNext()) {
                val download = iterator.next()
                if (download.runtime.runState?.isAtLeastFinished == true && download.quelle == DownloadSource.BUTTON) {
                    iterator.remove()
                    found = true
                }
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(ButtonStartEvent())
        }
        return found
    }

    fun setDialogOwner(owner: MainWindowHandle?) {
        starter.setDialogOwner(owner)
    }

    fun startStarter() {
        starter.start()
    }

    fun startWithProgram(pSet: DatenPset, film: DatenFilm, resolution: String) {
        starter.urlMitProgrammStarten(pSet, film, resolution)
    }

    fun delayNewStarts() {
        starter.delayNewStarts()
    }

    fun progressSnapshot(): DownloadProgressSnapshot =
        DownloadProgressSnapshot(
            bandwidthText = info.bandwidthStr,
            activeBytes = info.byteAktDownloads,
            totalBytes = info.byteAlleDownloads,
        )

    fun addDownload(download: DatenDownload) {
        synchronized(queue) {
            queue.add(download)
            renumber(queue)
        }
    }

    fun addButtonDownload(download: DatenDownload) {
        synchronized(buttonQueue) {
            buttonQueue.add(download)
            renumber(buttonQueue)
        }
    }

    fun addLoadedDownload(download: DatenDownload) {
        synchronized(queue) {
            queue.add(download)
        }
    }

    fun addLoadedDownloads(downloads: Collection<DatenDownload>) {
        synchronized(queue) {
            queue.addAll(downloads)
        }
    }

    fun findDownloadByFilmUrl(filmUrl: String): DatenDownload? = synchronized(queue) {
        queue.firstOrNull { download -> download.filmUrl == filmUrl }
    }

    fun findButtonDownloadByFilmUrl(filmUrl: String): DatenDownload? = synchronized(buttonQueue) {
        buttonQueue.firstOrNull { download -> download.filmUrl == filmUrl }
    }

    fun queuedDownloads(): List<DatenDownload> = synchronized(queue) {
        queue.toList()
    }

    fun buttonDownloads(): List<DatenDownload> = synchronized(buttonQueue) {
        buttonQueue.toList()
    }

    fun clearQueuedDownloads() {
        synchronized(queue) {
            queue.clear()
        }
    }

    fun renumberQueuedDownloads() {
        synchronized(queue) {
            renumber(queue)
        }
    }

    fun reloadTableModel(model: TModelDownload, filter: DownloadListFilter) {
        synchronized(queue) {
            DownloadTableModelUpdater.reload(model, queue, filter)
        }
    }

    fun updateTableModelProgress(model: TModelDownload) {
        synchronized(queue) {
            DownloadTableModelUpdater.updateProgress(model)
        }
    }

    fun nextStart(): DatenDownload? = synchronized(queue) {
        val maxNumDownloads = ApplicationConfiguration.getInstance().maxSimultaneousDownloads
        if (queue.isNotEmpty() && canStartMore(maxNumDownloads)) {
            return@synchronized queue.firstOrNull { download ->
                download.runtime.runState?.status == StartStatus.INITIALIZED
            }
        }

        null
    }

    fun restartDownload(): DatenDownload? = synchronized(queue) {
        if (!canStartMore(1)) {
            return@synchronized null
        }
        for (download in queue) {
            val state = download.runtime.runState ?: continue

            if (state.status == StartStatus.ERROR && state.countRestarted < Konstanten.MAX_DOWNLOAD_RESTARTS) {
                val restarted = state.countRestarted
                if (download.art == DownloadType.DIRECT) {
                    DownloadLifecycleActions.reset(download)
                    DownloadStartActions.start(download)
                    download.runtime.runState?.countRestarted = restarted + 1
                    return@synchronized download
                }
            }
        }

        null
    }

    fun requestStopForShutdown() {
        synchronized(queue) {
            for (download in queue) {
                download.runtime.runState?.requestStop()
            }
        }
    }

    fun unfinishedDownloads(): Long = synchronized(queue) {
        queue.count { download -> download.runNotFinished() }.toLong()
    }

    fun unfinishedDownloads(source: DownloadSource): List<DatenDownload> = synchronized(queue) {
        queue.filter { download ->
            download.runtime.runState?.isBeforeFinished == true &&
                (source == DownloadSource.ALL || download.quelle == source)
        }
    }

    fun automaticAboDownloadsToStart(): List<DatenDownload> = synchronized(queue) {
        queue.filter { download ->
            download.isFromAbo &&
                !download.isAutomaticStartBlockedByAbo &&
                download.runtime.runState == null
        }
    }

    fun startInfo(): DownloadStartInfo = synchronized(queue) {
        val info = DownloadStartInfo()
        info.total_num_download_list_entries = queue.size

        for (download in queue) {
            if (!download.isDeferred) {
                info.total_starts++
            }
            if (download.isFromAbo) {
                info.num_abos++
            } else {
                info.num_downloads++
            }
            val state = download.runtime.runState
            if (
                state != null &&
                (download.quelle == DownloadSource.ABO || download.quelle == DownloadSource.DOWNLOAD)
            ) {
                when (state.status) {
                    StartStatus.INITIALIZED -> info.initialized++
                    StartStatus.RUNNING -> info.running++
                    StartStatus.FINISHED -> info.finished++
                    StartStatus.ERROR -> info.error++
                }
            }
        }

        info
    }

    fun searchAboDownloads(parent: JFrame?): List<DatenDownload> = synchronized(queue) {
        // in der Filmliste nach passenden Filmen suchen und
        // in die Liste der Downloads eintragen
        val downloadUrls = HashSet<String>()
        val addedDownloads = mutableListOf<DatenDownload>()
        // mit den bereits enthaltenen URL füllen
        queue.forEach { download -> downloadUrls.add(download.downloadUrl) }

        // prüfen ob in "alle Filme" oder nur "nach Blacklist" gesucht werden soll
        val checkWithBlackList = ApplicationConfiguration.getInstance().blacklistApplyToAbo
        val defaultPset = programSets.list.getPsetAbo("")
        val today = LocalDate.now(DateUtil.MV_DEFAULT_TIMEZONE)

        val aboHistoryController = abos.historyController
        val listeFilme = filmCatalog.allFilms
        val blacklistFilter: Predicate<DatenFilm> = if (checkWithBlackList) {
            blacklist.createDownloadsPredicate()
        } else {
            Predicate { true }
        }

        for (film in listeFilme) {
            val abo = abos.findAboForFilm(film, true) ?: continue
            if (!abo.isActive) {
                continue
            }
            if (checkWithBlackList && !blacklistFilter.test(film)) {
                // Blacklist auch bei Abos anwenden
                continue
            }
            if (aboHistoryController.urlExists(film.urlNormalQuality)) {
                // ist schon mal geladen worden
                continue
            }

            val pset = if (abo.psetName.isEmpty()) defaultPset else programSets.list.getPsetAbo(abo.psetName)
            if (pset != null) {
                // mit der tatsächlichen URL prüfen, ob die URL schon in der Downloadliste ist
                val downloadUrl = film.getUrlFuerAufloesung(pset.aufloesung)
                if (!downloadUrls.add(downloadUrl)) {
                    continue
                }

                // diesen Film in die Downloadliste eintragen
                abo.downloadDate = today
                if (abo.psetName != pset.name) {
                    // nur den Namen anpassen, falls geändert
                    abo.psetName = pset.name
                }

                // dann in die Liste schreiben
                val download = DatenDownload(pset, film, DownloadSource.ABO, abo, "", "", "")
                queue.add(download)
                addedDownloads.add(download)
            } else {
                if (parent == null || CommandLineOptions.isDownloadAndQuit()) {
                    throw IllegalStateException("Kein Programmset für Abo \"${abo.name}\" konfiguriert.")
                }
                showMissingAboProgramSet(parent)
                break
            }
        }

        if (addedDownloads.isNotEmpty()) {
            renumber(queue)
        }
        addedDownloads
    }

    private fun canStartMore(max: Int): Boolean {
        var count = 0
        for (download in queue) {
            if (download.runtime.runState?.isRunning == true) {
                ++count
                if (count >= max) {
                    return false
                }
            }
        }
        return true
    }

    private fun renumber(downloads: Iterable<DatenDownload>) {
        var index = 1
        for (download in downloads) {
            download.nr = index++
        }
    }

    fun shutdown() {
        starter.shutdown()
    }

    companion object {
        private val logger = LogManager.getLogger(DownloadServices::class.java)
    }
}
