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

package mediathek.cli

import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.delay
import kotlinx.coroutines.withContext
import mediathek.config.DatenConfigurationPersistence
import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.SeenHistoryController
import mediathek.controller.starter.DownloadLifecycleActions
import mediathek.controller.starter.DownloadServices
import mediathek.controller.starter.DownloadStartActions
import mediathek.controller.starter.StartStatus
import mediathek.daten.DatenDownload
import mediathek.daten.abo.AboServices
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.filmlisten.FilmCatalog
import mediathek.filmlisten.FilmeLaden
import mediathek.filmlisten.reader.FilmListReader
import mediathek.gui.bookmark.BookmarkServices
import mediathek.tool.BandwidthFormatter
import mediathek.tool.FileSize
import org.apache.logging.log4j.LogManager
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.math.roundToInt
import kotlin.time.Duration.Companion.seconds

class DownloadAndQuitRunner(
    private val downloads: DownloadServices,
    private val filmCatalog: FilmCatalog,
    private val filmListLoader: FilmeLaden,
    private val abos: AboServices,
    private val bookmarks: BookmarkServices,
    private val configurationPersistence: DatenConfigurationPersistence,
) {
    private val logger = LogManager.getLogger()
    private val shutdownRequested = AtomicBoolean(false)

    @Volatile
    private var activeDownloads: List<DatenDownload> = emptyList()

    suspend fun run(): Int {
        logger.info("CLI download mode started.")
        try {
            return runInternal()
        } catch (ex: Exception) {
            logger.error("CLI download mode failed.", ex)
            return 1
        }
    }

    fun requestShutdown(): Boolean {
        if (!shutdownRequested.compareAndSet(false, true)) {
            logger.info("CLI shutdown is already in progress.")
            return false
        }

        logger.info("Ctrl-C received. Stopping CLI download mode gracefully...")
        stopDownloads(activeDownloads)
        return true
    }

    private suspend fun runInternal(): Int {
        if (!updateFilmlistWithProgress()) {
            return 1
        }

        if (shutdownRequested.get()) {
            logger.info("CLI shutdown requested before abo download search.")
            persistState()
            return INTERRUPTED_EXIT_CODE
        }

        logger.info("Loading downloads from abos...")
        prepareAboSearch()
        downloads.refreshAboDownloads()
        val addedDownloads = downloads.searchAboDownloads(null)
        updateAboDownloadSizes(addedDownloads)

        val downloadsToStart = downloads.automaticAboDownloadsToStart()
        activeDownloads = downloadsToStart
        if (shutdownRequested.get()) {
            logger.info("CLI shutdown requested before downloads were started.")
            markDownloadsInterrupted(downloadsToStart)
            persistState()
            return INTERRUPTED_EXIT_CODE
        }

        if (downloadsToStart.isEmpty()) {
            logger.info("No abo downloads to start.")
            persistState()
            return 0
        }

        logger.info("Starting {} abo download(s)...", downloadsToStart.size)
        DownloadStartActions.startAll(downloadsToStart)
        downloads.startStarter()
        if (shutdownRequested.get()) {
            stopDownloads(downloadsToStart)
        }
        val failedDownloads = monitorDownloads(downloadsToStart)

        persistState()

        if (shutdownRequested.get()) {
            logger.info("CLI shutdown completed after stopping downloads.")
            return INTERRUPTED_EXIT_CODE
        }

        if (failedDownloads > 0) {
            logger.error("{} download(s) finished with errors.", failedDownloads)
            return 2
        }

        logger.info("All downloads finished successfully.")
        return 0
    }

    private suspend fun updateFilmlistWithProgress(): Boolean = withContext(Dispatchers.IO) {
        loadLocalFilmlist()

        val completion = CompletableFuture<Boolean>()
        val listener = object : ListenerFilmeLaden() {
            private var lastProgress = -1

            override fun start(event: ListenerFilmeLadenEvent) {
                logger.info("Updating filmlist...")
                emitFilmlistProgress(event)
            }

            override fun progress(event: ListenerFilmeLadenEvent) {
                emitFilmlistProgress(event)
            }

            override fun fertig(event: ListenerFilmeLadenEvent) {
                filmListLoader.removeFilmLoadListener(this)
                if (event.fehler) {
                    logger.error("Filmlist update failed.")
                } else {
                    logger.info("Filmlist update finished.")
                }
                completion.complete(!event.fehler)
            }

            private fun emitFilmlistProgress(event: ListenerFilmeLadenEvent) {
                if (event.max <= 0) {
                    return
                }
                val percentage = ((event.progress.toDouble() / event.max.toDouble()) * 100.0).roundToInt()
                if (percentage != lastProgress) {
                    lastProgress = percentage
                    val details = event.text.takeIf { it.isNotBlank() }?.let { " $it" }.orEmpty()
                    logger.info("Filmlist: {}%{}", percentage, details)
                }
            }
        }

        filmListLoader.addFilmLoadListener(listener)
        val loadStarted = filmListLoader.loadFilmlist("", false)
        if (!loadStarted) {
            filmListLoader.removeFilmLoadListener(listener)
            logger.info("Filmlist update skipped because another filmlist load is already running.")
            return@withContext true
        }
        completion.get()
    }

    private suspend fun prepareAboSearch() = withContext(Dispatchers.Default) {
        logger.info("Preparing abo matches for {} film(s)...", filmCatalog.allFilms.size)
        abos.assignAbosToFilms(removeMissingAbos = false)
    }

    private suspend fun updateAboDownloadSizes(downloads: List<DatenDownload>) = withContext(Dispatchers.IO) {
        val lookupResults = mutableMapOf<DownloadSizeLookupKey, FileSize.LookupResult>()
        downloads.forEach { download ->
            runCatching {
                val lookupKey = DownloadSizeLookupKey(download.downloadUrl, download.selectedResolution.name)
                val cachedResult = lookupResults[lookupKey]
                if (cachedResult != null) {
                    download.applyLiveSizeLookupResult(cachedResult)
                } else {
                    download.queryLiveSize(forceFetch = false, probeHlsSegments = false)?.let { lookupResult ->
                        lookupResults[lookupKey] = lookupResult
                    }
                }
            }.onFailure { error ->
                logger.debug("Could not update live size for abo download {}", download.title, error)
            }
        }
    }

    private fun loadLocalFilmlist() {
        if (filmCatalog.allFilms.isNotEmpty()) {
            return
        }

        logger.info("Reading local filmlist cache...")
        FilmListReader().use { reader ->
            val numDays = ApplicationConfiguration.getInstance().filmListLoadNumDays
            reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), filmCatalog.allFilms, numDays)
        }
    }

    private suspend fun monitorDownloads(downloads: List<DatenDownload>): Int {
        var lastSummary = ""
        while (true) {
            val trackedDownloads = downloads.filter { it.runtime.runState != null }
            val interrupted = downloads.count { it.isInterrupted }
            val interruptedWithoutRunState = downloads.count { it.runtime.runState == null && it.isInterrupted }
            val waiting = trackedDownloads.count { it.runtime.runState?.status == StartStatus.INITIALIZED }
            val runningDownloads = trackedDownloads.filter { it.runtime.runState?.status == StartStatus.RUNNING }
            val finished = trackedDownloads.count { it.runtime.runState?.status == StartStatus.FINISHED }
            val errors = trackedDownloads.count { it.runtime.runState?.status == StartStatus.ERROR && !it.isInterrupted }
            val unfinished = waiting + runningDownloads.size
            val averageProgress = if (runningDownloads.isEmpty()) {
                0
            } else {
                runningDownloads
                    .mapNotNull { it.runtime.runState?.percent }
                    .map { it.coerceAtLeast(0) }
                    .average()
                    .div(10.0)
                    .roundToInt()
            }
            val bandwidth = runningDownloads.sumOf { it.runtime.runState?.bandbreite?.coerceAtLeast(0) ?: 0L }

            val summary = buildString {
                append("Downloads: ")
                append(finished)
                append('/')
                append(trackedDownloads.size + interruptedWithoutRunState)
                append(" finished, ")
                append(waiting)
                append(" waiting, ")
                append(runningDownloads.size)
                append(" running")
                if (runningDownloads.isNotEmpty()) {
                    append(", ")
                    append(averageProgress)
                    append("% avg, ")
                    append(BandwidthFormatter.format(bandwidth))
                }
                if (errors > 0) {
                    append(", ")
                    append(errors)
                    append(" error")
                    if (errors > 1) {
                        append('s')
                    }
                }
                if (interrupted > 0) {
                    append(", ")
                    append(interrupted)
                    append(" interrupted")
                }
            }

            if (summary != lastSummary) {
                logger.info(summary)
                lastSummary = summary
            }

            if (unfinished == 0) {
                return errors
            }

            delay(1.seconds)
        }
    }

    private fun stopDownloads(downloads: List<DatenDownload>) {
        if (downloads.isEmpty()) {
            return
        }

        this.downloads.delayNewStarts()
        for (download in downloads) {
            val start = download.runtime.runState
            if (start == null) {
                DownloadLifecycleActions.markInterrupted(download)
                continue
            }

            if (start.status < StartStatus.FINISHED) {
                start.requestStop()
                DownloadLifecycleActions.markInterrupted(download)
                if (start.status == StartStatus.INITIALIZED) {
                    DownloadLifecycleActions.reset(download)
                }
            }
        }
    }

    private fun markDownloadsInterrupted(downloads: List<DatenDownload>) {
        downloads.forEach(DownloadLifecycleActions::markInterrupted)
    }

    private fun persistState() {
        logger.info("Persisting download and configuration state...")
        downloads.cleanupFinishedDownloads()
        SeenHistoryController().use { history ->
            history.performMaintenance()
        }
        bookmarks.saveToFile()
        configurationPersistence.saveAll()
        ApplicationConfiguration.getInstance().writeConfiguration()
    }

    private data class DownloadSizeLookupKey(
        val url: String,
        val quality: String,
    )

    companion object {
        const val INTERRUPTED_EXIT_CODE = 130
    }
}
