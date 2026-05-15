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
import mediathek.config.Daten
import mediathek.config.StandardLocations
import mediathek.controller.history.SeenHistoryController
import mediathek.controller.starter.Start
import mediathek.daten.DatenDownload
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.filmlisten.reader.FilmListReader
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.BandwidthFormatter
import org.apache.logging.log4j.LogManager
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.math.roundToInt
import kotlin.time.Duration.Companion.seconds

object DownloadAndQuitRunner {
    const val INTERRUPTED_EXIT_CODE = 130

    private val logger = LogManager.getLogger()
    private val shutdownRequested = AtomicBoolean(false)

    @Volatile
    private var activeDownloads: List<DatenDownload> = emptyList()

    suspend fun run(): Int {
        val daten = Daten.getInstance()

        logger.info("CLI download mode started.")
        try {
            return runInternal(daten)
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

    private suspend fun runInternal(daten: Daten): Int {
        if (!updateFilmlistWithProgress(daten)) {
            return 1
        }

        if (shutdownRequested.get()) {
            logger.info("CLI shutdown requested before abo download search.")
            persistState(daten)
            return INTERRUPTED_EXIT_CODE
        }

        logger.info("Loading downloads from abos...")
        prepareAboSearch(daten)
        daten.listeDownloads.abosAuffrischen()
        daten.listeDownloads.abosSuchen(null)

        val downloadsToStart = collectDownloadsToStart(daten)
        activeDownloads = downloadsToStart
        if (shutdownRequested.get()) {
            logger.info("CLI shutdown requested before downloads were started.")
            markDownloadsInterrupted(downloadsToStart)
            persistState(daten)
            return INTERRUPTED_EXIT_CODE
        }

        if (downloadsToStart.isEmpty()) {
            logger.info("No abo downloads to start.")
            persistState(daten)
            return 0
        }

        logger.info("Starting {} abo download(s)...", downloadsToStart.size)
        DatenDownload.startenDownloads(downloadsToStart)
        if (shutdownRequested.get()) {
            stopDownloads(downloadsToStart)
        }
        val failedDownloads = monitorDownloads(downloadsToStart)

        persistState(daten)

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

    private suspend fun updateFilmlistWithProgress(daten: Daten): Boolean = withContext(Dispatchers.IO) {
        loadLocalFilmlist(daten)

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
                daten.filmeLaden.removeAdListener(this)
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

        daten.filmeLaden.addAdListener(listener)
        val loadStarted = daten.filmeLaden.loadFilmlist("", false)
        if (!loadStarted) {
            daten.filmeLaden.removeAdListener(listener)
            logger.info("Filmlist update skipped because another filmlist load is already running.")
            return@withContext true
        }
        completion.get()
    }

    private suspend fun prepareAboSearch(daten: Daten) = withContext(Dispatchers.Default) {
        logger.info("Preparing abo matches for {} film(s)...", daten.listeFilme.size)
        daten.listeAbo.setAboFuerFilm(daten.listeFilme, false)
    }

    private fun loadLocalFilmlist(daten: Daten) {
        if (daten.listeFilme.isNotEmpty()) {
            return
        }

        logger.info("Reading local filmlist cache...")
        FilmListReader().use { reader ->
            val numDays = ApplicationConfiguration.getConfiguration()
                .getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0)
            reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), daten.listeFilme, numDays)
        }
    }

    private fun collectDownloadsToStart(daten: Daten): ArrayList<DatenDownload> {
        val downloadsToStart = ArrayList<DatenDownload>()
        for (download in daten.listeDownloads) {
            if (!download.isFromAbo() || download.isAutomaticStartBlockedByAbo()) {
                continue
            }
            if (download.start == null) {
                downloadsToStart.add(download)
            }
        }
        return downloadsToStart
    }

    private suspend fun monitorDownloads(downloads: List<DatenDownload>): Int {
        var lastSummary = ""
        while (true) {
            val trackedDownloads = downloads.filter { it.start != null }
            val waiting = trackedDownloads.count { it.start?.status == Start.STATUS_INIT }
            val runningDownloads = trackedDownloads.filter { it.start?.status == Start.STATUS_RUN }
            val finished = trackedDownloads.count { it.start?.status == Start.STATUS_FERTIG }
            val errors = trackedDownloads.count { it.start?.status == Start.STATUS_ERR }
            val unfinished = waiting + runningDownloads.size
            val averageProgress = if (runningDownloads.isEmpty()) {
                0
            } else {
                runningDownloads
                    .mapNotNull { it.start?.percent }
                    .map { it.coerceAtLeast(0) }
                    .average()
                    .div(10.0)
                    .roundToInt()
            }
            val bandwidth = runningDownloads.sumOf { it.start?.bandbreite?.coerceAtLeast(0) ?: 0L }

            val summary = buildString {
                append("Downloads: ")
                append(finished)
                append('/')
                append(trackedDownloads.size)
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

        Daten.getInstance().starterClass.delayNewStarts()
        for (download in downloads) {
            val start = download.start
            if (start == null) {
                download.interrupt()
                continue
            }

            if (start.status < Start.STATUS_FERTIG) {
                start.stoppen = true
                download.interrupt()
                if (start.status == Start.STATUS_INIT) {
                    start.status = Start.STATUS_ERR
                }
            }
        }
    }

    private fun markDownloadsInterrupted(downloads: List<DatenDownload>) {
        downloads.forEach(DatenDownload::interrupt)
    }

    private fun persistState(daten: Daten) {
        logger.info("Persisting download and configuration state...")
        daten.listeDownloads.listePutzen()
        SeenHistoryController().use { history ->
            history.performMaintenance()
        }
        daten.listeBookmarkList.saveToFile()
        daten.allesSpeichern()
        ApplicationConfiguration.getInstance().writeConfiguration()
    }
}
