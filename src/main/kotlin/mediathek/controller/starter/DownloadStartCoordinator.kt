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

package mediathek.controller.starter

import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.AboHistoryController
import mediathek.controller.history.FilmSeenHistoryController
import mediathek.daten.*
import mediathek.mainwindow.MainWindowHandle
import mediathek.tool.CdnDetector
import org.apache.logging.log4j.LogManager
import java.time.LocalDateTime
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.ScheduledFuture
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import kotlin.time.Duration.Companion.seconds

private val logger = LogManager.getLogger(DownloadStartCoordinator::class.java)
private const val DOWNLOAD_DELAY_SECONDS = 2L
private const val NEW_START_PAUSE_SECONDS = 5L

class DownloadStartCoordinator(
    private val downloads: DownloadServices,
    private val aboHistoryControllerProvider: () -> AboHistoryController,
) {
    private val starterScheduler: ScheduledExecutorService = Executors.newSingleThreadScheduledExecutor { runnable ->
        Thread.ofPlatform().name("StarterScheduler").daemon(true).unstarted(runnable)
    }
    @Volatile
    private var starterFuture: ScheduledFuture<*>? = null
    private val pause = AtomicBoolean(false)
    @Volatile
    private var pauseUntilEpochMillis: Long = 0
    @Volatile
    private var dialogOwner: MainWindowHandle? = null

    @Synchronized
    fun start() {
        if (starterFuture != null) {
            return
        }
        starterFuture = starterScheduler.scheduleWithFixedDelay(
            ::processStarterTick,
            0,
            DOWNLOAD_DELAY_SECONDS,
            TimeUnit.SECONDS,
        )
    }

    fun setDialogOwner(owner: MainWindowHandle?) {
        dialogOwner = owner
    }

    @Synchronized
    fun urlMitProgrammStarten(pSet: DatenPset, film: DatenFilm, aufloesung: String) {
        // url mit dem Programm mit der Nr. starten (Button oder TabDownload "rechte Maustaste")
        // Quelle "Button" ist immer ein vom User gestarteter Film, also Quelle_Button!!!!!!!!!!!
        val url = film.urlNormalQuality
        if (url.isNotEmpty()) {
            val download = DatenDownload(pSet, film, DownloadSource.BUTTON, null, "", "", aufloesung)
            download.runtime.startRun()
            launchDownloadThread(download)
            // gestartete Filme (originalURL des Films) auch in die History eintragen
            FilmSeenHistoryController().use { historyController ->
                historyController.markSeen(film)
            }

            // falls gemerkt, Film in Merkliste als abgespielt kennzeichnen
            if (film.isBookmarked) {
                film.bookmark?.seen = true
            }
            // und jetzt noch in die Downloadliste damit die Farbe im Tab Filme passt
            downloads.addButtonDownload(download)
        }
    }

    fun delayNewStarts() {
        pause.set(true)
    }

    fun shutdown() {
        starterFuture?.cancel(true)
        starterFuture = null
        starterScheduler.shutdownNow()
    }

    private fun reStartmeldung(datenDownload: DatenDownload) {
        val start = checkNotNull(datenDownload.runtime.runState)
        val text = mutableListOf<String>()
        text.add("Fehlerhaften Download neu starten - Restart (Summe Starts: ${start.countRestarted})")
        text.add("Ziel: ${datenDownload.targetPathFileName}")
        text.add("URL: ${datenDownload.downloadUrl}")
        logger.info(text)
    }

    private fun processStarterTick() {
        try {
            if (isPauseActive()) {
                return
            }

            val datenDownload = getNextStart()
            if (datenDownload != null) {
                launchDownloadThread(datenDownload)
                return
            }

            downloads.cleanupFinishedButtonDownloads() // Button Starts aus der Liste löschen
        } catch (ex: Exception) {
            logger.error("Fehler im Starter-Scheduler:", ex)
        }
    }

    private fun isPauseActive(): Boolean {
        if (pause.getAndSet(false)) {
            pauseUntilEpochMillis = System.currentTimeMillis() + NEW_START_PAUSE_SECONDS.seconds.inWholeMilliseconds
        }
        return System.currentTimeMillis() < pauseUntilEpochMillis
    }

    @Synchronized
    private fun getNextStart(): DatenDownload? {
        // get: erstes passendes Element der Liste zurückgeben oder null
        // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
        var download = downloads.nextStart()
        if (download == null) {
            // dann versuchen einen Fehlerhaften nochmal zu starten
            download = downloads.restartDownload()
            if (download != null) {
                reStartmeldung(download)
            }
        }
        return download
    }

    private fun selectDirectDownload(datenDownload: DatenDownload): Thread {
        val useCdnAwareDirectDownload = ApplicationConfiguration.getInstance().useCdnAwareDirectDownload
        val result = CdnDetector.detect(datenDownload.downloadUrl)
        return if (useCdnAwareDirectDownload && CdnDetector.isCdn(result)) {
            logger.trace("CDN detected: {}", result)
            CdnAwareDirectDownloadThread(aboHistoryControllerProvider, datenDownload, ::dialogOwnerFrame)
        } else {
            if (!useCdnAwareDirectDownload) {
                logger.info("CDN detection is disabled")
            } else {
                logger.trace("Not a CDN detected: {}", result)
            }
            DirectHttpDownload(aboHistoryControllerProvider, datenDownload, ::dialogOwnerFrame)
        }
    }

    private fun dialogOwnerFrame() = dialogOwner?.ownerFrame()

    /**
     * This will start the download process.
     *
     * @param datenDownload The [DatenDownload] info object for download.
     */
    private fun launchDownloadThread(datenDownload: DatenDownload) {
        checkNotNull(datenDownload.runtime.runState).startTime = LocalDateTime.now()
        DownloadProgressEventPublisher.publishThrottled()

        val downloadThread = when (datenDownload.art) {
            DownloadType.PROGRAM -> ExternalProgramDownload(aboHistoryControllerProvider, datenDownload, ::dialogOwnerFrame)
            DownloadType.DIRECT -> selectDirectDownload(datenDownload)
        }
        downloadThread.start()
    }
}
