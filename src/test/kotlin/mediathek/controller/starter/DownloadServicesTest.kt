package mediathek.controller.starter

import mediathek.config.Daten
import mediathek.controller.history.AboHistoryController
import mediathek.controller.DownloadColumn
import mediathek.daten.DatenPset
import mediathek.daten.ProgramSetRepository
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.daten.DownloadSource
import mediathek.daten.DownloadType
import mediathek.daten.abo.AboServices
import mediathek.daten.abo.DatenAbo
import mediathek.daten.blacklist.BlacklistServices
import mediathek.filmlisten.FilmCatalog
import mediathek.tool.ReplacementRules
import mediathek.tool.models.TModelDownload
import mediathek.tool.notification.NotificationPublisher
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.mockito.Mockito
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import javax.swing.JFrame

internal class DownloadServicesTest {
    private lateinit var daten: Daten

    @BeforeEach
    fun setUp() {
        daten = Daten()
    }

    @AfterEach
    fun tearDown() {
        daten.downloads.shutdown()
    }

    @Test
    fun cancelsRunningButtonDownloadByFilmUrl() {
        val download = buttonDownload(
            filmUrl = "https://example.invalid/film",
            downloadUrl = "https://example.invalid/download.mp4",
            status = StartStatus.RUNNING,
        )
        addButtonDownload(download)

        assertTrue(daten.downloads.cancelRunningButtonDownloadByFilmUrl("https://example.invalid/film"))

        assertNull(download.runtime.runState)
    }

    @Test
    fun ignoresButtonDownloadsThatAreNotRunning() {
        val download = buttonDownload(
            filmUrl = "https://example.invalid/film",
            downloadUrl = "https://example.invalid/download.mp4",
            status = StartStatus.INITIALIZED,
        )
        val runState = download.runtime.runState
        addButtonDownload(download)

        assertFalse(daten.downloads.cancelRunningButtonDownloadByFilmUrl("https://example.invalid/film"))

        assertSame(runState, download.runtime.runState)
    }

    @Test
    fun removesFinishedButtonDownloads() {
        val finishedButtonDownload = buttonDownload(
            filmUrl = "https://example.invalid/finished",
            downloadUrl = "https://example.invalid/finished.mp4",
            status = StartStatus.FINISHED,
        )
        val runningButtonDownload = buttonDownload(
            filmUrl = "https://example.invalid/running",
            downloadUrl = "https://example.invalid/running.mp4",
            status = StartStatus.RUNNING,
        )
        val finishedManualDownload = download(DownloadRunState().apply { status = StartStatus.FINISHED })
        addButtonDownload(finishedButtonDownload)
        addButtonDownload(runningButtonDownload)
        addButtonDownload(finishedManualDownload)

        assertTrue(daten.downloads.cleanupFinishedButtonDownloads())

        assertEquals(listOf(runningButtonDownload, finishedManualDownload), daten.downloads.buttonDownloads())
    }

    @Test
    fun reportsNoFinishedButtonDownloads() {
        val runningButtonDownload = buttonDownload(
            filmUrl = "https://example.invalid/running",
            downloadUrl = "https://example.invalid/running.mp4",
            status = StartStatus.RUNNING,
        )
        addButtonDownload(runningButtonDownload)

        assertFalse(daten.downloads.cleanupFinishedButtonDownloads())

        assertEquals(listOf(runningButtonDownload), daten.downloads.buttonDownloads())
    }

    @Test
    fun requestsStopForQueuedDownloadsDuringShutdown() {
        val runState = DownloadRunState().apply {
            status = StartStatus.RUNNING
        }
        val download = download(runState)
        addQueuedDownload(download)

        daten.downloads.requestStopForShutdown()

        assertTrue(runState.stoppen)
    }

    @Test
    fun addsAndRenumbersDownloads() {
        val firstDownload = download(DownloadRunState().apply { status = StartStatus.INITIALIZED }).apply {
            filmUrl = "https://example.invalid/first"
        }
        val secondDownload = download(DownloadRunState().apply { status = StartStatus.INITIALIZED }).apply {
            filmUrl = "https://example.invalid/second"
        }

        daten.downloads.addDownload(firstDownload)
        daten.downloads.addDownload(secondDownload)

        assertEquals(listOf(firstDownload, secondDownload), daten.downloads.queuedDownloads())
        assertEquals(1, firstDownload.nr)
        assertEquals(2, secondDownload.nr)
        assertSame(secondDownload, daten.downloads.findDownloadByFilmUrl("https://example.invalid/second"))
    }

    @Test
    fun addsAndRenumbersButtonDownloads() {
        val firstDownload = buttonDownload(
            filmUrl = "https://example.invalid/first",
            downloadUrl = "https://example.invalid/first.mp4",
            status = StartStatus.RUNNING,
        )
        val secondDownload = buttonDownload(
            filmUrl = "https://example.invalid/second",
            downloadUrl = "https://example.invalid/second.mp4",
            status = StartStatus.RUNNING,
        )

        daten.downloads.addButtonDownload(firstDownload)
        daten.downloads.addButtonDownload(secondDownload)

        assertEquals(listOf(firstDownload, secondDownload), daten.downloads.buttonDownloads())
        assertEquals(1, firstDownload.nr)
        assertEquals(2, secondDownload.nr)
        assertSame(firstDownload, daten.downloads.findButtonDownloadByFilmUrl("https://example.invalid/first"))
    }

    @Test
    fun addsLoadedDownloadsWithoutRenumberingUntilRequested() {
        val firstDownload = namedDownload("first").apply {
            nr = 42
        }
        val secondDownload = namedDownload("second").apply {
            nr = 99
        }

        daten.downloads.addLoadedDownloads(listOf(firstDownload, secondDownload))

        assertEquals(listOf(firstDownload, secondDownload), daten.downloads.queuedDownloads())
        assertEquals(42, firstDownload.nr)
        assertEquals(99, secondDownload.nr)

        daten.downloads.renumberQueuedDownloads()

        assertEquals(1, firstDownload.nr)
        assertEquals(2, secondDownload.nr)
    }

    @Test
    fun returnsQueuedDownloadSnapshot() {
        val download = namedDownload("snapshot")
        addQueuedDownload(download)

        val snapshot = daten.downloads.queuedDownloads()
        daten.downloads.clearQueuedDownloads()

        assertEquals(listOf(download), snapshot)
    }

    @Test
    fun clearsQueuedDownloads() {
        addQueuedDownload(namedDownload("queued"))

        daten.downloads.clearQueuedDownloads()

        assertTrue(daten.downloads.queuedDownloads().isEmpty())
    }

    @Test
    fun reordersQueueToMatchDownloadOrder() {
        val firstDownload = namedDownload("first")
        val secondDownload = namedDownload("second")
        val thirdDownload = namedDownload("third")
        addQueuedDownload(firstDownload)
        addQueuedDownload(secondDownload)
        addQueuedDownload(thirdDownload)

        daten.downloads.reorderQueueToMatch(listOf(thirdDownload, firstDownload, secondDownload))

        assertEquals(listOf(thirdDownload, firstDownload, secondDownload), daten.downloads.queuedDownloads())
    }

    @Test
    fun movesDownloadsToQueueIndex() {
        val firstDownload = namedDownload("first")
        val secondDownload = namedDownload("second")
        val thirdDownload = namedDownload("third")
        addQueuedDownload(firstDownload)
        addQueuedDownload(secondDownload)
        addQueuedDownload(thirdDownload)

        daten.downloads.moveDownloadsTo(0, listOf(thirdDownload))

        assertEquals(listOf(thirdDownload, firstDownload, secondDownload), daten.downloads.queuedDownloads())
    }

    @Test
    fun reloadsDownloadTableModel() {
        val model = TModelDownload()
        val download = download(DownloadRunState().apply { status = StartStatus.INITIALIZED })
        addQueuedDownload(download)

        daten.downloads.reloadTableModel(model, allDownloadsFilter())

        assertEquals(1, model.rowCount)
        assertSame(download, model.getValueAt(0, DownloadColumn.REF.index))
    }

    @Test
    fun returnsNextInitializedDownload() {
        val finishedDownload = download(DownloadRunState().apply { status = StartStatus.FINISHED })
        val initializedDownload = download(DownloadRunState().apply { status = StartStatus.INITIALIZED })
        val laterInitializedDownload = download(DownloadRunState().apply { status = StartStatus.INITIALIZED })
        addQueuedDownload(finishedDownload)
        addQueuedDownload(initializedDownload)
        addQueuedDownload(laterInitializedDownload)

        assertSame(initializedDownload, daten.downloads.nextStart())
    }

    @Test
    fun restartsErroredDirectDownload() {
        val erroredDownload = download(DownloadRunState().apply {
            status = StartStatus.ERROR
            countRestarted = 1
        })
        addQueuedDownload(erroredDownload)

        assertSame(erroredDownload, daten.downloads.restartDownload())

        val restartedState = erroredDownload.runtime.runState
        assertEquals(StartStatus.INITIALIZED, restartedState?.status)
        assertEquals(2, restartedState?.countRestarted)
    }

    @Test
    fun ignoresErroredProgramDownloadForRestart() {
        val programDownload = download(DownloadRunState().apply { status = StartStatus.ERROR }).apply {
            art = DownloadType.PROGRAM
        }
        addQueuedDownload(programDownload)

        assertNull(daten.downloads.restartDownload())
        assertEquals(StartStatus.ERROR, programDownload.runtime.runState?.status)
    }

    @Test
    fun countsOnlyUnfinishedDownloads() {
        addQueuedDownload(download(DownloadRunState().apply { status = StartStatus.RUNNING }))
        addQueuedDownload(download(DownloadRunState().apply { status = StartStatus.FINISHED }))

        assertEquals(1L, daten.downloads.unfinishedDownloads())
    }

    @Test
    fun returnsUnfinishedDownloadsForSource() {
        val manualDownload = download(DownloadRunState().apply { status = StartStatus.RUNNING })
        val aboDownload = download(DownloadRunState().apply { status = StartStatus.INITIALIZED }).apply {
            quelle = DownloadSource.ABO
        }
        val finishedManualDownload = download(DownloadRunState().apply { status = StartStatus.FINISHED })
        addQueuedDownload(manualDownload)
        addQueuedDownload(aboDownload)
        addQueuedDownload(finishedManualDownload)

        assertEquals(listOf(manualDownload), daten.downloads.unfinishedDownloads(DownloadSource.DOWNLOAD))
        assertEquals(listOf(manualDownload, aboDownload), daten.downloads.unfinishedDownloads(DownloadSource.ALL))
    }

    @Test
    fun returnsAutomaticAboDownloadsToStart() {
        val startableAbo = aboDownload(null)
        val blockedAbo = aboDownload(null).apply {
            abo = DatenAbo().apply {
                isDoNotStartAutomatically = true
            }
        }
        val alreadyStartedAbo = aboDownload(DownloadRunState().apply { status = StartStatus.INITIALIZED })
        val manualDownload = download(null)
        addQueuedDownload(startableAbo)
        addQueuedDownload(blockedAbo)
        addQueuedDownload(alreadyStartedAbo)
        addQueuedDownload(manualDownload)

        assertEquals(listOf(startableAbo), daten.downloads.automaticAboDownloadsToStart())
    }

    @Test
    fun buildsDownloadStartInfo() {
        val initialized = download(DownloadRunState().apply { status = StartStatus.INITIALIZED })
        val runningAbo = download(DownloadRunState().apply { status = StartStatus.RUNNING }).apply {
            quelle = DownloadSource.ABO
            aboName = "Abo"
        }
        val finishedDeferred = download(DownloadRunState().apply { status = StartStatus.FINISHED }).apply {
            isDeferred = true
        }
        val buttonDownload = download(DownloadRunState().apply { status = StartStatus.RUNNING }).apply {
            quelle = DownloadSource.BUTTON
        }
        addQueuedDownload(initialized)
        addQueuedDownload(runningAbo)
        addQueuedDownload(finishedDeferred)
        addQueuedDownload(buttonDownload)

        val info = daten.downloads.startInfo()

        assertEquals(4, info.totalDownloadListEntries)
        assertEquals(3, info.totalStarts)
        assertEquals(1, info.aboCount)
        assertEquals(3, info.downloadCount)
        assertEquals(1, info.initialized)
        assertEquals(1, info.running)
        assertEquals(1, info.finished)
        assertEquals(0, info.error)
    }

    @Test
    fun reconnectsDownloadsToLoadedFilms() {
        val film = DatenFilm().apply {
            urlNormalQuality = "https://example.invalid/video.mp4"
        }
        val matchingDownload = DatenDownload().apply {
            downloadUrl = film.urlNormalQuality
        }
        val existingFilm = DatenFilm().apply {
            urlNormalQuality = "https://example.invalid/existing.mp4"
        }
        val alreadyConnectedDownload = DatenDownload().apply {
            this.film = existingFilm
            downloadUrl = film.urlNormalQuality
        }
        daten.filmCatalog.allFilms.add(film)
        addQueuedDownload(matchingDownload)
        addQueuedDownload(alreadyConnectedDownload)

        daten.downloads.reconnectFilms()

        assertSame(film, matchingDownload.film)
        assertSame(existingFilm, alreadyConnectedDownload.film)
    }

    @Test
    fun refreshAboDownloadsRemovesResetsAndClearsDeferredEntries() {
        val unstartedAbo = aboDownload(null)
        val erroredAbo = aboDownload(DownloadRunState().apply { status = StartStatus.ERROR })
        val runningAbo = aboDownload(DownloadRunState().apply { status = StartStatus.RUNNING }).apply {
            isDeferred = true
        }
        val interruptedAbo = aboDownload(DownloadRunState().apply { status = StartStatus.RUNNING }).apply {
            isInterruptedFlag = true
        }
        val manualDownload = download(DownloadRunState().apply { status = StartStatus.INITIALIZED }).apply {
            isDeferred = true
        }
        addQueuedDownload(unstartedAbo)
        addQueuedDownload(erroredAbo)
        addQueuedDownload(runningAbo)
        addQueuedDownload(interruptedAbo)
        addQueuedDownload(manualDownload)

        daten.downloads.refreshAboDownloads()

        assertEquals(listOf(erroredAbo, runningAbo, interruptedAbo, manualDownload), daten.downloads.queuedDownloads())
        assertNull(erroredAbo.runtime.runState)
        assertFalse(runningAbo.isDeferred)
        assertTrue(interruptedAbo.isInterrupted)
        assertFalse(manualDownload.isDeferred)
    }

    @Test
    fun subscriptionScanAndMissingProgramSetPromptDoNotHoldDownloadQueueLock() {
        val filmCatalog = FilmCatalog()
        val film = DatenFilm().apply { urlNormalQuality = "https://example.invalid/abo.mp4" }
        filmCatalog.allFilms.add(film)
        val abos = Mockito.mock(AboServices::class.java)
        val history = Mockito.mock(AboHistoryController::class.java)
        Mockito.`when`(abos.historyController).thenReturn(history)
        val executor = Executors.newSingleThreadExecutor()
        var promptShown = false
        lateinit var downloads: DownloadServices
        downloads = DownloadServices(
            filmCatalog,
            ProgramSetRepository(),
            abos,
            BlacklistServices(filmCatalog),
            ReplacementRules(),
            Mockito.mock(NotificationPublisher::class.java),
        ) {
            assertEquals(emptyList<DatenDownload>(), executor.submit<List<DatenDownload>> { downloads.queuedDownloads() }.get(1, TimeUnit.SECONDS))
            promptShown = true
        }
        Mockito.`when`(abos.findAboForFilm(film, true)).thenAnswer {
            assertEquals(emptyList<DatenDownload>(), executor.submit<List<DatenDownload>> { downloads.queuedDownloads() }.get(1, TimeUnit.SECONDS))
            DatenAbo()
        }

        try {
            assertTrue(downloads.searchAboDownloads(Mockito.mock(JFrame::class.java)).isEmpty())
            assertTrue(promptShown)
        } finally {
            executor.shutdownNow()
            downloads.shutdown()
        }
    }

    @Test
    fun subscriptionScanRejectsConcurrentDownloadUsingStoredUrl() {
        val filmCatalog = FilmCatalog()
        val film = DatenFilm().apply { urlNormalQuality = "https://example.invalid/abo.mp4?token=abc" }
        filmCatalog.allFilms.add(film)
        val abos = Mockito.mock(AboServices::class.java)
        Mockito.`when`(abos.historyController).thenReturn(Mockito.mock(AboHistoryController::class.java))
        val programSets = ProgramSetRepository().apply { list.add(DatenPset()) }
        val downloads = DownloadServices(
            filmCatalog, programSets, abos, BlacklistServices(filmCatalog), ReplacementRules(),
            Mockito.mock(NotificationPublisher::class.java),
        ) { fail("No missing program set expected") }
        val concurrentDownload = DatenDownload().apply { downloadUrl = "https://example.invalid/abo.mp4" }
        Mockito.`when`(abos.findAboForFilm(film, true)).thenAnswer {
            downloads.addDownload(concurrentDownload)
            DatenAbo()
        }

        try {
            assertTrue(downloads.searchAboDownloads(null).isEmpty())
            assertEquals(listOf(concurrentDownload), downloads.queuedDownloads())
        } finally {
            downloads.shutdown()
        }
    }

    @Test
    fun subscriptionMergeReadsExistingQueueUrlsOnlyOnceAfterSnapshot() {
        val filmCatalog = FilmCatalog()
        val films = List(12) { index ->
            DatenFilm().apply { urlNormalQuality = "https://example.invalid/$index.mp4" }
        }
        filmCatalog.allFilms.addAll(films)
        val abos = Mockito.mock(AboServices::class.java)
        Mockito.`when`(abos.historyController).thenReturn(Mockito.mock(AboHistoryController::class.java))
        val programSets = ProgramSetRepository().apply { list.add(DatenPset()) }
        val downloads = DownloadServices(
            filmCatalog, programSets, abos, BlacklistServices(filmCatalog), ReplacementRules(),
            Mockito.mock(NotificationPublisher::class.java),
        ) { fail("No missing program set expected") }
        val existingDownload = Mockito.spy(DatenDownload().apply { downloadUrl = "https://example.invalid/existing.mp4" })
        downloads.addDownload(existingDownload)
        films.forEach { film -> Mockito.`when`(abos.findAboForFilm(film, true)).thenReturn(DatenAbo()) }
        Mockito.clearInvocations(existingDownload)

        try {
            val added = downloads.searchAboDownloads(null)
            assertEquals(films, added.map { it.film })
            assertEquals(listOf(existingDownload) + added, downloads.queuedDownloads())
            assertEquals((1..13).toList(), downloads.queuedDownloads().map { it.nr })
            Mockito.verify(existingDownload, Mockito.atMost(2)).downloadUrl
        } finally {
            downloads.shutdown()
        }
    }

    @Test
    fun subscriptionScanKeepsPartialResultsWhenMissingProgramSetThrows() {
        withSubscriptionScan { downloads, abos, programSets, films ->
            Mockito.`when`(abos.findAboForFilm(films[0], true)).thenReturn(DatenAbo())
            Mockito.`when`(abos.findAboForFilm(films[1], true)).thenAnswer {
                programSets.clear()
                DatenAbo().apply { psetName = "missing" }
            }

            assertThrows(IllegalStateException::class.java) { downloads.searchAboDownloads(null) }
            assertEquals(listOf(films[0]), downloads.queuedDownloads().map { it.film })
        }
    }

    @Test
    fun subscriptionScanKeepsPartialResultsWhenMatchingThrows() {
        withSubscriptionScan { downloads, abos, _, films ->
            Mockito.`when`(abos.findAboForFilm(films[0], true)).thenReturn(DatenAbo())
            val failure = IllegalArgumentException("matching failed")
            Mockito.`when`(abos.findAboForFilm(films[1], true)).thenThrow(failure)

            assertSame(failure, assertThrows(IllegalArgumentException::class.java) { downloads.searchAboDownloads(null) })
            assertEquals(listOf(films[0]), downloads.queuedDownloads().map { it.film })
        }
    }

    @Test
    fun subscriptionScanMergesPartialResultsBeforeMissingProgramSetPrompt() {
        var promptShown = false
        withSubscriptionScan(prompt = { downloads ->
            val queued = downloads.queuedDownloads()
            assertEquals(1, queued.size)
            assertEquals(1, queued.single().nr)
            val executor = Executors.newSingleThreadExecutor()
            try {
                assertEquals(queued, executor.submit<List<DatenDownload>> { downloads.queuedDownloads() }.get(1, TimeUnit.SECONDS))
            } finally {
                executor.shutdownNow()
            }
            promptShown = true
        }) { downloads, abos, programSets, films ->
            Mockito.`when`(abos.findAboForFilm(films[0], true)).thenReturn(DatenAbo())
            Mockito.`when`(abos.findAboForFilm(films[1], true)).thenAnswer {
                programSets.clear()
                DatenAbo().apply { psetName = "missing" }
            }

            assertEquals(listOf(films[0]), downloads.searchAboDownloads(Mockito.mock(JFrame::class.java)).map { it.film })
            assertTrue(promptShown)
        }
    }

    @Test
    fun subscriptionMergeDeduplicatesNormalizedUrlsWithinPreparedBatch() {
        withSubscriptionScan { downloads, abos, _, films ->
            films[0].urlNormalQuality = "https://example.invalid/shared.mp4?token=first"
            films[1].urlNormalQuality = "https://example.invalid/shared.mp4?token=second"
            films.forEach { film -> Mockito.`when`(abos.findAboForFilm(film, true)).thenReturn(DatenAbo()) }

            val added = downloads.searchAboDownloads(null)
            assertEquals(listOf(films[0]), added.map { it.film })
            assertEquals(added, downloads.queuedDownloads())
            assertEquals("https://example.invalid/shared.mp4", added.single().downloadUrl)
            assertEquals(1, added.single().nr)
        }
    }

    @Test
    fun subscriptionMergeRechecksQueueAfterCandidatesHaveBeenPrepared() {
        withSubscriptionScan { downloads, abos, _, films ->
            films[0].urlNormalQuality = "https://example.invalid/first.mp4?token=abc"
            val concurrentDownload = DatenDownload().apply { downloadUrl = "https://example.invalid/first.mp4" }
            Mockito.`when`(abos.findAboForFilm(films[0], true)).thenReturn(DatenAbo())
            val executor = Executors.newSingleThreadExecutor()
            Mockito.`when`(abos.findAboForFilm(films[1], true)).thenAnswer {
                executor.submit { downloads.addDownload(concurrentDownload) }.get(1, TimeUnit.SECONDS)
                DatenAbo()
            }

            try {
                val added = downloads.searchAboDownloads(null)
                assertEquals(listOf(films[1]), added.map { it.film })
                assertEquals(listOf(concurrentDownload) + added, downloads.queuedDownloads())
                assertEquals(listOf(1, 2), downloads.queuedDownloads().map { it.nr })
            } finally {
                executor.shutdownNow()
            }
        }
    }

    private fun withSubscriptionScan(
        prompt: (DownloadServices) -> Unit = { fail("No missing program set prompt expected") },
        block: (DownloadServices, AboServices, ProgramSetRepository, List<DatenFilm>) -> Unit,
    ) {
        val filmCatalog = FilmCatalog()
        val films = List(2) { index ->
            DatenFilm().apply { urlNormalQuality = "https://example.invalid/$index.mp4" }
        }
        filmCatalog.allFilms.addAll(films)
        val abos = Mockito.mock(AboServices::class.java)
        Mockito.`when`(abos.historyController).thenReturn(Mockito.mock(AboHistoryController::class.java))
        val programSets = ProgramSetRepository().apply { list.add(DatenPset()) }
        lateinit var downloads: DownloadServices
        downloads = DownloadServices(
            filmCatalog, programSets, abos, BlacklistServices(filmCatalog), ReplacementRules(),
            Mockito.mock(NotificationPublisher::class.java),
        ) { prompt(downloads) }
        try {
            block(downloads, abos, programSets, films)
        } finally {
            downloads.shutdown()
        }
    }

    private fun buttonDownload(
        filmUrl: String,
        downloadUrl: String,
        status: StartStatus,
    ): DatenDownload =
        DatenDownload().apply {
            this.filmUrl = filmUrl
            this.downloadUrl = downloadUrl
            quelle = DownloadSource.BUTTON
            runtime.runState = DownloadRunState().apply {
                this.status = status
            }
        }

    private fun download(runState: DownloadRunState?): DatenDownload =
        DatenDownload().apply {
            quelle = DownloadSource.DOWNLOAD
            runtime.runState = runState
        }

    private fun namedDownload(name: String): DatenDownload =
        download(DownloadRunState().apply { status = StartStatus.INITIALIZED }).apply {
            filmUrl = "https://example.invalid/$name"
        }

    private fun aboDownload(runState: DownloadRunState?): DatenDownload =
        DatenDownload().apply {
            quelle = DownloadSource.ABO
            aboName = "Abo"
            runtime.runState = runState
        }

    private fun addQueuedDownload(download: DatenDownload) {
        daten.downloads.addLoadedDownload(download)
    }

    private fun addButtonDownload(download: DatenDownload) {
        daten.downloads.addButtonDownload(download)
    }

    private fun allDownloadsFilter(): DownloadListFilter =
        DownloadListFilter(
            onlyAbos = false,
            onlyDownloads = false,
            onlyNotStarted = false,
            onlyStarted = false,
            onlyWaiting = false,
            onlyRun = false,
            onlyFinished = false,
        )
}
