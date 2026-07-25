package mediathek.controller.starter

import mediathek.config.Daten
import mediathek.controller.DownloadColumn
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.daten.DownloadSource
import mediathek.daten.DownloadType
import mediathek.daten.abo.DatenAbo
import mediathek.tool.models.TModelDownload
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test

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
