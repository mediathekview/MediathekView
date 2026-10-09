package mediathek.filmlisten

import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeout
import mediathek.config.StandardLocations
import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.tool.MessageBus
import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.Test
import org.junit.jupiter.api.io.TempDir
import org.junit.jupiter.api.parallel.ResourceLock
import java.nio.file.Files
import java.nio.file.Path
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.Duration.Companion.seconds

@ResourceLock("SenderFilmlistLoadApprover")
class FilmListLoadCoordinatorTest {
    @TempDir
    lateinit var tempDir: Path

    private var previousPortableBaseDirectory: String? = null
    private lateinit var approvedSenders: Set<String>

    @BeforeEach
    fun setUp() {
        previousPortableBaseDirectory = StandardLocations.portableBaseDirectory
        StandardLocations.portableBaseDirectory = tempDir.toString()
        approvedSenders = SenderFilmlistLoadApprover.senderSet.toSet()
        SenderFilmlistLoadApprover.senderSet.clear()
        SenderFilmlistLoadApprover.senderSet.add("APPROVED")
    }

    @AfterEach
    fun tearDown() {
        StandardLocations.portableBaseDirectory = previousPortableBaseDirectory
        SenderFilmlistLoadApprover.senderSet.clear()
        SenderFilmlistLoadApprover.senderSet.addAll(approvedSenders)
    }

    @Test
    fun `startStartupPostLoad skips new loads while post-load work is running`() = runBlocking {
        val loader = loader()
        val presenter = BlockingFilmListLoadPresenter()

        val firstLoad = loader.startStartupPostLoad(failed = false, presenter)
        assertTrue(firstLoad.started)
        presenter.awaitEntered()
        assertTrue(loader.isFilmListLoadRunning)

        val skippedLoad = loader.startStartupPostLoad(failed = false, NoOpFilmListLoadPresenter)

        assertFalse(skippedLoad.started)
        assertTrue(skippedLoad.completion.await().skipped)
        presenter.release()
        assertFalse(firstLoad.completion.await().failed)
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad skips while another load is running`() = runBlocking {
        val loader = loader()
        val presenter = BlockingFilmListLoadPresenter()
        val runningLoad = loader.startStartupPostLoad(failed = false, presenter)
        presenter.awaitEntered()

        val skippedLoad = loader.startFilmlistLoad("", immerNeuLaden = false)

        assertFalse(skippedLoad.started)
        assertTrue(skippedLoad.completion.await().skipped)
        presenter.release()
        assertFalse(runningLoad.completion.await().failed)
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistUpdate skips while another load is running`() = runBlocking {
        val loader = loader()
        val presenter = BlockingFilmListLoadPresenter()
        val runningLoad = loader.startStartupPostLoad(failed = false, presenter)
        presenter.awaitEntered()

        val skippedLoad = loader.startFilmlistUpdate("https://example.invalid/filmlist.json")

        assertFalse(skippedLoad.started)
        assertTrue(skippedLoad.completion.await().skipped)
        presenter.release()
        assertFalse(runningLoad.completion.await().failed)
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad routes non-empty source through file import`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.NO_UPDATE))
        val loader = loader(importer)

        val load = loader.startFilmlistLoad("/tmp/filmlist.json", immerNeuLaden = false)

        assertTrue(load.started)
        assertFalse(load.completion.await().failed)
        assertEquals(0, importer.importFromUrlCount.get())
        assertEquals(1, importer.importFromFileCount.get())
        assertEquals(0, importer.importAdditionalFromFileCount.get())
        assertEquals("/tmp/filmlist.json", importer.lastFileImportPath)
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistUpdate routes through additional file import`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.NO_UPDATE))
        val loader = loader(importer)

        val load = loader.startFilmlistUpdate("https://example.invalid/update.json")

        assertTrue(load.started)
        assertFalse(load.completion.await().failed)
        assertEquals(0, importer.importFromUrlCount.get())
        assertEquals(0, importer.importFromFileCount.get())
        assertEquals(1, importer.importAdditionalFromFileCount.get())
        assertEquals("https://example.invalid/update.json", importer.lastAdditionalImportPath)
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad forwards reader start and progress events`() = runBlocking {
        val loader = loader()
        val listener = RecordingFilmListLoadListener()
        val source = writeFilmList(filmEntry()).toString()
        loader.addLoadListener(listener)

        val load = loader.startFilmlistLoad(source, immerNeuLaden = false)

        assertTrue(load.started)
        assertEquals(source, listener.awaitStarted().senderUrl)
        assertEquals(source, listener.awaitProgress().senderUrl)
        assertFalse(load.completion.await().failed)
        awaitNotRunning(loader)
    }

    @Test
    fun `startStartupPostLoad completes failed startup loads as failed`() = runBlocking {
        val loader = loader()

        val failedLoad = loader.startStartupPostLoad(failed = true, NoOpFilmListLoadPresenter)

        assertTrue(failedLoad.started)
        assertTrue(failedLoad.completion.await().failed)
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad completes no-update loads without post-processing by default`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.NO_UPDATE))
        val loader = loader(importer)
        val presenter = BlockingFilmListLoadPresenter()
        val listener = RecordingFilmListLoadListener()
        loader.setLoadPresenter(presenter)
        loader.addLoadListener(listener)

        val load = loader.startFilmlistLoad("", immerNeuLaden = false)

        assertTrue(load.started)
        assertFalse(load.completion.await().failed)
        assertFalse(listener.awaitFinished().failed)
        assertEquals(1, importer.importFromUrlCount.get())
        assertFalse(loader.isFilmListLoadRunning)
        assertFalse(presenter.hasEntered)
    }

    @Test
    fun `startFilmlistLoad runs post-processing for no-update loads when requested`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.NO_UPDATE))
        val loader = loader(importer)
        val presenter = BlockingFilmListLoadPresenter()
        loader.setLoadPresenter(presenter)

        val load = loader.startFilmlistLoadWithNoUpdatePostProcessing(
            "",
            immerNeuLaden = true,
            persistAfterLoad = true,
        )
        presenter.awaitEntered()

        assertTrue(load.started)
        assertEquals(1, importer.importFromUrlCount.get())
        assertTrue(loader.isFilmListLoadRunning)
        presenter.release()
        assertFalse(load.completion.await().failed)
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad completes successful imports after post-processing`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.SUCCESS))
        val loader = loader(importer)
        val presenter = BlockingFilmListLoadPresenter()
        val listener = RecordingFilmListLoadListener()
        loader.setLoadPresenter(presenter)
        loader.addLoadListener(listener)

        val load = loader.startFilmlistLoad("", immerNeuLaden = false)
        presenter.awaitEntered()

        assertTrue(load.started)
        assertEquals(1, importer.importFromUrlCount.get())
        assertEquals(0, importer.reloadSavedFilmListCount.get())
        assertEquals(0, presenter.loadFailedDialogCount.get())
        assertTrue(loader.isFilmListLoadRunning)
        presenter.release()
        assertFalse(load.completion.await().failed)
        assertFalse(listener.awaitFinished().failed)
        assertTrue(Files.exists(Path.of(StandardLocations.getFilmlistFilePathString())))
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad publishes read stop event for successful full imports`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.SUCCESS))
        val loader = loader(importer)
        val presenter = BlockingFilmListLoadPresenter()
        val subscriber = RecordingFilmListReadStopSubscriber()
        loader.setLoadPresenter(presenter)

        val load = withReadStopSubscriber(subscriber) {
            loader.startFilmlistLoad("", immerNeuLaden = false)
        }
        presenter.awaitEntered()
        presenter.release()

        assertFalse(load.completion.await().failed)
        assertEquals(1, subscriber.readStopEventCount.get())
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad publishes read stop event for failed full imports`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.FAILURE))
        val loader = loader(importer)
        val presenter = BlockingFilmListLoadPresenter()
        val subscriber = RecordingFilmListReadStopSubscriber()
        loader.setLoadPresenter(presenter)

        val load = withReadStopSubscriber(subscriber) {
            loader.startFilmlistLoad("", immerNeuLaden = false)
        }
        presenter.awaitEntered()
        presenter.release()

        assertTrue(load.completion.await().failed)
        assertEquals(1, subscriber.readStopEventCount.get())
        awaitNotRunning(loader)
    }

    @Test
    fun `startFilmlistLoad completes failed imports as failed after restoring saved list`() = runBlocking {
        val importer = RecordingFilmListImporter(FilmListImportOutcome(FilmListImportResult.FAILURE))
        val loader = loader(importer)
        val presenter = BlockingFilmListLoadPresenter()
        val listener = RecordingFilmListLoadListener()
        loader.setLoadPresenter(presenter)
        loader.addLoadListener(listener)

        val load = loader.startFilmlistLoad("", immerNeuLaden = false)
        presenter.awaitEntered()

        assertTrue(load.started)
        assertEquals(1, importer.importFromUrlCount.get())
        assertEquals(1, presenter.loadFailedDialogCount.get())
        assertEquals(1, importer.reloadSavedFilmListCount.get())
        presenter.release()
        assertTrue(load.completion.await().failed)
        assertTrue(listener.awaitFinished().failed)
        awaitNotRunning(loader)
    }

    private fun loader(): FilmListLoadCoordinator {
        val filmCatalog = FilmCatalog()
        return FilmListLoadCoordinator(
            filmCatalog,
            AboServices(filmCatalog.allFilms),
            BlacklistServices(filmCatalog),
        )
    }

    private fun loader(importer: FilmListImporter): FilmListLoadCoordinator {
        val filmCatalog = FilmCatalog()
        return FilmListLoadCoordinator(
            filmCatalog,
            AboServices(filmCatalog.allFilms),
            BlacklistServices(filmCatalog),
            importer,
        )
    }

    private fun withReadStopSubscriber(
        subscriber: RecordingFilmListReadStopSubscriber,
        block: () -> FilmListLoadHandle,
    ): FilmListLoadHandle {
        MessageBus.messageBus.subscribe(subscriber)
        val handle = try {
            block()
        } catch (ex: Throwable) {
            MessageBus.messageBus.unsubscribe(subscriber)
            throw ex
        }
        handle.completion.invokeOnCompletion {
            MessageBus.messageBus.unsubscribe(subscriber)
        }
        return handle
    }

    private suspend fun awaitNotRunning(loader: FilmListLoadCoordinator) {
        withTimeout(5.seconds) {
            while (loader.isFilmListLoadRunning) {
                delay(10.milliseconds)
            }
        }
    }

    private fun writeFilmList(vararg entries: String): Path {
        val file = tempDir.resolve("progress.json")
        Files.writeString(
            file,
            buildString {
                append("{")
                append("\"Filmliste\":[\"\",\"15.05.2026, 12:00\",\"\",\"\",\"test-list\"],")
                append("\"Filmliste\":[\"\"],")
                append(entries.joinToString(","))
                append("}")
            },
        )
        return file
    }

    private fun filmEntry(): String {
        val sender = "APPROVED"
        val title = "New title"
        return listOf(
            sender,
            "New",
            title,
            "15.05.2026",
            "12:00",
            "00:30:00",
            "",
            "Beschreibung",
            "https://example.test/$sender/$title.mp4",
            "https://example.test/$sender/$title",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "",
            "false",
        ).joinToString(prefix = "\"X\":[", postfix = "]") { "\"${it.escapeJson()}\"" }
    }

    private fun String.escapeJson(): String =
        replace("\\", "\\\\")
            .replace("\"", "\\\"")
}
