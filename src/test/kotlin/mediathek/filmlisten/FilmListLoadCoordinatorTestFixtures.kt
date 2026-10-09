package mediathek.filmlisten

import kotlinx.coroutines.CompletableDeferred
import kotlinx.coroutines.withTimeout
import mediathek.daten.ListeFilme
import mediathek.gui.messages.FilmListReadStopEvent
import net.engio.mbassy.listener.Handler
import java.util.concurrent.atomic.AtomicInteger
import javax.swing.JLabel
import javax.swing.JProgressBar
import kotlin.time.Duration.Companion.seconds

internal class BlockingFilmListLoadPresenter : FilmListLoadPresenter {
    private val entered = CompletableDeferred<Unit>()
    private val released = CompletableDeferred<Unit>()

    val hasEntered: Boolean
        get() = entered.isCompleted

    val loadFailedDialogCount = AtomicInteger(0)
    val noUpdateAvailableCount = AtomicInteger(0)
    var lastNoUpdateShowDialogs: Boolean? = null

    override fun showNoUpdateAvailable(showDialogs: Boolean) {
        noUpdateAvailableCount.incrementAndGet()
        lastNoUpdateShowDialogs = showDialogs
    }

    override fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean) = Unit

    override fun showLoadFailedDialog() {
        loadFailedDialogCount.incrementAndGet()
    }

    override suspend fun <T> withStatusBarWidgets(block: suspend (FilmListStatusBarWidgets) -> T): T {
        entered.complete(Unit)
        released.await()
        return block(FilmListStatusBarWidgets(TestProgressHandle(), host = null))
    }

    suspend fun awaitEntered() {
        withTimeout(5.seconds) {
            entered.await()
        }
    }

    fun release() {
        released.complete(Unit)
    }
}

internal class RecordingFilmListImporter(
    private val outcome: FilmListImportOutcome,
) : FilmListImporter {
    val importFromUrlCount = AtomicInteger(0)
    val importFromFileCount = AtomicInteger(0)
    val importAdditionalFromFileCount = AtomicInteger(0)
    val reloadSavedFilmListCount = AtomicInteger(0)
    var lastFileImportPath: String? = null
    var lastAdditionalImportPath: String? = null

    override fun importFromUrl(
        dateiUrl: String,
        listeFilme: ListeFilme,
        days: Int,
        immerNeuLaden: Boolean,
        prepareImport: () -> Set<String>,
    ): FilmListImportOutcome {
        importFromUrlCount.incrementAndGet()
        prepareSuccessfulFullImport(listeFilme)
        return outcome
    }

    override fun importFromFile(
        pfad: String,
        listeFilme: ListeFilme,
        days: Int,
        prepareImport: () -> Set<String>,
    ): FilmListImportOutcome {
        importFromFileCount.incrementAndGet()
        lastFileImportPath = pfad
        prepareSuccessfulFullImport(listeFilme)
        return outcome
    }

    private fun prepareSuccessfulFullImport(listeFilme: ListeFilme) {
        if (outcome.result == FilmListImportResult.SUCCESS) {
            listeFilme.metaData.datum = "15.05.2026, 12:00"
            listeFilme.metaData.id = "test-list"
        }
    }

    override fun importAdditionalFromFile(
        pfad: String,
        days: Int,
        oldFilmUrlKeys: Set<String>,
    ): FilmListImportOutcome {
        importAdditionalFromFileCount.incrementAndGet()
        lastAdditionalImportPath = pfad
        return outcome
    }

    override fun reloadSavedFilmList(listeFilme: ListeFilme, days: Int) {
        reloadSavedFilmListCount.incrementAndGet()
    }
}

internal class RecordingFilmListLoadListener : FilmListLoadListener {
    private val startedProgress = CompletableDeferred<FilmListLoadProgress>()
    private val currentProgress = CompletableDeferred<FilmListLoadProgress>()
    private val finishedProgress = CompletableDeferred<FilmListLoadProgress>()

    override fun loadStarted(progress: FilmListLoadProgress) {
        startedProgress.complete(progress)
    }

    override fun loadProgress(progress: FilmListLoadProgress) {
        currentProgress.complete(progress)
    }

    override fun loadFinished(progress: FilmListLoadProgress) {
        finishedProgress.complete(progress)
    }

    suspend fun awaitStarted(): FilmListLoadProgress =
        withTimeout(5.seconds) {
            startedProgress.await()
        }

    suspend fun awaitProgress(): FilmListLoadProgress =
        withTimeout(5.seconds) {
            currentProgress.await()
        }

    suspend fun awaitFinished(): FilmListLoadProgress =
        withTimeout(5.seconds) {
            finishedProgress.await()
        }
}

internal class RecordingFilmListReadStopSubscriber {
    val readStopEventCount = AtomicInteger(0)

    @Handler
    @Suppress("UNUSED_PARAMETER")
    fun handleFilmListReadStop(event: FilmListReadStopEvent) {
        readStopEventCount.incrementAndGet()
    }
}

private class TestProgressHandle : FilmListProgressHandle {
    private val label = JLabel()
    private val progressBar = JProgressBar()

    override fun label(): JLabel = label

    override fun progressBar(): JProgressBar = progressBar

    override fun close() = Unit
}
