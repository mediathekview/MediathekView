package mediathek.daten.abo

import kotlinx.coroutines.*
import mediathek.controller.history.AboHistoryController
import mediathek.daten.DatenFilm
import mediathek.daten.ListeAbo
import mediathek.daten.ListeFilme
import mediathek.gui.messages.AboListChangedEvent
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import java.util.concurrent.ExecutionException

class AboServices(
    private val allFilms: ListeFilme,
) {
    private val historyScope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val filmAssignmentService = AboFilmAssignmentService()
    private var completedAboHistory: AboHistoryController? = null
    private var historyJob: Deferred<Unit>? = null

    val list: ListeAbo = ListeAbo(::handleListChanged)

    val historyController: AboHistoryController
        get() = checkNotNull(completedAboHistory) {
            "AboHistoryController accessed before launchHistoryDataLoading() completed"
        }

    fun findAboForFilm(film: DatenFilm, checkLength: Boolean): DatenAbo? =
        filmAssignmentService.findAboForFilm(film, checkLength)

    fun assignAbosToFilms(removeMissingAbos: Boolean) {
        filmAssignmentService.assignAbosToFilms(
            list.assignmentSnapshot(),
            allFilms,
            removeMissingAbos,
        )
    }

    fun notifyListChanged() {
        handleListChanged()
    }

    private fun handleListChanged() {
        assignAbosToFilms(removeMissingAbos = true)
        MessageBus.messageBus.publishAsync(AboListChangedEvent())
    }

    fun launchHistoryDataLoading() {
        logger.trace("launching async history data loading")
        val loadingJob = historyScope.async {
            completedAboHistory = AboHistoryController()
        }
        loadingJob.invokeOnCompletion { throwable ->
            if (throwable != null) {
                logger.error("launchAboHistoryController", throwable)
            }
        }
        historyJob = loadingJob
    }

    @Throws(ExecutionException::class, InterruptedException::class)
    fun waitForHistoryDataLoadingToComplete() {
        val runningHistoryLoad = historyJob ?: return

        try {
            runBlocking {
                runningHistoryLoad.await()
            }
        } catch (exception: InterruptedException) {
            throw exception
        } catch (exception: Throwable) {
            throw ExecutionException(exception)
        } finally {
            if (historyJob === runningHistoryLoad) {
                historyJob = null
            }
        }
    }

    private companion object {
        private val logger = LogManager.getLogger(AboServices::class.java)
    }
}
