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

package mediathek.filmlisten

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.launch
import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.ListeFilme
import mediathek.daten.abo.AboServices
import mediathek.daten.blacklist.BlacklistServices
import mediathek.gui.messages.FilmListReadStopEvent
import mediathek.tool.FilmListUpdateType
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import kotlin.coroutines.cancellation.CancellationException

private enum class NoUpdateCompletion {
    COMPLETE_LOAD,
    POST_PROCESS,
}

class FilmListLoadCoordinator(
    private val filmCatalog: FilmCatalog,
    abos: AboServices,
    blacklist: BlacklistServices,
) {
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val events = FilmListLoadEventDispatcher(scope)
    private val postLoadRunner = FilmListPostLoadRunner(filmCatalog, abos, blacklist, scope)
    private val loadState = FilmListLoadState()
    private var presenter: FilmListLoadPresenter = NoOpFilmListLoadPresenter
    private var importService: FilmListImporter = createImportService()

    internal constructor(
        filmCatalog: FilmCatalog,
        abos: AboServices,
        blacklist: BlacklistServices,
        importService: FilmListImporter,
    ) : this(filmCatalog, abos, blacklist) {
        this.importService = importService
    }

    private fun createImportService(): FilmListImporter = FilmListImportService(
        feedback = PresenterFilmListImportFeedback { presenter },
        progressListener = object : FilmListLoadListener {
            @Synchronized
            override fun loadStarted(progress: FilmListLoadProgress) {
                events.notifyStart(progress)
            }

            @Synchronized
            override fun loadProgress(progress: FilmListLoadProgress) {
                events.notifyProgress(progress)
            }

            @Synchronized
            override fun loadFinished(progress: FilmListLoadProgress) {
                // handled by the async import methods below
            }
        },
    )

    private fun displayLogInfo(listeFilme: ListeFilme) {
        logger.info("Alte Liste erstellt am: {}", listeFilme.metaData.generationDateTimeAsString)
        logFilmCounts(listeFilme)
    }

    private fun logFilmCounts(listeFilme: ListeFilme) {
        logger.info("  Anzahl Filme: {}", listeFilme.size)
        logger.info("  Anzahl Neue: {}", listeFilme.countNewFilms())
    }

    private fun logFilmListSource(dateiUrl: String) {
        logger.info("Filmliste laden von: {}", dateiUrl)
    }

    fun startFilmlistLoad(dateiUrl: String, immerNeuLaden: Boolean): FilmListLoadHandle =
        startFilmlistLoad(dateiUrl, immerNeuLaden, persistAfterLoad = true)

    fun startAutomaticStartupUpdate(): FilmListLoadHandle {
        if (!shouldStartAutomaticStartupUpdate()) {
            return FilmListLoadHandle.skipped()
        }

        return startFilmlistLoadWithNoUpdatePostProcessing("", true, persistAfterLoad = true)
    }

    private fun shouldStartAutomaticStartupUpdate(): Boolean =
        FilmListUpdateType.fromConfig() == FilmListUpdateType.AUTOMATIC &&
            filmCatalog.allFilms.needsUpdate()

    internal fun startFilmlistLoad(
        dateiUrl: String,
        immerNeuLaden: Boolean,
        persistAfterLoad: Boolean,
    ): FilmListLoadHandle = startFilmlistLoad(
        dateiUrl = dateiUrl,
        immerNeuLaden = immerNeuLaden,
        persistAfterLoad = persistAfterLoad,
        noUpdateCompletion = NoUpdateCompletion.COMPLETE_LOAD,
    )

    internal fun startFilmlistLoadWithNoUpdatePostProcessing(
        dateiUrl: String,
        immerNeuLaden: Boolean,
        persistAfterLoad: Boolean,
    ): FilmListLoadHandle = startFilmlistLoad(
        dateiUrl = dateiUrl,
        immerNeuLaden = immerNeuLaden,
        persistAfterLoad = persistAfterLoad,
        noUpdateCompletion = NoUpdateCompletion.POST_PROCESS,
    )

    private fun startFilmlistLoad(
        dateiUrl: String,
        immerNeuLaden: Boolean,
        persistAfterLoad: Boolean,
        noUpdateCompletion: NoUpdateCompletion,
    ): FilmListLoadHandle {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        val listeFilme = filmCatalog.allFilms

        logger.trace("startFilmlistLoad(String,boolean,boolean,boolean)")
        logger.info("")
        displayLogInfo(listeFilme)

        return beginLoadOperationOrSkip { operation ->
            if (dateiUrl.isEmpty()) {
                startFullFilmListImportFromUrl(
                    listeFilme = listeFilme,
                    immerNeuLaden = immerNeuLaden,
                    persistAfterLoad = persistAfterLoad,
                    noUpdateCompletion = noUpdateCompletion,
                    operation = operation,
                )
            } else {
                startFullFilmListImportFromFile(
                    dateiUrl = dateiUrl,
                    listeFilme = listeFilme,
                    persistAfterLoad = persistAfterLoad,
                    noUpdateCompletion = noUpdateCompletion,
                    operation = operation,
                )
            }
        }
    }

    private fun startFullFilmListImportFromUrl(
        listeFilme: ListeFilme,
        immerNeuLaden: Boolean,
        persistAfterLoad: Boolean,
        noUpdateCompletion: NoUpdateCompletion,
        operation: FilmListLoadOperation,
    ) {
        logger.info("Filmliste laden (Netzwerk)")
        runImportAsync(
            operationName = "importFromUrl",
            persistAfterLoad = persistAfterLoad,
            operation = operation,
            completeNoUpdate = { finishNoUpdateImport(noUpdateCompletion, operation) },
        ) {
            importService.importFromUrl("", listeFilme, loadNumDays, immerNeuLaden, ::prepareLoad)
        }
    }

    private fun startFullFilmListImportFromFile(
        dateiUrl: String,
        listeFilme: ListeFilme,
        persistAfterLoad: Boolean,
        noUpdateCompletion: NoUpdateCompletion,
        operation: FilmListLoadOperation,
    ) {
        logFilmListSource(dateiUrl)
        runImportAsync(
            operationName = "importFromFile",
            persistAfterLoad = persistAfterLoad,
            operation = operation,
            completeNoUpdate = { finishNoUpdateImport(noUpdateCompletion, operation) },
        ) {
            importService.importFromFile(dateiUrl, listeFilme, loadNumDays, ::prepareLoad)
        }
    }

    fun startFilmlistUpdate(dateiUrl: String): FilmListLoadHandle {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        logger.debug("Filme laden (Update), start")
        logger.info("")
        displayLogInfo(filmCatalog.allFilms)

        return beginLoadOperationOrSkip { operation ->
            startAdditionalFilmListImport(dateiUrl, operation)
        }
    }

    private fun startAdditionalFilmListImport(
        dateiUrl: String,
        operation: FilmListLoadOperation,
    ) {
        logFilmListSource(dateiUrl)
        val sourceUrl = dateiUrl.ifEmpty {
            StandardLocations.getFilmListUrl(FilmListDownloadType.FULL)
        }
        val oldFilmUrlKeys = prepareLoad()
        runImportAsync(
            operationName = "importAdditionalFromFile",
            persistAfterLoad = true,
            operation = operation,
            completeNoUpdate = { finishLoad(FilmListLoadProgress.completed(failed = false), operation) },
        ) {
            importService.importAdditionalFromFile(sourceUrl, loadNumDays, oldFilmUrlKeys)
        }
    }

    fun addLoadListener(listener: FilmListLoadListener) {
        events.addListener(listener)
    }

    fun removeLoadListener(listener: FilmListLoadListener) {
        events.removeListener(listener)
    }

    internal fun setLoadPresenter(presenter: FilmListLoadPresenter) {
        this.presenter = presenter
    }

    val isFilmListLoadRunning: Boolean
        get() = loadState.isRunning

    private fun beginLoadOperationOrSkip(startLoad: (FilmListLoadOperation) -> Unit): FilmListLoadHandle {
        val operation = FilmListLoadOperation.begin(loadState)
        if (operation.handle.started) {
            startLoad(operation)
        }
        return operation.handle
    }

    private fun prepareLoad(): Set<String> {
        val oldFilmUrlKeys = FilmListImportApplier.collectFilmUrlKeys(filmCatalog.allFilms)
        filmCatalog.filteredFilms.clear()
        return oldFilmUrlKeys
    }

    private val loadNumDays: Int
        get() = ApplicationConfiguration.getInstance().filmListLoadNumDays

    private fun runImportAsync(
        operationName: String,
        persistAfterLoad: Boolean,
        operation: FilmListLoadOperation,
        completeNoUpdate: () -> Unit,
        importAction: () -> FilmListImportOutcome,
    ) {
        scope.launch {
            val outcome = try {
                importAction()
            } catch (ex: CancellationException) {
                operation.completeExceptionally(ex)
                throw ex
            } catch (ex: Exception) {
                logger.error(operationName, ex)
                FilmListImportOutcome(FilmListImportResult.FAILURE)
            }

            logger.trace("Filme laden, ende")
            if (outcome.result == FilmListImportResult.NO_UPDATE) {
                completeNoUpdate()
                return@launch
            }
            finishImport(
                failed = outcome.result != FilmListImportResult.SUCCESS,
                persistAfterLoad,
                outcome.oldFilmUrlKeys,
                outcome.importedDiffList,
                operation,
            )
        }
    }

    private fun finishNoUpdateImport(
        noUpdateCompletion: NoUpdateCompletion,
        operation: FilmListLoadOperation,
    ) {
        when (noUpdateCompletion) {
            NoUpdateCompletion.COMPLETE_LOAD -> finishLoad(FilmListLoadProgress.completed(failed = false), operation)
            NoUpdateCompletion.POST_PROCESS -> startPostLoadWork(persistFilmList = false, operation)
        }
    }

    private fun finishImport(
        failed: Boolean,
        persistAfterLoad: Boolean,
        oldFilmUrlKeys: Set<String>,
        diffListe: ListeFilme,
        operation: FilmListLoadOperation,
    ) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        logger.debug("finishImport()")
        val listeFilme = filmCatalog.allFilms
        FilmListImportApplier.applyImportedFilms(listeFilme, diffListe, oldFilmUrlKeys)

        val persistFilmList = if (failed) restoreSavedFilmListAfterFailure(listeFilme) else persistAfterLoad

        logger.info("")
        logger.info("Jetzige Liste erstellt am: {}", listeFilme.metaData.generationDateTimeAsString)
        logFilmCounts(listeFilme)
        logger.info("")

        MessageBus.messageBus.publish(FilmListReadStopEvent())
        startPostLoadWork(persistFilmList, operation, failed)
    }

    private fun restoreSavedFilmListAfterFailure(listeFilme: ListeFilme): Boolean {
        logger.info("")
        logger.info("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen")
        presenter.showLoadFailedDialog()

        importService.reloadSavedFilmList(listeFilme, loadNumDays)
        logger.info("")

        return false
    }

    internal fun startStartupPostLoad(failed: Boolean, startupPresenter: FilmListLoadPresenter): FilmListLoadHandle =
        beginLoadOperationOrSkip { operation ->
            scope.launch {
                if (failed) {
                    finishLoad(FilmListLoadProgress.completed(failed = true), operation)
                } else {
                    operation.startPostLoad()
                    postLoadRunner.start(persistFilmList = false, startupPresenter) { progress ->
                        finishLoad(progress, operation)
                    }
                }
            }
        }

    private fun startPostLoadWork(
        persistFilmList: Boolean,
        operation: FilmListLoadOperation,
        failed: Boolean = false,
    ) {
        operation.startPostLoad()
        postLoadRunner.start(persistFilmList, presenter) { progress ->
            finishLoad(progress.copy(failed = progress.failed || failed), operation)
        }
    }

    private fun finishLoad(
        progress: FilmListLoadProgress,
        operation: FilmListLoadOperation,
    ) {
        events.notifyFinished(progress)
        operation.finish(progress)
    }

    companion object {
        private val logger = LogManager.getLogger(FilmListLoadCoordinator::class.java)
    }
}
