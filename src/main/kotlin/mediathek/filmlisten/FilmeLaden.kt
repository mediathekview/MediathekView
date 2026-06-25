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
import mediathek.config.Daten
import mediathek.config.StandardLocations
import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.ListeFilme
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.gui.messages.FilmListReadStopEvent
import mediathek.mainwindow.FilmListLoadHost
import mediathek.tool.FilmListUpdateType
import mediathek.tool.MessageBus
import org.apache.logging.log4j.LogManager
import kotlin.coroutines.cancellation.CancellationException

class FilmeLaden(private val daten: Daten) {
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)
    private val ui = FilmListLoadUi(scope)
    private val events = FilmListLoadEventDispatcher(scope)
    private val postLoadRunner = FilmListPostLoadRunner(daten, scope, ui)
    private val importState = FilmListImportState()
    private val importService = FilmListImportService(
        feedback = ui,
        progressListener = object : ListenerFilmeLaden() {
            @Synchronized
            override fun start(event: ListenerFilmeLadenEvent) {
                events.notifyStart(event)
            }

            @Synchronized
            override fun progress(event: ListenerFilmeLadenEvent) {
                events.notifyProgress(event)
            }

            @Suppress("UNUSED_PARAMETER")
            @Synchronized
            override fun fertig(event: ListenerFilmeLadenEvent) {
                // handled by the async import methods below
            }
        },
    )
    private fun displayLogInfo(listeFilme: ListeFilme) {
        logger.info("Alte Liste erstellt am: {}", listeFilme.metaData.generationDateTimeAsString)
        logger.info("  Anzahl Filme: {}", listeFilme.size)
        logger.info("  Anzahl Neue: {}", listeFilme.countNewFilms())
    }

    fun loadFilmlist(dateiUrl: String, immerNeuLaden: Boolean): Boolean =
        loadFilmlist(dateiUrl, immerNeuLaden, FilmListLoadOptions.normal())

    fun startAutomaticStartupUpdateIfNeeded(): Boolean {
        if (!shouldStartAutomaticStartupUpdate()) {
            return false
        }

        return loadFilmlist("", true, FilmListLoadOptions(writeAfterLoad = true, postProcessWhenNoUpdate = true))
    }

    private fun shouldStartAutomaticStartupUpdate(): Boolean =
        FilmListUpdateType.fromConfig() == FilmListUpdateType.AUTOMATIC &&
            daten.listeFilme.needsUpdate()

    fun loadFilmlist(dateiUrl: String, immerNeuLaden: Boolean, loadOptions: FilmListLoadOptions): Boolean {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        val listeFilme = daten.listeFilme

        logger.trace("loadFilmlist(String,boolean,FilmListLoadOptions)")
        logger.info("")
        displayLogInfo(listeFilme)

        if (!importState.tryBegin()) {
            return false
        }

        val days = loadNumDays
        if (dateiUrl.isEmpty()) {
            logger.info("Filmliste laden (Netzwerk)")
            runImportAsync(
                operationName = "importFromUrl",
                options = loadOptions,
            ) {
                importService.importFromUrl(dateiUrl, listeFilme, days, immerNeuLaden, ::prepareLoad)
            }
        } else {
            logger.info("Filmliste laden von: {}", dateiUrl)
            runImportAsync(
                operationName = "importFromFile",
                options = loadOptions,
            ) {
                importService.importFromFile(dateiUrl, listeFilme, days, ::prepareLoad)
            }
        }
        return true
    }

    fun updateFilmlist(dateiUrl: String) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        logger.debug("Filme laden (Update), start")
        logger.info("")
        displayLogInfo(daten.listeFilme)

        if (!beginLoad()) {
            return
        }

        logger.info("Filmliste laden von: {}", dateiUrl)
        val sourceUrl = dateiUrl.ifEmpty {
            StandardLocations.getFilmListUrl(FilmListDownloadType.FULL)
        }
        val oldFilmUrls = prepareLoad()
        runImportAsync(
            operationName = "importFromFile",
            options = FilmListLoadOptions.normal(),
        ) {
            importService.importAdditionalFromFile(sourceUrl, loadNumDays, oldFilmUrls)
        }
    }

    fun addFilmLoadListener(listener: ListenerFilmeLaden) {
        events.addListener(listener)
    }

    fun removeFilmLoadListener(listener: ListenerFilmeLaden) {
        events.removeListener(listener)
    }

    fun setUiHost(host: FilmListLoadHost?) {
        ui.setHost(host)
    }

    val isFilmListImportRunning: Boolean
        get() = importState.isRunning

    private fun beginLoad(): Boolean {
        return importState.tryBegin()
    }

    private fun prepareLoad(): Set<String> {
        val oldFilmUrls = FilmListImportApplier.collectFilmUrls(daten.listeFilme)
        daten.listeFilmeNachBlackList.clear()
        return oldFilmUrls
    }

    private val loadNumDays: Int
        get() = ApplicationConfiguration.getInstance().filmListLoadNumDays

    private fun runImportAsync(
        operationName: String,
        options: FilmListLoadOptions,
        importAction: () -> FilmListImportOutcome,
    ) {
        scope.launch {
            val outcome = try {
                importAction()
            } catch (ex: CancellationException) {
                throw ex
            } catch (ex: Exception) {
                logger.error(operationName, ex)
                FilmListImportOutcome(FilmListImportResult.FAILURE)
            }

            logger.trace("Filme laden, ende")
            if (outcome.result == FilmListImportResult.NO_UPDATE) {
                importState.finish()
                if (options.postProcessWhenNoUpdate) {
                    val statusBarWidgets = ui.attachStatusBarWidgets(ui.currentHost)
                    startPostLoadWork(writeFilmList = false, statusBarWidgets)
                } else {
                    events.notifyFinished(ListenerFilmeLadenEvent("", "", 100, 100, false))
                }
                return@launch
            }
            finishImport(
                ListenerFilmeLadenEvent("", "", 0, 0, outcome.result != FilmListImportResult.SUCCESS),
                options,
                outcome.oldFilmUrls,
                outcome.importedDiffList,
            )
        }
    }

    private suspend fun finishImport(
        event: ListenerFilmeLadenEvent,
        options: FilmListLoadOptions,
        oldFilmUrls: Set<String>,
        diffListe: ListeFilme,
    ) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        logger.debug("finishImport()")
        val listeFilme = daten.listeFilme
        FilmListImportApplier.applyImportedFilms(listeFilme, diffListe, oldFilmUrls)

        val host = ui.currentHost
        importState.finish()
        val writeFilmList = if (event.fehler) {
            logger.info("")
            logger.info("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen")
            ui.showLoadFailedDialog()

            importService.reloadSavedFilmList(listeFilme, loadNumDays)
            logger.info("")

            false
        } else {
            options.writeAfterLoad
        }

        logger.info("")
        logger.info("Jetzige Liste erstellt am: {}", listeFilme.metaData.generationDateTimeAsString)
        logger.info("  Anzahl Filme: {}", listeFilme.size)
        logger.info("  Anzahl Neue:  {}", listeFilme.countNewFilms())
        logger.info("")

        MessageBus.messageBus.publish(FilmListReadStopEvent())
        val statusBarWidgets = ui.attachStatusBarWidgets(host)
        startPostLoadWork(writeFilmList, statusBarWidgets)
    }

    fun completeStartupFilmListLoad(failed: Boolean) {
        events.notifyFinished(ListenerFilmeLadenEvent("", "", 100, 100, failed))
    }

    private fun startPostLoadWork(writeFilmList: Boolean, widgets: FilmListStatusBarWidgets) {
        postLoadRunner.start(writeFilmList, widgets, events::notifyFinished)
    }

    companion object {
        private val logger = LogManager.getLogger(FilmeLaden::class.java)
    }
}
