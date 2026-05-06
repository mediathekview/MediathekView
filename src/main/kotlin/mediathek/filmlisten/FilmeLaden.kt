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
import mediathek.config.Config
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.StandardLocations
import mediathek.daten.DatenFilm
import mediathek.daten.IndexedFilmList
import mediathek.daten.ListeFilme
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.filmlisten.reader.FilmListReader
import mediathek.gui.duplicates.CommonStatsEvaluationTask
import mediathek.gui.duplicates.FilmDuplicateEvaluationTask
import mediathek.gui.messages.FilmListReadStopEvent
import mediathek.gui.tasks.BlacklistFilterWorker
import mediathek.gui.tasks.FilmlistWriterWorker
import mediathek.gui.tasks.LuceneIndexWorker
import mediathek.gui.tasks.RefreshAboWorker
import mediathek.mainwindow.MediathekGui
import mediathek.tool.*
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import java.awt.GraphicsEnvironment
import java.io.IOException
import java.lang.reflect.InvocationTargetException
import java.net.UnknownHostException
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.*
import javax.swing.JLabel
import javax.swing.JOptionPane
import javax.swing.JProgressBar
import javax.swing.SwingUtilities
import javax.swing.event.EventListenerList

class FilmeLaden(private val daten: Daten) {
    private data class StatusBarWidgets(val label: JLabel, val progressBar: JProgressBar)

    private enum class ImportResult {
        SUCCESS,
        FAILURE,
        NO_UPDATE,
    }

    private val hashSet = HashSet<String>()
    private val diffListe = ListeFilme()
    private val filmListReader = FilmListReader()
    private val listeners = EventListenerList()
    private val scope = CoroutineScope(SupervisorJob() + Dispatchers.IO)

    @Volatile
    private var istAmLaufen = false
    private var onlyOne = false

    private val canShowUiDialogs: Boolean
        get() = MediathekGui.ui() != null && !Config.isDownloadAndQuit() && !GraphicsEnvironment.isHeadless()

    init {
        filmListReader.addAdListener(object : ListenerFilmeLaden() {
            @Synchronized
            override fun start(event: ListenerFilmeLadenEvent) {
                notifyStart(event)
            }

            @Synchronized
            override fun progress(event: ListenerFilmeLadenEvent) {
                notifyProgress(event)
            }

            @Suppress("UNUSED_PARAMETER")
            @Synchronized
            override fun fertig(event: ListenerFilmeLadenEvent) {
                // handled by the async import methods below
            }
        })
    }

    private fun showNoUpdateAvailableDialog() {
        val ui = MediathekGui.ui()
        if (canShowUiDialogs && ui != null) {
            SwingUtilities.invokeLater {
                JOptionPane.showMessageDialog(
                    ui,
                    NO_UPDATE_AVAILABLE,
                    Konstanten.PROGRAMMNAME,
                    JOptionPane.INFORMATION_MESSAGE,
                )
            }
        } else {
            logger.info(NO_UPDATE_AVAILABLE)
        }
    }

    private fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean) {
        val ui = MediathekGui.ui()
        if (showDialogs && canShowUiDialogs && ui != null) {
            SwingUtilities.invokeLater {
                SwingErrorDialog.showExceptionMessage(ui, message, ex)
            }
        }
    }

    private fun hasNewRemoteFilmlist(sourceUrl: String): Boolean {
        var result = false
        logger.trace("hasNewRemoteFilmList()")
        val showDialogs = GuiFunktionen.getFilmListUpdateType() != FilmListUpdateType.AUTOMATIC

        val filmListUrl = sourceUrl.toHttpUrl()
        val storedEtag = FilmListMetadataStore.readEtag(sourceUrl)
        val requestBuilder = Request.Builder()
            .url(filmListUrl)
            .head()
        if (!storedEtag.isNullOrBlank()) {
            requestBuilder.header("If-None-Match", storedEtag)
        }

        val request = requestBuilder.build()
        try {
            MVHttpClient.httpClient.newCall(request).execute().use { response ->
                response.body.close()
                result = when {
                    response.code == 304 -> false
                    response.isSuccessful -> {
                        val remoteEtag = response.header("ETag")
                        storedEtag.isNullOrBlank() || storedEtag != remoteEtag
                    }
                    else -> {
                        logger.warn(
                            "hasNewRemoteFilmlist HTTP Response Code: {} for {}",
                            response.code,
                            response.request.url,
                        )
                        response.code == HTTP_NOT_FOUND || response.code == 405
                    }
                }

                if (!result) {
                    if (showDialogs) {
                        showNoUpdateAvailableDialog()
                    } else {
                        logger.info(NO_UPDATE_AVAILABLE)
                    }
                }
            }
        } catch (ex: UnknownHostException) {
            logger.debug(ex)
            showExceptionMessage(NETWORK_NOT_AVAILABLE, ex, showDialogs)
            if (!showDialogs) {
                logger.warn(NETWORK_NOT_AVAILABLE)
            }
        } catch (ex: IOException) {
            logger.error("IOxception:", ex)
            showExceptionMessage("Netzwerkfehler aufgetreten!", ex, true)
        } catch (ex: Exception) {
            logger.error("Filmlist update check failed", ex)
            showExceptionMessage("Ein unbekannter Fehler ist aufgetreten.", ex, showDialogs)
        }

        return result
    }

    /**
     * Determine whether we want to perform a remote update check.
     * This will be done if:
     * 1. don´t have film entries
     * 2. dateiUrl is either empty or string starts with http
     * 3. our filmlist is old enough that we dont use diff list - we dont check them.
     *
     * @return true if we need to load a new list, false if we should not load a remote list
     */
    private fun performUpdateCheck(listeFilme: ListeFilme, dateiUrl: String): Boolean {
        if (listeFilme.isEmpty()) {
            return true
        }

        // remote download is using an empty file name!...
        // or somebody put a web adress into the text field
        if (dateiUrl.isEmpty() || dateiUrl.startsWith("http")) {
            val remoteSource = dateiUrl.ifEmpty {
                StandardLocations.getFilmListUrl(
                    if (listeFilme.metaData.canUseDiffList()) FilmListDownloadType.DIFF_ONLY else FilmListDownloadType.FULL,
                )
            }
            return hasNewRemoteFilmlist(remoteSource)
        }

        return true
    }

    private fun prepareHashTable() {
        hashSet.clear()
        fillHash(daten.listeFilme)
    }

    private fun displayLogInfo(listeFilme: ListeFilme) {
        logger.info("Alte Liste erstellt am: {}", listeFilme.metaData.generationDateTimeAsString)
        logger.info("  Anzahl Filme: {}", listeFilme.size)
        logger.info("  Anzahl Neue: {}", listeFilme.countNewFilms())
    }

    fun loadFilmlist(dateiUrl: String, immerNeuLaden: Boolean): Boolean =
        loadFilmlist(dateiUrl, immerNeuLaden, FilmListLoadOptions.normal())

    fun loadFilmlist(dateiUrl: String, immerNeuLaden: Boolean, options: FilmListLoadOptions): Boolean {
        // damit wird die Filmliste geladen UND auch gleich im Konfig-Ordner gespeichert
        val loadOptions = requireNotNull(options)
        val listeFilme = daten.listeFilme

        logger.trace("loadFilmlist(String,boolean,FilmListLoadOptions)")
        logger.info("")
        displayLogInfo(listeFilme)

        if (!canStartLoad()) {
            return true
        }

        markLoadRunning()

        val days = loadNumDays
        if (dateiUrl.isEmpty()) {
            logger.info("Filmliste laden (Netzwerk)")
            importFromUrl(dateiUrl, listeFilme, diffListe, days, immerNeuLaden, loadOptions)
        } else {
            logger.info("Filmliste laden von: {}", dateiUrl)
            importFromFile(dateiUrl, listeFilme, days, immerNeuLaden, loadOptions)
        }
        return true
    }

    fun updateFilmlist(dateiUrl: String) {
        // damit wird die Filmliste mit einer weiteren aktualisiert (die bestehende bleibt
        // erhalten) UND auch gleich im Konfig-Ordner gespeichert
        logger.debug("Filme laden (Update), start")
        logger.info("")
        displayLogInfo(daten.listeFilme)

        if (!canStartLoad()) {
            return
        }

        beginLoad()

        logger.info("Filmliste laden von: {}", dateiUrl)
        val sourceUrl = dateiUrl.ifEmpty {
            StandardLocations.getFilmListUrl(FilmListDownloadType.FULL)
        }
        importFromFile(sourceUrl, diffListe, loadNumDays, FilmListLoadOptions.normal())
    }

    fun addAdListener(listener: ListenerFilmeLaden) {
        listeners.add(ListenerFilmeLaden::class.java, listener)
    }

    fun removeAdListener(listener: ListenerFilmeLaden) {
        listeners.remove(ListenerFilmeLaden::class.java, listener)
    }

    private fun canStartLoad(): Boolean = !istAmLaufen

    private fun markLoadRunning() {
        istAmLaufen = true
    }

    private fun beginLoad() {
        markLoadRunning()
        prepareLoad()
    }

    private fun prepareLoad() {
        prepareHashTable()
        daten.listeFilmeNachBlackList.clear()
    }

    private val loadNumDays: Int
        get() = ApplicationConfiguration.getConfiguration().getInt(ApplicationConfiguration.FilmList.LOAD_NUM_DAYS, 0)

    private fun importFromUrl(
        dateiUrl: String,
        listeFilme: ListeFilme,
        listeFilmeDiff: ListeFilme,
        days: Int,
        immerNeuLaden: Boolean,
        options: FilmListLoadOptions,
    ) {
        runImportAsync(
            importAction = {
                if (!performUpdateCheck(listeFilme, dateiUrl)) {
                    return@runImportAsync ImportResult.NO_UPDATE
                }
                prepareLoad()
                if (immerNeuLaden) {
                    // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
                    listeFilme.clear() // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
                }
                importFromUrlSynchronously(listeFilme, listeFilmeDiff, days).toImportResult()
            },
            operationName = "importFromUrl",
            options = options,
        )
    }

    private fun importFromFile(
        pfad: String,
        listeFilme: ListeFilme,
        days: Int,
        immerNeuLaden: Boolean,
        options: FilmListLoadOptions,
    ) {
        runImportAsync(
            importAction = {
                if (!performUpdateCheck(listeFilme, pfad)) {
                    return@runImportAsync ImportResult.NO_UPDATE
                }
                prepareLoad()
                if (immerNeuLaden) {
                    // dann die alte löschen, damit immer komplett geladen wird, aber erst nach dem Hash!!
                    listeFilme.clear() // sonst wird eine "zu kurze" Liste wieder nur mit einer Diff-Liste aufgefüllt, wenn das Alter noch passt
                }
                listeFilme.clear()
                urlLaden(pfad, listeFilme, days).toImportResult()
            },
            operationName = "importFromFile",
            options = options,
        )
    }

    private fun importFromFile(pfad: String, listeFilme: ListeFilme, days: Int, options: FilmListLoadOptions) {
        runImportAsync(
            importAction = { urlLaden(pfad, listeFilme, days).toImportResult() },
            operationName = "importFromFile",
            options = options,
        )
    }

    private fun Boolean.toImportResult(): ImportResult = if (this) ImportResult.SUCCESS else ImportResult.FAILURE

    private fun importFromUrlSynchronously(listeFilme: ListeFilme, listeFilmeDiff: ListeFilme, days: Int): Boolean {
        if (listeFilme.isEmpty() || !listeFilme.metaData.canUseDiffList()) {
            return ladeKompletteListe(listeFilme, days)
        }

        if (ladeDiffListe(listeFilmeDiff, days)) {
            return true
        }

        listeFilmeDiff.clear()
        return ladeKompletteListe(listeFilme, days)
    }

    private fun ladeKompletteListe(listeFilme: ListeFilme, days: Int): Boolean {
        listeFilme.clear()
        return urlLaden(StandardLocations.getFilmListUrl(FilmListDownloadType.FULL), listeFilme, days)
    }

    private fun ladeDiffListe(listeFilmeDiff: ListeFilme, days: Int): Boolean =
        urlLaden(StandardLocations.getFilmListUrl(FilmListDownloadType.DIFF_ONLY), listeFilmeDiff, days) &&
            !listeFilmeDiff.isEmpty()

    private fun urlLaden(dateiUrl: String, listeFilme: ListeFilme, days: Int): Boolean {
        var ret = false
        try {
            if (dateiUrl.isNotEmpty()) {
                logger.trace("Filmliste laden von: {}", dateiUrl)
                filmListReader.readFilmListe(dateiUrl, listeFilme, days)
                if (!listeFilme.isEmpty()) {
                    ret = true
                }
            }
        } catch (ex: Exception) {
            logger.error("urlLaden", ex)
        }
        return ret
    }

    private fun runImportAsync(importAction: () -> ImportResult, operationName: String, options: FilmListLoadOptions) {
        scope.launch {
            val result = try {
                importAction()
            } catch (throwable: Throwable) {
                logger.error(operationName, throwable)
                ImportResult.FAILURE
            }

            logger.trace("Filme laden, ende")
            if (result == ImportResult.NO_UPDATE) {
                istAmLaufen = false
                notifyFertig(ListenerFilmeLadenEvent("", "", 100, 100, false))
                return@launch
            }
            undEnde(ListenerFilmeLadenEvent("", "", 0, 0, result != ImportResult.SUCCESS), options)
        }
    }

    private fun undEnde(event: ListenerFilmeLadenEvent, options: FilmListLoadOptions) {
        // Abos eintragen in der gesamten Liste vor Blacklist da das nur beim Ändern der Filmliste oder
        // beim Ändern von Abos gemacht wird

        logger.debug("undEnde()")
        val listeFilme = daten.listeFilme
        val readDate = DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm")
            .format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()))

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            logger.info("Liste Diff gelesen am: {}", readDate)
            logger.info("  Liste Diff erstellt am: {}", diffListe.metaData.generationDateTimeAsString)
            logger.info("  Anzahl Filme: {}", diffListe.size)

            listeFilme.updateFromFilmList(diffListe)
            listeFilme.metaData = diffListe.metaData
            Collections.sort(listeFilme)
            diffListe.clear()
        } else {
            logger.info("Liste Kompl. gelesen am: {}", readDate)
            logger.info("  Liste Kompl erstellt am: {}", listeFilme.metaData.generationDateTimeAsString)
            logger.info("  Anzahl Filme: {}", listeFilme.size)
        }

        findAndMarkNewFilms(daten.listeFilme)

        val ui = MediathekGui.ui()
        istAmLaufen = false
        val writeFilmList = if (event.fehler) {
            logger.info("")
            logger.info("Filmliste laden war fehlerhaft, alte Liste wird wieder geladen")
            if (canShowUiDialogs && ui != null) {
                SwingUtilities.invokeLater {
                    JOptionPane.showMessageDialog(
                        ui,
                        "Das Laden der Filmliste hat nicht geklappt!",
                        Konstanten.PROGRAMMNAME,
                        JOptionPane.ERROR_MESSAGE,
                    )
                }
            }

            // dann die alte Liste wieder laden
            listeFilme.clear()

            FilmListReader().use { reader ->
                reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), listeFilme, loadNumDays)
            }
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

        MessageBus.messageBus.publishAsync(FilmListReadStopEvent())
        val statusBarWidgets = attachStatusBarWidgets(ui)
        startPostLoadWork(writeFilmList, statusBarWidgets, ui)
    }

    private fun fillHash(listeFilme: ListeFilme) {
        hashSet.addAll(listeFilme.parallelStream().map { it.urlNormalQuality }.toList())
    }

    /**
     * Search through history and mark new films.
     */
    private fun findAndMarkNewFilms(listeFilme: ListeFilme) {
        // reset all current new films to false
        listeFilme.parallelStream()
            .filter(DatenFilm::isNew)
            .forEach { film -> film.isNew = false }
        // mark new entries
        listeFilme.parallelStream()
            .filter { film -> !hashSet.contains(film.urlNormalQuality) }
            .forEach { film -> film.isNew = true }

        hashSet.clear()
    }

    fun notifyStart(event: ListenerFilmeLadenEvent) {
        try {
            notifyListenersAsync { listener -> listener.start(event) }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    fun notifyProgress(event: ListenerFilmeLadenEvent) {
        try {
            notifyListenersAsync { listener -> listener.progress(event) }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    fun notifyFertig(event: ListenerFilmeLadenEvent) {
        try {
            notifyListenersAsync { listener -> listener.fertig(event) }

            if (!onlyOne) {
                onlyOne = true
                notifyListenersAsync { listener -> listener.fertigOnlyOne(event) }
            }
        } catch (ex: Exception) {
            logger.error(ex)
        }
    }

    private fun attachStatusBarWidgets(ui: MediathekGui?): StatusBarWidgets {
        if (ui == null) {
            return StatusBarWidgets(JLabel(), JProgressBar())
        }
        val widgets = StatusBarWidgets(ui.progressLabel, ui.progressBar)
        invokeOnEdtAndWait {
            ui.swingStatusBar.add(widgets.label)
            ui.swingStatusBar.add(widgets.progressBar)
        }
        return widgets
    }

    private fun detachStatusBarWidgets(ui: MediathekGui?, widgets: StatusBarWidgets) {
        if (ui == null) {
            return
        }
        invokeOnEdtAndWait {
            ui.swingStatusBar.remove(widgets.progressBar)
            ui.swingStatusBar.remove(widgets.label)
        }
    }

    private fun startPostLoadWork(writeFilmList: Boolean, widgets: StatusBarWidgets, ui: MediathekGui?) {
        scope.launch {
            buildPostLoadWorkerChain(writeFilmList, widgets)
            SwingUtilities.invokeLater {
                Daten.getInstance().filmeLaden.notifyFertig(ListenerFilmeLadenEvent("", "", 100, 100, false))
            }
            detachStatusBarWidgets(ui, widgets)
        }
    }

    private fun buildPostLoadWorkerChain(writeFilmList: Boolean, widgets: StatusBarWidgets) {
        RefreshAboWorker(widgets.label, widgets.progressBar).run()
        BlacklistFilterWorker(widgets.label, widgets.progressBar).run()

        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FILM_EVALUATE_DUPLICATES, true)) {
            FilmDuplicateEvaluationTask().run()
        }

        CommonStatsEvaluationTask().run()

        if (writeFilmList) {
            FilmlistWriterWorker(widgets.label, widgets.progressBar).run()
        }
        if (daten.listeFilmeNachBlackList is IndexedFilmList) {
            LuceneIndexWorker(widgets.label, widgets.progressBar).run()
        }
    }

    private fun invokeOnEdtAndWait(action: () -> Unit) {
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                action()
            } else {
                SwingUtilities.invokeAndWait(action)
            }
        } catch (ex: InterruptedException) {
            Thread.currentThread().interrupt()
            throw RuntimeException(ex)
        } catch (ex: InvocationTargetException) {
            throw RuntimeException(ex)
        }
    }

    private fun notifyListenersAsync(action: (ListenerFilmeLaden) -> Unit) {
        val currentListeners = listeners.getListeners(ListenerFilmeLaden::class.java)
        val runnable = Runnable {
            currentListeners.forEach { listener -> action(listener) }
        }
        if (Config.isDownloadAndQuit() || GraphicsEnvironment.isHeadless()) {
            runnable.run()
        } else {
            SwingUtilities.invokeLater(runnable)
        }
    }

    companion object {
        private val logger = LogManager.getLogger(FilmeLaden::class.java)
        private const val NETWORK_NOT_AVAILABLE = "Netzwerk nicht verfügbar"
        private const val NO_UPDATE_AVAILABLE = "Es ist keine aktuellere Filmliste verfügbar."
        private const val HTTP_NOT_FOUND = 404
    }
}
