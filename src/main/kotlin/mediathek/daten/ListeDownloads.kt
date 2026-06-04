/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten

import mediathek.config.CommandLineOptions
import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.config.MVConfig
import mediathek.controller.starter.DownloadLifecycleActions
import mediathek.controller.starter.DownloadStartActions
import mediathek.controller.starter.StartStatus
import mediathek.gui.dialog.MissingProgramSetDialog
import mediathek.gui.messages.ButtonStartEvent
import mediathek.gui.messages.DownloadListChangedEvent
import mediathek.gui.messages.DownloadQueueRankChangedEvent
import mediathek.gui.messages.StartEvent
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.MessageBus
import mediathek.tool.datum.DateUtil
import mediathek.tool.models.TModelDownload
import org.apache.logging.log4j.LogManager
import java.time.LocalDate
import java.util.LinkedList
import java.util.function.Predicate
import javax.swing.JFrame

class ListeDownloads : LinkedList<DatenDownload>() {
    @Synchronized
    fun addMitNummer(download: DatenDownload) {
        add(download)
        listeNummerieren()
    }

    @Synchronized
    fun filmEintragen() {
        // bei einmal Downloads nach einem Programmstart/Neuladen der Filmliste
        // den Film wieder eintragen
        logger.info("Filme in Downloads eintragen")
        val listeFilme = Daten.getInstance().listeFilme
        filter { download -> download.film == null }
            .forEach { download ->
                download.film = listeFilme.getFilmByUrl_klein_hoch_hd(download.downloadUrl)
                download.setGroesse("")
            }
    }

    @Synchronized
    fun listePutzen() {
        // fertige Downloads löschen
        // fehlerhafte zurücksetzen
        var found = false
        val iterator = iterator()
        while (iterator.hasNext()) {
            val download = iterator.next()
            when (download.runtime.runState?.status) {
                StartStatus.FINISHED -> {
                    // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                    iterator.remove()
                    found = true
                }

                StartStatus.ERROR -> {
                    // fehlerhafte werden zurückgesetzt
                    DownloadLifecycleActions.reset(download)
                    found = true
                }

                else -> Unit
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
        }
    }

    @Synchronized
    fun listePutzen(datenDownload: DatenDownload) {
        // fertigen Download löschen
        var found = false
        when (datenDownload.runtime.runState?.status) {
            StartStatus.FINISHED -> {
                // alles was fertig/fehlerhaft ist, kommt beim putzen weg
                remove(datenDownload)
                found = true
            }

            StartStatus.ERROR -> {
                // fehlerhafte werden zurückgesetzt
                DownloadLifecycleActions.reset(datenDownload)
                found = true
            }

            else -> Unit
        }
        if (found) {
            MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
        }
    }

    @Synchronized
    fun abosAuffrischen() {
        // fehlerhafte und nicht gestartete löschen, wird nicht gemeldet ob was gefunden wurde
        val iterator = iterator()
        while (iterator.hasNext()) {
            val download = iterator.next()
            if (download.isInterrupted) {
                // guter Rat teuer was da besser wäre??
                // wird auch nach dem Neuladen der Filmliste aufgerufen: also Finger weg
                download.setGroesseFromFilm() // bei den Abgebrochenen wird die tatsächliche Dateigröße angezeigt
                continue
            }
            if (!download.isFromAbo) {
                continue
            }
            when (download.runtime.runState?.status) {
                null -> iterator.remove() // noch nicht gestartet
                StartStatus.ERROR -> DownloadLifecycleActions.reset(download) // fehlerhafte
                else -> Unit
            }
        }

        forEach { download ->
            DownloadLifecycleActions.clearDeferred(download)
        }
    }

    /**
     * Get the number of unfinished download tasks.
     *
     * @return number of unfinished tasks
     */
    @Synchronized
    fun unfinishedDownloads(): Long = count { download -> download.runNotFinished() }.toLong()

    @Synchronized
    fun requestStopForShutdown() {
        for (download in this) {
            download.runtime.runState?.requestStop()
        }
    }

    @Synchronized
    fun downloadsVorziehen(downloads: ArrayList<DatenDownload>) {
        for (download in downloads) {
            remove(download)
            addFirst(download)
        }

        MessageBus.messageBus.publishAsync(DownloadQueueRankChangedEvent())
    }

    @Synchronized
    fun delDownloadButton(url: String) {
        for (download in this) {
            if (download.downloadUrl == url) {
                val state = download.runtime.runState
                if (state?.isBeforeFinished == true) {
                    state.requestStop()
                }
                DownloadLifecycleActions.reset(download)
                MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
                break
            }
        }
    }

    @Synchronized
    fun downloadAbbrechen(downloads: ArrayList<DatenDownload>?) {
        var found = false
        if (downloads != null) {
            for (download in downloads) {
                if (contains(download)) {
                    // nur dann ist er in der Liste
                    download.runtime.runState?.let { state ->
                        if (state.isBeforeFinished) {
                            state.requestStop()
                        }
                        if (state.isRunning) {
                            DownloadLifecycleActions.markInterrupted(download)
                        }
                    }
                    DownloadLifecycleActions.reset(download)
                    found = true
                }
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(StartEvent())
        }
    }

    @Synchronized
    fun downloadLoeschen(downloads: ArrayList<DatenDownload>?) {
        var found = false
        if (downloads != null) {
            for (download in downloads) {
                val state = download.runtime.runState
                if (state?.isBeforeFinished == true) {
                    state.requestStop()
                }
                if (remove(download)) {
                    found = true
                }
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(DownloadListChangedEvent())
        }
    }

    @Synchronized
    fun getDownloadUrlFilm(urlFilm: String): DatenDownload? =
        firstOrNull { download -> download.filmUrl == urlFilm }

    @Synchronized
    fun getModel(tModel: TModelDownload, filter: DownloadListFilter) {
        DownloadTableModelUpdater.reload(tModel, this, filter)
    }

    @Synchronized
    fun setModelProgress(tModel: TModelDownload) {
        DownloadTableModelUpdater.updateProgress(tModel)
    }

    @Synchronized
    fun abosSuchen(parent: JFrame?): List<DatenDownload> {
        // in der Filmliste nach passenden Filmen suchen und
        // in die Liste der Downloads eintragen
        val downloadUrls = HashSet<String>()
        val addedDownloads = mutableListOf<DatenDownload>()
        // mit den bereits enthaltenen URL füllen
        forEach { download -> downloadUrls.add(download.downloadUrl) }

        // prüfen ob in "alle Filme" oder nur "nach Blacklist" gesucht werden soll
        val checkWithBlackList = MVConfig.get(MVConfig.Configs.SYSTEM_BLACKLIST_AUCH_ABO).toBoolean()
        val defaultPset = Daten.getInstance().listePset.getPsetAbo("")
        val today = LocalDate.now(DateUtil.MV_DEFAULT_TIMEZONE)

        val daten = Daten.getInstance()
        val listeAbo = daten.listeAbo
        val listeBlacklist = daten.listeBlacklist
        val aboHistoryController = daten.aboHistoryController
        val listeFilme = daten.listeFilme
        val blacklistFilter: Predicate<DatenFilm> = if (checkWithBlackList) {
            listeBlacklist.createDownloadsPredicate()
        } else {
            Predicate { true }
        }

        for (film in listeFilme) {
            val abo = listeAbo.getAboFuerFilm_schnell(film, true) ?: continue
            if (!abo.isActive) {
                continue
            }
            if (checkWithBlackList && !blacklistFilter.test(film)) {
                // Blacklist auch bei Abos anwenden
                continue
            }
            if (aboHistoryController.urlExists(film.urlNormalQuality)) {
                // ist schon mal geladen worden
                continue
            }

            val pset = if (abo.psetName.isEmpty()) defaultPset else Daten.getInstance().listePset.getPsetAbo(abo.psetName)
            if (pset != null) {
                // mit der tatsächlichen URL prüfen, ob die URL schon in der Downloadliste ist
                val downloadUrl = film.getUrlFuerAufloesung(pset.aufloesung)
                if (!downloadUrls.add(downloadUrl)) {
                    continue
                }

                // diesen Film in die Downloadliste eintragen
                abo.downDatum = today
                if (abo.psetName != pset.name) {
                    // nur den Namen anpassen, falls geändert
                    abo.psetName = pset.name
                }

                // dann in die Liste schreiben
                val download = DatenDownload(pset, film, DownloadSource.ABO, abo, "", "", "")
                add(download)
                addedDownloads.add(download)
            } else {
                if (parent == null || CommandLineOptions.isDownloadAndQuit()) {
                    throw IllegalStateException("Kein Programmset für Abo \"${abo.name}\" konfiguriert.")
                }
                MissingProgramSetDialog.showMissingAboProgramSet(parent)
                break
            }
        }

        if (addedDownloads.isNotEmpty()) {
            listeNummerieren()
        }
        return addedDownloads
    }

    @Synchronized
    fun listeNummerieren() {
        var index = 1
        for (download in this) {
            download.nr = index++
        }
    }

    @get:Synchronized
    val starts: DownloadStartInfo
        get() {
            val info = DownloadStartInfo()
            info.total_num_download_list_entries = size

            for (download in this) {
                if (!download.isDeferred) {
                    info.total_starts++
                }
                if (download.isFromAbo) {
                    info.num_abos++
                } else {
                    info.num_downloads++
                }
                val state = download.runtime.runState
                if (
                    state != null &&
                    (download.quelle == DownloadSource.ABO || download.quelle == DownloadSource.DOWNLOAD)
                ) {
                    when (state.status) {
                        StartStatus.INITIALIZED -> info.initialized++
                        StartStatus.RUNNING -> info.running++
                        StartStatus.FINISHED -> info.finished++
                        StartStatus.ERROR -> info.error++
                    }
                }
            }

            return info
        }

    /**
     * Return a List of all not yet finished downloads.
     *
     * @param quelle the download source to include
     * @return A list with all download objects.
     */
    @Synchronized
    fun getListOfStartsNotFinished(quelle: DownloadSource): List<DatenDownload> =
        filter { download ->
            download.runtime.runState?.isBeforeFinished == true &&
                (quelle == DownloadSource.ALL || download.quelle == quelle)
        }

    @Synchronized
    fun buttonStartsPutzen() {
        // Starts durch Button die fertig sind, löschen
        var found = false
        val iterator = iterator()
        while (iterator.hasNext()) {
            val download = iterator.next()
            if (download.runtime.runState?.isAtLeastFinished == true && download.quelle == DownloadSource.BUTTON) {
                // dann ist er fertig oder abgebrochen
                iterator.remove()
                found = true
            }
        }
        if (found) {
            MessageBus.messageBus.publishAsync(ButtonStartEvent())
        }
    }

    @get:Synchronized
    val nextStart: DatenDownload?
        get() {
            // get: erstes passendes Element der Liste zurückgeben oder null
            // und versuchen dass bei mehreren laufenden Downloads ein anderer Sender gesucht wird
            val maxNumDownloads = ApplicationConfiguration.getConfiguration()
                .getInt(ApplicationConfiguration.DOWNLOAD_MAX_SIMULTANEOUS_NUM, 1)
            if (isNotEmpty() && canStartMore(maxNumDownloads)) {
                return nextPossibleDownload()
            }

            return null
        }

    @get:Synchronized
    val restartDownload: DatenDownload?
        get() {
            // Versuch einen Fehlgeschlagenen Download zu finden um ihn wieder zu starten
            // die Fehler laufen aber einzeln, vorsichtshalber
            if (!canStartMore(1)) {
                return null
            }
            for (download in this) {
                val state = download.runtime.runState ?: continue

                if (state.status == StartStatus.ERROR && state.countRestarted < Konstanten.MAX_DOWNLOAD_RESTARTS) {
                    val restarted = state.countRestarted
                    if (download.art == DownloadType.DIRECT) {
                        DownloadLifecycleActions.reset(download)
                        DownloadStartActions.start(download)
                        download.runtime.runState?.countRestarted = restarted + 1
                        return download
                    }
                }
            }
            return null
        }

    private fun canStartMore(max: Int): Boolean {
        var count = 0
        for (download in this) {
            if (download.runtime.runState?.isRunning == true) {
                ++count
                if (count >= max) {
                    return false
                }
            }
        }
        return true
    }

    private fun nextPossibleDownload(): DatenDownload? =
        firstOrNull { download -> download.runtime.runState?.status == StartStatus.INITIALIZED }

    companion object {
        private val logger = LogManager.getLogger(ListeDownloads::class.java)
    }
}
