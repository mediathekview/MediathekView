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

import mediathek.config.StandardLocations
import mediathek.daten.ListeFilme
import mediathek.filmeSuchen.ListenerFilmeLaden
import mediathek.filmeSuchen.ListenerFilmeLadenEvent
import mediathek.filmlisten.reader.FilmListReader
import mediathek.tool.FilmListUpdateType
import mediathek.tool.http.MVHttpClient
import okhttp3.HttpUrl.Companion.toHttpUrl
import okhttp3.Request
import org.apache.logging.log4j.LogManager
import java.io.IOException
import java.net.UnknownHostException

internal enum class FilmListImportResult {
    SUCCESS,
    FAILURE,
    NO_UPDATE,
}

internal data class FilmListImportOutcome(
    val result: FilmListImportResult,
    val oldFilmUrls: Set<String> = emptySet(),
    val importedDiffList: ListeFilme = ListeFilme(),
)

internal interface FilmListImportFeedback {
    fun showNoUpdateAvailable(showDialogs: Boolean)

    fun showExceptionMessage(message: String, ex: Exception, showDialogs: Boolean)
}

internal class FilmListImportService(
    private val feedback: FilmListImportFeedback,
    progressListener: ListenerFilmeLaden,
) {
    private val filmListReader = FilmListReader()

    init {
        filmListReader.addAdListener(progressListener)
    }

    fun importFromUrl(
        dateiUrl: String,
        listeFilme: ListeFilme,
        days: Int,
        immerNeuLaden: Boolean,
        prepareImport: () -> Set<String>,
    ): FilmListImportOutcome {
        if (!performUpdateCheck(listeFilme, dateiUrl)) {
            return FilmListImportOutcome(FilmListImportResult.NO_UPDATE)
        }

        val oldFilmUrls = prepareImport()
        if (immerNeuLaden) {
            // Preserve existing behavior: clear only after capturing old URLs for new-film marking.
            listeFilme.clear()
        }

        val diffList = ListeFilme()
        return FilmListImportOutcome(
            result = importFromUrlSynchronously(listeFilme, diffList, days).toImportResult(),
            oldFilmUrls = oldFilmUrls,
            importedDiffList = diffList,
        )
    }

    fun importFromFile(
        pfad: String,
        listeFilme: ListeFilme,
        days: Int,
        prepareImport: () -> Set<String>,
    ): FilmListImportOutcome {
        if (!performUpdateCheck(listeFilme, pfad)) {
            return FilmListImportOutcome(FilmListImportResult.NO_UPDATE)
        }

        val oldFilmUrls = prepareImport()
        listeFilme.clear()
        return FilmListImportOutcome(
            result = urlLaden(pfad, listeFilme, days).toImportResult(),
            oldFilmUrls = oldFilmUrls,
        )
    }

    fun importAdditionalFromFile(
        pfad: String,
        days: Int,
        oldFilmUrls: Set<String>,
    ): FilmListImportOutcome {
        val importedList = ListeFilme()
        return FilmListImportOutcome(
            result = urlLaden(pfad, importedList, days).toImportResult(),
            oldFilmUrls = oldFilmUrls,
            importedDiffList = importedList,
        )
    }

    fun reloadSavedFilmList(listeFilme: ListeFilme, days: Int) {
        listeFilme.clear()
        FilmListReader().use { reader ->
            reader.readFilmListe(StandardLocations.getFilmlistFilePathString(), listeFilme, days)
        }
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

    private fun hasNewRemoteFilmlist(sourceUrl: String): Boolean {
        var result = false
        logger.trace("hasNewRemoteFilmList()")
        val showDialogs = FilmListUpdateType.fromConfig() != FilmListUpdateType.AUTOMATIC

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
                    feedback.showNoUpdateAvailable(showDialogs)
                }
            }
        } catch (ex: UnknownHostException) {
            logger.debug(ex)
            feedback.showExceptionMessage(NETWORK_NOT_AVAILABLE, ex, showDialogs)
            if (!showDialogs) {
                logger.warn(NETWORK_NOT_AVAILABLE)
            }
        } catch (ex: IOException) {
            logger.error("IOxception:", ex)
            feedback.showExceptionMessage("Netzwerkfehler aufgetreten!", ex, true)
        } catch (ex: Exception) {
            logger.error("Filmlist update check failed", ex)
            feedback.showExceptionMessage("Ein unbekannter Fehler ist aufgetreten.", ex, showDialogs)
        }

        return result
    }

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

    private fun Boolean.toImportResult(): FilmListImportResult =
        if (this) FilmListImportResult.SUCCESS else FilmListImportResult.FAILURE

    private companion object {
        private val logger = LogManager.getLogger(FilmListImportService::class.java)
        private const val NETWORK_NOT_AVAILABLE = "Netzwerk nicht verfügbar"
        private const val HTTP_NOT_FOUND = 404
    }
}
