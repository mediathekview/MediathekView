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

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import org.apache.logging.log4j.LogManager
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.*

internal object FilmListImportApplier {
    fun collectFilmUrls(listeFilme: ListeFilme): Set<String> =
        listeFilme.parallelStream()
            .map { film -> film.urlNormalQuality }
            .toList()
            .toHashSet()

    fun applyImportedFilms(listeFilme: ListeFilme, diffListe: ListeFilme, oldFilmUrls: Set<String>) {
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

        findAndMarkNewFilms(listeFilme, oldFilmUrls)
    }

    /**
     * Search through history and mark new films.
     */
    private fun findAndMarkNewFilms(listeFilme: ListeFilme, oldFilmUrls: Set<String>) {
        // reset all current new films to false
        listeFilme.parallelStream()
            .filter(DatenFilm::isNew)
            .forEach { film -> film.isNew = false }
        // mark new entries
        listeFilme.parallelStream()
            .filter { film -> film.urlNormalQuality !in oldFilmUrls }
            .forEach { film -> film.isNew = true }
    }

    private val logger = LogManager.getLogger(FilmListImportApplier::class.java)
}
