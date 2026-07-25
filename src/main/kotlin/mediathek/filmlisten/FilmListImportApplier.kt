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

import mediathek.daten.ListeFilme
import org.apache.logging.log4j.LogManager
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneId
import java.time.format.DateTimeFormatter

internal object FilmListImportApplier {
    fun collectFilmUrlKeys(listeFilme: ListeFilme): Set<String> =
        synchronized(listeFilme) {
            HashSet<String>(listeFilme.size + 1, 1f).apply {
                listeFilme.forEach { film -> add(film.storedNormalQualityUrl) }
            }
        }

    fun applyImportedFilms(listeFilme: ListeFilme, diffListe: ListeFilme, oldFilmUrlKeys: Set<String>) {
        val readDate = DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm")
            .format(LocalDateTime.ofInstant(Instant.now(), ZoneId.systemDefault()))

        // wenn nur ein Update
        if (!diffListe.isEmpty()) {
            logger.info("Liste Diff gelesen am: {}", readDate)
            logger.info("  Liste Diff erstellt am: {}", diffListe.metaData.generationDateTimeAsString)
            logFilmCount(diffListe.size)

            listeFilme.updateFromFilmList(diffListe)
            listeFilme.metaData = diffListe.metaData
            listeFilme.sort()
            diffListe.clear()
        } else {
            logger.info("Liste Kompl. gelesen am: {}", readDate)
            logger.info("  Liste Kompl erstellt am: {}", listeFilme.metaData.generationDateTimeAsString)
            logFilmCount(listeFilme.size)
        }

        findAndMarkNewFilms(listeFilme, oldFilmUrlKeys)
    }

    /**
     * Search through history and mark new films.
     */
    private fun findAndMarkNewFilms(listeFilme: ListeFilme, oldFilmUrlKeys: Set<String>) {
        synchronized(listeFilme) {
            listeFilme.forEach { film ->
                film.isNew = film.storedNormalQualityUrl !in oldFilmUrlKeys
            }
        }
    }

    private fun logFilmCount(size: Int) {
        logger.info("  Anzahl Filme: {}", size)
    }

    private val logger = LogManager.getLogger(FilmListImportApplier::class.java)
}
