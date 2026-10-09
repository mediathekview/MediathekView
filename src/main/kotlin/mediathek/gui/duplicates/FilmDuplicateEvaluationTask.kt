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

package mediathek.gui.duplicates

import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.filmlisten.FilmCatalog
import org.apache.logging.log4j.LogManager

class FilmDuplicateEvaluationTask(
    private val filmCatalog: FilmCatalog,
) : Runnable {
    private val listeFilme: ListeFilme = filmCatalog.allFilms
    private val duplicateComparator = BigSenderPenaltyComparator()

    private data class DuplicateUrlKey(
        val normalQualityUrl: String,
        val highQualityUrl: String,
        val lowQualityUrl: String,
    )

    private fun printDuplicateStatistics(statisticsMap: Map<String, Long>) {
        val duplicateCount = statisticsMap.values.sum()

        replaceFilmStatistics(filmCatalog.duplicateStatistics, statisticsMap)

        logger.trace("Number of duplicates: {}", duplicateCount)
    }

    private fun checkDuplicates(): Map<String, Long> {
        logger.trace("Start Duplicate URL search")
        val winnersByUrl = HashMap<DuplicateUrlKey, DatenFilm>()
        val duplicateStatistics = HashMap<String, Long>()

        for (film in listeFilme) {
            if (film.isLivestream) {
                continue
            }

            film.isDuplicate = false
            val key = duplicateKey(film)
            val winner = winnersByUrl[key]
            if (winner == null) {
                winnersByUrl[key] = film
            } else if (duplicateComparator.compare(film, winner) < 0) {
                markDuplicate(winner, duplicateStatistics)
                winnersByUrl[key] = film
            } else {
                markDuplicate(film, duplicateStatistics)
            }
        }

        return duplicateStatistics
    }

    private fun duplicateKey(film: DatenFilm): DuplicateUrlKey =
        DuplicateUrlKey(
            normalQualityUrl = film.urlNormalQuality,
            highQualityUrl = if (film.isHighQuality) film.highQualityUrl else "",
            lowQualityUrl = if (film.hasLowQuality()) film.lowQualityUrl else "",
        )

    private fun markDuplicate(film: DatenFilm, duplicateStatistics: MutableMap<String, Long>) {
        film.isDuplicate = true
        duplicateStatistics.merge(film.sender, 1L, Long::plus)
    }

    override fun run() {
        printDuplicateStatistics(checkDuplicates())
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
