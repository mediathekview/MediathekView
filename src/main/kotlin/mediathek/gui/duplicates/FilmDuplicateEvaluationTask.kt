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

import mediathek.config.Daten
import mediathek.daten.ListeFilme
import org.apache.logging.log4j.LogManager

class FilmDuplicateEvaluationTask : Runnable {
    private val listeFilme: ListeFilme = Daten.getInstance().listeFilme

    private fun printDuplicateStatistics() {
        val statisticsMap = listeFilme.parallelStream()
            .filter { film -> film.isDuplicate }
            .countFilmsBySender()
        val duplicateCount = statisticsMap.values.sum()

        replaceFilmStatistics(Daten.getInstance().duplicateStatistics, statisticsMap)

        logger.trace("Number of duplicates: {}", duplicateCount)
    }

    private fun checkDuplicates() {
        logger.trace("Start Duplicate URL search")
        val urlCache = HashMap<String, MutableMap<String, MutableSet<String>>>()
        listeFilme.stream()
            .filter { film -> !film.isLivestream }
            .sorted(BigSenderPenaltyComparator())
            .forEach { film ->
                val normalUrl = film.urlNormalQuality
                val highQualityUrl = if (film.isHighQuality) film.highQualityUrl else ""
                val lowQualityUrl = if (film.hasLowQuality()) film.lowQualityUrl else ""

                val byHighQualityUrl = urlCache.getOrPut(normalUrl) { HashMap() }
                val seenLowQualityUrls = byHighQualityUrl.getOrPut(highQualityUrl) { HashSet() }

                film.isDuplicate = !seenLowQualityUrls.add(lowQualityUrl)
            }
    }

    override fun run() {
        checkDuplicates()
        printDuplicateStatistics()
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
