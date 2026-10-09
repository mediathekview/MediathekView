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

package mediathek.daten.abo

import kotlinx.coroutines.*
import mediathek.daten.DatenFilm
import mediathek.daten.ListeFilme
import mediathek.tool.Filter
import java.util.*

class AboFilmAssignmentService {
    fun findAboForFilm(film: DatenFilm, checkLength: Boolean): DatenAbo? {
        val abo = film.abo ?: return null

        if (checkLength && !matchesLength(abo, film)) {
            return null
        }

        return abo
    }

    /**
     * Assign found active abo to the film objects.
     * Time-intensive procedure!
     */
    fun assignAbosToFilms(abos: List<DatenAbo>, films: ListeFilme, removeMissingAbos: Boolean) {
        if (abos.isEmpty() && removeMissingAbos) {
            films.forEach { film -> clearAbo(film) }
            return
        }

        val validAbos = abos.filterNot { datenAbo -> datenAbo.isInvalid }
        val aboMatchers = validAbos
            .asSequence()
            .filter { datenAbo -> datenAbo.isActive }
            .mapIndexed { index, datenAbo -> createAboMatcher(index, datenAbo) }
            .toList()

        if (aboMatchers.isEmpty()) {
            films.forEach { film -> clearAbo(film) }
            return
        }

        val indexedAboMatchers = IndexedAboMatchers(aboMatchers)
        assignAbosToFilmSnapshot(films.snapshot(), indexedAboMatchers)
    }

    private fun matchesLength(abo: DatenAbo, film: DatenFilm): Boolean =
        Filter.laengePruefen(
            abo.mindestDauerMinuten,
            film.filmLength.toLong(),
            abo.filmLengthState == FilmLengthState.MINIMUM,
        )

    private fun clearAbo(film: DatenFilm) {
        film.abo = null
    }

    private fun createAboMatcher(index: Int, abo: DatenAbo): CompiledAboMatcher =
        CompiledAboMatcher(
            index = index,
            abo = abo,
            titelFilterPattern = createFilterPattern(abo.title),
            themaFilterPattern = createFilterPattern(abo.themaTitel),
            irgendwoFilterPattern = createFilterPattern(abo.irgendwo),
        )

    private fun createFilterPattern(value: String): Array<String> =
        when {
            value.isEmpty() -> LEER
            Filter.isPattern(value) -> arrayOf(value)
            else -> value.lowercase(Locale.ROOT).split(",").toTypedArray()
        }

    private fun assignAboToFilm(film: DatenFilm, aboMatchers: IndexedAboMatchers) {
        var textMatch: DatenAbo? = null

        val candidates = aboMatchers.candidatesFor(film.sender)
        var candidateIndex = 0
        while (candidateIndex < candidates.size) {
            val matcher = candidates[candidateIndex]
            candidateIndex++
            val abo = matcher.abo
            if (!matcher.matches(film)) {
                continue
            }

            if (textMatch == null) {
                textMatch = abo
            }

            if (matchesLength(abo, film)) {
                film.abo = abo
                return
            }
        }

        if (textMatch == null) {
            clearAbo(film)
        } else {
            film.abo = textMatch
        }
    }

    private fun assignAbosToFilmSnapshot(films: List<DatenFilm>, aboMatchers: IndexedAboMatchers) {
        if (films.isEmpty()) {
            return
        }

        runBlocking {
            withContext(Dispatchers.Default) {
                val workerCount = Runtime.getRuntime().availableProcessors().coerceAtLeast(1)
                val chunkSize = ((films.size + workerCount - 1) / workerCount).coerceAtLeast(1)
                val deferredAssignments = ArrayList<Deferred<Unit>>()
                var startIndex = 0
                while (startIndex < films.size) {
                    val endIndex = (startIndex + chunkSize).coerceAtMost(films.size)
                    val chunkStartIndex = startIndex
                    deferredAssignments.add(
                        async {
                            assignAboToFilmRange(films, chunkStartIndex, endIndex, aboMatchers)
                        }
                    )
                    startIndex = endIndex
                }

                var deferredIndex = 0
                while (deferredIndex < deferredAssignments.size) {
                    deferredAssignments[deferredIndex].await()
                    deferredIndex++
                }
            }
        }
    }

    private fun assignAboToFilmRange(
        films: List<DatenFilm>,
        startIndex: Int,
        endIndex: Int,
        aboMatchers: IndexedAboMatchers,
    ) {
        var index = startIndex
        while (index < endIndex) {
            assignAboToFilm(films[index], aboMatchers)
            index++
        }
    }

    private companion object {
        private val LEER = arrayOf("")
    }

    private class IndexedAboMatchers(matchers: List<CompiledAboMatcher>) {
        private val globalMatchers = matchers.filter { matcher -> matcher.matchesAnySender }
        private val senderMatchers = matchers
            .filterNot { matcher -> matcher.matchesAnySender }
            .groupBy { matcher -> matcher.sender }
        private val candidatesBySender = senderMatchers.mapValues { (_, matchers) ->
            mergeByOriginalOrder(globalMatchers, matchers)
        }

        fun candidatesFor(sender: String): List<CompiledAboMatcher> =
            candidatesBySender[sender] ?: globalMatchers

        private fun mergeByOriginalOrder(
            globalMatchers: List<CompiledAboMatcher>,
            senderMatchers: List<CompiledAboMatcher>,
        ): List<CompiledAboMatcher> {
            if (globalMatchers.isEmpty()) {
                return senderMatchers
            }
            if (senderMatchers.isEmpty()) {
                return globalMatchers
            }

            val merged = ArrayList<CompiledAboMatcher>(globalMatchers.size + senderMatchers.size)
            var globalIndex = 0
            var senderIndex = 0
            while (globalIndex < globalMatchers.size && senderIndex < senderMatchers.size) {
                val globalMatcher = globalMatchers[globalIndex]
                val senderMatcher = senderMatchers[senderIndex]
                if (globalMatcher.index < senderMatcher.index) {
                    merged.add(globalMatcher)
                    globalIndex++
                } else {
                    merged.add(senderMatcher)
                    senderIndex++
                }
            }
            while (globalIndex < globalMatchers.size) {
                merged.add(globalMatchers[globalIndex++])
            }
            while (senderIndex < senderMatchers.size) {
                merged.add(senderMatchers[senderIndex++])
            }
            return merged
        }
    }

    private class CompiledAboMatcher(
        val index: Int,
        val abo: DatenAbo,
        val titelFilterPattern: Array<String>,
        val themaFilterPattern: Array<String>,
        val irgendwoFilterPattern: Array<String>,
    ) {
        val sender: String = abo.sender
        val matchesAnySender: Boolean = sender.isEmpty()

        fun matches(film: DatenFilm): Boolean =
            themaConditionExists(film) &&
                filterMatches(titelFilterPattern, film.title) &&
                (filterMatches(themaFilterPattern, film.thema) ||
                    filterMatches(themaFilterPattern, film.title)) &&
                (filterMatches(irgendwoFilterPattern, film.description) ||
                    filterMatches(irgendwoFilterPattern, film.thema) ||
                    filterMatches(irgendwoFilterPattern, film.title))

        private fun themaConditionExists(film: DatenFilm): Boolean =
            abo.thema.isEmpty() || film.thema.equals(abo.thema, ignoreCase = true)

        private fun filterMatches(filter: Array<String>, text: String): Boolean {
            val firstFilter = filter[0]
            if (filter.size == 1) {
                if (firstFilter.isEmpty()) {
                    return true
                }

                Filter.makePattern(firstFilter)?.let { pattern ->
                    return pattern.matcher(text).matches()
                }
            }

            return Filter.checkContainsIgnoreCase(filter, text)
        }
    }
}
