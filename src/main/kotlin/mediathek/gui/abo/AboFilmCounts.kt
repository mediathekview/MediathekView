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

package mediathek.gui.abo

import mediathek.daten.DatenFilm
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.tool.Filter
import java.util.*

internal object AboFilmCounts {
    fun countMatchingFilms(abos: Iterable<DatenAbo>, films: Iterable<DatenFilm>): Map<DatenAbo, Int> {
        val matchers = abos
            .filterNot { abo -> abo.isInvalid }
            .map(::AboMatcher)
        if (matchers.isEmpty()) {
            return emptyMap()
        }

        val counts = IdentityHashMap<DatenAbo, Int>()
        for (film in films) {
            for (matcher in matchers) {
                if (matcher.matches(film)) {
                    val abo = matcher.abo
                    counts[abo] = (counts[abo] ?: 0) + 1
                }
            }
        }
        return Collections.unmodifiableMap(IdentityHashMap(counts))
    }

    fun changedAbos(previous: Map<DatenAbo, Int>, current: Map<DatenAbo, Int>): Set<DatenAbo> {
        val changedAbos = Collections.newSetFromMap(IdentityHashMap<DatenAbo, Boolean>())
        for (abo in previous.keys) {
            if ((previous[abo] ?: 0) != (current[abo] ?: 0)) {
                changedAbos.add(abo)
            }
        }
        for (abo in current.keys) {
            if ((previous[abo] ?: 0) != (current[abo] ?: 0)) {
                changedAbos.add(abo)
            }
        }
        return changedAbos
    }

    private class AboMatcher(
        val abo: DatenAbo,
    ) {
        private val titelFilterPattern = createFilterPattern(abo.title)
        private val themaFilterPattern = createFilterPattern(abo.themaTitel)
        private val irgendwoFilterPattern = createFilterPattern(abo.irgendwo)

        fun matches(film: DatenFilm): Boolean =
            senderConditionExists(film) &&
                themaConditionExists(film) &&
                filterMatches(titelFilterPattern, film.title) &&
                (filterMatches(themaFilterPattern, film.thema) ||
                    filterMatches(themaFilterPattern, film.title)) &&
                (filterMatches(irgendwoFilterPattern, film.description) ||
                    filterMatches(irgendwoFilterPattern, film.thema) ||
                    filterMatches(irgendwoFilterPattern, film.title)) &&
                matchesLength(film)

        private fun themaConditionExists(film: DatenFilm): Boolean =
            abo.thema.isEmpty() || film.thema.equals(abo.thema, ignoreCase = true)

        private fun senderConditionExists(film: DatenFilm): Boolean =
            abo.sender.isEmpty() || film.sender == abo.sender

        private fun matchesLength(film: DatenFilm): Boolean =
            Filter.laengePruefen(
                abo.mindestDauerMinuten,
                film.filmLength.toLong(),
                abo.filmLengthState == FilmLengthState.MINIMUM,
            )
    }

    private fun createFilterPattern(value: String): Array<String> =
        when {
            value.isEmpty() -> EMPTY_FILTER
            Filter.isPattern(value) -> arrayOf(value)
            else -> value.lowercase(Locale.getDefault()).split(",").toTypedArray()
        }

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

    private val EMPTY_FILTER = arrayOf("")
}
