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
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

class AboFilmCountsTest {
    @Test
    fun countMatchingFilmsIncludesInactiveAbos() {
        val activeAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
        }
        val inactiveAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
            isActive = false
        }
        val films = listOf(
            DatenFilm().apply {
                sender = "ZDF"
                title = "Heute Journal"
            },
            DatenFilm().apply {
                sender = "ARD"
                title = "Heute Journal"
            },
        )

        val counts = AboFilmCounts.countMatchingFilms(listOf(activeAbo, inactiveAbo), films)

        assertEquals(1, counts[activeAbo])
        assertEquals(1, counts[inactiveAbo])
        assertNull(counts[DatenAbo()])
    }

    @Test
    fun countMatchingFilmsCountsEveryPotentialAboIndependently() {
        val broadAbo = DatenAbo().apply {
            sender = "ZDF"
        }
        val titleAbo = DatenAbo().apply {
            sender = "ZDF"
            title = "Heute Journal"
        }
        val film = DatenFilm().apply {
            sender = "ZDF"
            title = "Heute Journal"
        }

        val counts = AboFilmCounts.countMatchingFilms(listOf(broadAbo, titleAbo), listOf(film))

        assertEquals(1, counts[broadAbo])
        assertEquals(1, counts[titleAbo])
    }

    @Test
    fun countMatchingFilmsRespectsLengthFilter() {
        val minimumLengthAbo = DatenAbo().apply {
            sender = "ZDF"
            mindestDauerMinuten = 30
            filmLengthState = FilmLengthState.MINIMUM
        }
        val longEnoughFilm = DatenFilm().apply {
            sender = "ZDF"
            setFilmLengthSeconds(45 * 60)
        }
        val tooShortFilm = DatenFilm().apply {
            sender = "ZDF"
            setFilmLengthSeconds(10 * 60)
        }

        val counts = AboFilmCounts.countMatchingFilms(listOf(minimumLengthAbo), listOf(longEnoughFilm, tooShortFilm))

        assertEquals(1, counts[minimumLengthAbo])
    }

    @Test
    fun changedAbosReportsAddedChangedAndRemovedCounts() {
        val removedAbo = DatenAbo()
        val changedAbo = DatenAbo()
        val addedAbo = DatenAbo()
        val unchangedAbo = DatenAbo()
        val previous = mapOf(
            removedAbo to 1,
            changedAbo to 1,
            unchangedAbo to 2,
        )
        val current = mapOf(
            changedAbo to 3,
            addedAbo to 1,
            unchangedAbo to 2,
        )

        val changedAbos = AboFilmCounts.changedAbos(previous, current)

        assertTrue(removedAbo in changedAbos)
        assertTrue(changedAbo in changedAbos)
        assertTrue(addedAbo in changedAbos)
        assertTrue(unchangedAbo !in changedAbos)
    }
}
