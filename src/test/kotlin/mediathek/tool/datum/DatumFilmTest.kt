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

package mediathek.tool.datum

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.time.LocalDate

class DatumFilmTest {

    @Test
    fun undefinedFilmDateRendersAsEmptyText() {
        assertEquals("", DatumFilm.UNDEFINED_FILM_DATE.toString())
    }

    @Test
    fun legacyDateConstructorKeepsDateSemantics() {
        val filmDate = DatumFilm(124, 5, 7)

        assertEquals("07.06.2024", filmDate.toString())
        assertEquals(LocalDate.of(2024, 6, 7), DateUtil.convertToLocalDate(filmDate))
    }

    @Test
    fun epochMillisConstructorKeepsOriginalTime() {
        val time = -122749200000L

        assertEquals(time, DatumFilm(time).time)
    }
}
