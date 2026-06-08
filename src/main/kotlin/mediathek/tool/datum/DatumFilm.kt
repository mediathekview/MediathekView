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

import java.time.LocalDate
import java.util.Date

class DatumFilm : Date {
    constructor(date: Long) : super(date)

    constructor(year: Int, month: Int, date: Int) : this(legacyDateToEpochMillis(year, month, date))

    override fun toString(): String =
        if (this == UNDEFINED_FILM_DATE) {
            ""
        } else {
            DateUtil.FORMATTER.format(DateUtil.convertToLocalDate(this))
        }

    companion object {
        val UNDEFINED_FILM_DATE = DatumFilm(0, 0, 1)

        private fun legacyDateToEpochMillis(year: Int, month: Int, date: Int): Long =
            LocalDate.of(year + 1900, month + 1, date)
                .atStartOfDay(DateUtil.MV_DEFAULT_TIMEZONE)
                .toInstant()
                .toEpochMilli()
    }
}
