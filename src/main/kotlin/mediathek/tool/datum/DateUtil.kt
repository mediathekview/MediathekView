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

import mediathek.daten.DatenFilm
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter
import java.util.*

object DateUtil {
    @JvmField
    val MV_DEFAULT_TIMEZONE: ZoneId = ZoneId.of("Europe/Berlin")

    @JvmField
    val FORMATTER: DateTimeFormatter = DateTimeFormatter.ofPattern("dd.MM.yyyy")
        .withZone(MV_DEFAULT_TIMEZONE)

    private val UTC_ZONE_ID: ZoneId = ZoneId.of("UTC")

    fun convertFilmDateToLuceneDate(film: DatenFilm): Long =
        convertToLocalDate(film.datumFilm)!!
            .atStartOfDay()
            .atZone(UTC_ZONE_ID)
            .toInstant()
            .toEpochMilli()

    @JvmStatic
    fun convertToLocalDate(dateToConvert: Date?): LocalDate? =
        dateToConvert
            ?.toInstant()
            ?.atZone(MV_DEFAULT_TIMEZONE)
            ?.toLocalDate()

    fun convertToDate(ld: LocalDate): Date =
        Date.from(ld.atStartOfDay(MV_DEFAULT_TIMEZONE).toInstant())
}
