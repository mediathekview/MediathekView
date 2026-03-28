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

package mediathek.tool.datum;

import mediathek.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;

public class DateUtil {
    public static final ZoneId MV_DEFAULT_TIMEZONE = ZoneId.of("Europe/Berlin");

    public static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy")
            .withZone(MV_DEFAULT_TIMEZONE);
    private static final ZoneId UTC_ZONE_ID = ZoneId.of("UTC");

    public static long convertFilmDateToLuceneDate(@NotNull DatenFilm film) {
        var ldt = DateUtil.convertToLocalDate(film.getDatumFilm()).atStartOfDay();
        return ldt.atZone(UTC_ZONE_ID).toInstant().toEpochMilli();
    }

    public static LocalDate convertToLocalDate(@Nullable Date dateToConvert) {
        if (dateToConvert == null)
            return null;

        return dateToConvert.toInstant()
                .atZone(MV_DEFAULT_TIMEZONE)
                .toLocalDate();
    }

    public static Date convertToDate(@NotNull LocalDate ld) {
        return Date.from(ld.atStartOfDay(MV_DEFAULT_TIMEZONE).toInstant());
    }
}
