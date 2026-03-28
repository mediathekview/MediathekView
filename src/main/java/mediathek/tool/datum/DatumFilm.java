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

import java.time.LocalDate;
import java.time.ZonedDateTime;
import java.util.Date;

public class DatumFilm extends Date {
    /**
     * When no date is specified in the film list, set to undefined for proper sorting.
     * Fictious date is 1.1.1900
     */
    public static final DatumFilm UNDEFINED_FILM_DATE = new DatumFilm(0, 0, 1);
    private ZonedDateTime zonedDateTime;

    public DatumFilm(long date) {
        super(date);
        convertToZonedDateTime();
    }

    public DatumFilm(int year, int month, int date) {
        this(legacyDateToEpochMillis(year, month, date));
    }

    @Override
    public String toString() {
        if (this.equals(UNDEFINED_FILM_DATE)) {
            return "";
        }
        else {
            return DateUtil.FORMATTER.format(DateUtil.convertToLocalDate(this));
        }
    }

    public ZonedDateTime getZonedDateTime() {
        return zonedDateTime;
    }

    private void convertToZonedDateTime() {
        zonedDateTime = this.toInstant().atZone(DateUtil.MV_DEFAULT_TIMEZONE);
    }

    private static long legacyDateToEpochMillis(int year, int month, int date) {
        return LocalDate.of(year + 1900, month + 1, date)
                .atStartOfDay(DateUtil.MV_DEFAULT_TIMEZONE)
                .toInstant()
                .toEpochMilli();
    }
}
