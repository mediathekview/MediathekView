package mediathek.tool.datum;

import mediathek.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.time.Instant;
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

    public static Date convertToDate(@NotNull Instant instant) {
        return Date.from(instant.atZone(MV_DEFAULT_TIMEZONE).toInstant());
    }

    public static Date convertToDate(@NotNull LocalDate ld) {
        return Date.from(ld.atStartOfDay(MV_DEFAULT_TIMEZONE).toInstant());
    }
}
