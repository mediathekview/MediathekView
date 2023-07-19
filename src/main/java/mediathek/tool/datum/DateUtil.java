package mediathek.tool.datum;

import mediathek.daten.DatenFilm;
import org.jetbrains.annotations.NotNull;

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

    public static LocalDate convertToLocalDate(Date dateToConvert) {
        return dateToConvert.toInstant()
                .atZone(MV_DEFAULT_TIMEZONE)
                .toLocalDate();
    }
}
