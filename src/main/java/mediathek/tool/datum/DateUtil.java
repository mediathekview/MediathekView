package mediathek.tool.datum;

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;

public class DateUtil {
    public static final ZoneId MV_DEFAULT_TIMEZONE = ZoneId.of("Europe/Berlin");

    public static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy")
            .withZone(MV_DEFAULT_TIMEZONE);

    public static LocalDate convertToLocalDate(Date dateToConvert) {
        return dateToConvert.toInstant()
                .atZone(MV_DEFAULT_TIMEZONE)
                .toLocalDate();
    }
}
