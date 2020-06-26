package mediathek.tool;

import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.Date;

@SuppressWarnings("serial")
public class DatumFilm extends Date {
    // die Filme werden immer in der Zeitzone "Europe/Berlin" gesucht
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy").withZone(ZoneId.of("Europe/Berlin"));

    public DatumFilm(long l) {
        super(l);
    }

    @Override
    public String toString() {
        if (getTime() == 0) {
            return "";
        } else {

            return formatter.format(DateUtil.convertToLocalDate(this));
        }
    }
}
