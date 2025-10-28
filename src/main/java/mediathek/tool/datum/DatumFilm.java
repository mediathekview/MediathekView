package mediathek.tool.datum;

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
        super(year, month, date);
        convertToZonedDateTime();
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
}