package mSearch.tool;

import org.apache.commons.lang3.time.FastDateFormat;

import java.util.TimeZone;

@SuppressWarnings("serial")
public class DatumFilm extends Datum {
    // die Filme werden immer in der Zeitzone "Europe/Berlin" gesucht
    protected static final FastDateFormat dateFormatter1 = FastDateFormat.getInstance("dd.MM.yyyy", TimeZone.getTimeZone("Europe/Berlin"));

    public DatumFilm(long l) {
        super(l);

    }

    @Override
    public String toString() {
        if (getTime() == 0) {
            return "";
        } else {
            return dateFormatter1.format(this);
        }
    }
}
