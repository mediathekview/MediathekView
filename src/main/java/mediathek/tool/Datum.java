package mediathek.tool;

import java.time.format.DateTimeFormatter;
import java.util.Date;

@SuppressWarnings("serial")
public class Datum extends Date {
    //hier war timezone nicht gefordert...
    private static final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");//.withZone(ZoneId.of("Europe/Berlin"));

    public Datum() {
        super();
    }

    public Datum(long l) {
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

    /**
     * Liefert den Betrag der Zeitdifferenz zu jetzt.
     *
     * @return Differenz in Sekunden.
     */
    public int diffInSekunden() {
        final int ret = Long.valueOf((getTime() - System.currentTimeMillis()) / 1000).intValue();
        return Math.abs(ret);
    }

    /**
     * Liefert den BETRAG! der Zeitdifferenz zu jetzt.
     *
     * @return Differenz in Minuten.
     */
    public int diffInMinuten() {
        return (diffInSekunden() / 60);
    }
}
