package mediathek.tool.datum;

import java.util.Date;

public class Datum extends Date {
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
            return DateUtil.FORMATTER.format(DateUtil.convertToLocalDate(this));
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
