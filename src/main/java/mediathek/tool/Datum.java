package mediathek.tool;

import org.apache.commons.lang3.time.FastDateFormat;

import java.util.Date;

@SuppressWarnings("serial")
public class Datum extends Date {
    protected final static FastDateFormat dateFormatter1 = FastDateFormat.getInstance("dd.MM.yyyy");

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
            return dateFormatter1.format(this);
        }
    }

    /**
     * Liefert den Betrag der Zeitdifferenz zu jetzt.
     *
     * @return Differenz in Sekunden.
     */
    public int diffInSekunden() {
        final int ret = Long.valueOf((getTime() - new Date().getTime()) / 1000).intValue();
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
