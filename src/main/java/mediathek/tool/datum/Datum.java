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
}
