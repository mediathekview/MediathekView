package mediathek.daten;

import org.apache.commons.lang3.time.FastDateFormat;

import java.text.ParseException;
import java.util.Date;
import java.util.SimpleTimeZone;

public class FilmListMetaData {
    private final static String DATUM_ZEIT_FORMAT = "dd.MM.yyyy, HH:mm";
    private static final FastDateFormat sdf_ = FastDateFormat.getInstance(DATUM_ZEIT_FORMAT,new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"));
    private String datum = "";
    private String id = "";

    public String getDatum() {
        return datum;
    }

    public void setDatum(String datum) {
        this.datum = datum;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getVersion() {
        return "3";
    }

    /**
     * Return the filmlist creation date and time as string.
     * Filmlist date is in UTC.
     * @return creation date and time in local format.
     */
    public String getGenerationDateTime() {
        final String date = datum;

        String ret;
        try {
            final Date filmDate = sdf_.parse(datum);
            final FastDateFormat formatter = FastDateFormat.getInstance(DATUM_ZEIT_FORMAT);
            ret = formatter.format(filmDate);
        } catch (ParseException ignored) {
            ret = date;
        }

        return ret;
    }

    /**
     * Get the age of the film list.
     *
     * @return Age as a {@link java.util.Date} object.
     */
    public Date getAgeAsDate() {
        String date = datum;

        Date filmDate = null;
        try {
            filmDate = sdf_.parse(date);
        } catch (ParseException ignored) {
        }

        return filmDate;
    }

    /**
     * Get the age of the film list.
     *
     * @return Age in seconds.
     */
    public long getAge() {
        long ret = 0;

        Date filmDate = getAgeAsDate();
        if (filmDate != null) {
            ret = (System.currentTimeMillis() - filmDate.getTime()) / 1000;
            if (ret < 0) {
                ret = 0;
            }
        }
        return ret;
    }
}
