package mediathek.daten;

import mediathek.config.Konstanten;
import org.apache.commons.lang3.time.FastDateFormat;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.text.ParseException;
import java.text.SimpleDateFormat;
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

    private static final Logger logger = LogManager.getLogger();
    /**
     * Check if list is older than specified parameter.
     *
     * @param sekunden The age in seconds.
     * @return true if older.
     */
    public boolean isOlderThan(long sekunden) {
        final long ret = getAge();
        if (ret != 0) {
            logger.info("Die Filmliste ist {} Minuten alt", ret / 60);
        }
        return ret > sekunden;
    }

    /**
     * Check if Filmlist is too old for using a diff list.
     *
     * @return true if empty or too old.
     */
    public boolean isTooOldForDiff() {
        try {
            final String dateMaxDiff_str = new SimpleDateFormat("yyyy.MM.dd__").format(new Date()) + Konstanten.TIME_MAX_AGE_FOR_DIFF + ":00:00";
            final Date dateMaxDiff = new SimpleDateFormat("yyyy.MM.dd__HH:mm:ss").parse(dateMaxDiff_str);
            final Date dateFilmliste = getAgeAsDate();
            if (dateFilmliste != null) {
                return dateFilmliste.getTime() < dateMaxDiff.getTime();
            }
        } catch (Exception ignored) {
        }
        return true;
    }

}
