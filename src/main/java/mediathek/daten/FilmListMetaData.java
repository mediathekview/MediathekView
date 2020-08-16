package mediathek.daten;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.time.*;
import java.time.format.DateTimeFormatter;
import java.util.Objects;

public class FilmListMetaData {
    private static final DateTimeFormatter FORMATTER = DateTimeFormatter.ofPattern("dd.MM.yyyy, HH:mm");
    private static final Logger logger = LogManager.getLogger();
    private String id = "";

    /**
     * Creation date/time of the filmlist stored in UTC.
     */
    private ZonedDateTime creationDateTime;

    /**
     * Get the filmlist create date/time in UTC
     * @return date/time in UTC as String.
     */
    public String getDatum() {
        return FORMATTER.format(creationDateTime);
    }

    /**
     * Store creation date of the filmlist.
     * This will always be UTC.
     *
     * @param datum the UTC creation date and time
     */
    public void setDatum(String datum) {
        creationDateTime = LocalDateTime.parse(datum, FORMATTER).atZone(ZoneOffset.UTC);
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
     *
     * @return creation date and time in local date/time format.
     */
    public String getGenerationDateTimeAsString() {
        String res;
        try {
            res = FORMATTER.format(creationDateTime.withZoneSameInstant(ZoneId.systemDefault()));
        }
        catch (Exception ex) {
            res = "0";
        }
        return res;
    }

    private @Nullable Duration getAge() {
        return Duration.between(creationDateTime, Instant.now().atZone(ZoneId.systemDefault()));
    }

    /**
     * Get the age of the film list.
     *
     * @return Age in seconds.
     */
    public long getAgeInSeconds() {
        long newAge;
        try {
            newAge = Objects.requireNonNull(getAge()).toSeconds();
        } catch (Exception ex) {
            newAge = 0;
        }
        return newAge;
    }

    /**
     * Check if list is older than specified parameter.
     *
     * @param sekunden The age in seconds.
     * @return true if older.
     */
    public boolean isOlderThan(long sekunden) {
        final long ret = getAgeInSeconds();
        if (ret != 0) {
            logger.info("Die Filmliste ist {} Minuten alt", ret / 60);
        }
        return ret > sekunden;
    }

    /**
     * Check if Filmlist was created after today´s date 07:00Z so we can try to use diff lists.
     *
     * @return true if too old
     */
    public boolean canUseDiffList() {
        // cannot use diff as we don´t have a reference
        if (creationDateTime == null)
            return false;

        //earliest possible diff usage is current day 07:00Z
        ZonedDateTime firstPossibleDiffTime = ZonedDateTime.of(LocalDate.now(), LocalTime.of(7, 0), ZoneOffset.UTC);
        return creationDateTime.isAfter(firstPossibleDiffTime);
    }

}
