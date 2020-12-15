package mediathek.daten;

import mediathek.config.Daten;
import mediathek.javafx.bookmark.BookmarkData;
import mediathek.tool.*;
import mediathek.tool.sql.SqlAutoRollback;
import mediathek.tool.sql.SqlAutoSetAutoCommit;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.lang.ref.Cleaner;
import java.sql.SQLException;
import java.util.EnumSet;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicInteger;

/*
 * TODO:
 * - Remove the Database Stuff from this Class to own Classes and a real OR-Mapping
 * - Finalize a Real Entity
 * - Write test cases for each Method
 * - Write JavaDoc for each of the new Methods that were split from this moloch
 */

public class DatenFilm implements AutoCloseable, Comparable<DatenFilm>, Cloneable {
    public static final int FILM_NR = 0;      // wird vor dem Speichern gelöscht!
    public static final int FILM_SENDER = 1;
    public static final int FILM_THEMA = 2;
    public static final int FILM_TITEL = 3;
    public static final int FILM_ABSPIELEN = 4; // no getter/setter access
    public static final int FILM_AUFZEICHNEN = 5; // no getter/setter access
    public static final int FILM_MERKEN = 6; // no getter/setter access
    public static final int FILM_DATUM = 7;
    public static final int FILM_ZEIT = 8;
    public static final int FILM_DAUER = 9;
    public static final int FILM_GROESSE = 10;
    public static final int FILM_HD = 11; // no getter/setter access
    public static final int FILM_UT = 12; // no getter/setter access
    public static final int FILM_GEO = 13; // Geoblocking
    public static final int FILM_URL = 14;
    /**
     * Index for Date as long value in SECONDS!!
     */
    public static final int FILM_DATUM_LONG = 15;
    public static final int FILM_REF = 16; // no getter/setter access // Referenz auf this
    public static final int MAX_ELEM = 17;
    /**
     * The database instance for all descriptions.
     */
    private final static AtomicInteger FILM_COUNTER = new AtomicInteger(0);
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static final Logger logger = LogManager.getLogger(DatenFilm.class);
    private final EnumSet<DatenFilmFlags> flags = EnumSet.noneOf(DatenFilmFlags.class);
    private DatenAbo abo;
    private BookmarkData bookmark;
    /**
     * film date stored IN SECONDS!!!
     */
    private DatumFilm datumFilm = DatumFilm.UNDEFINED_FILM_DATE;
    /**
     * File size in MByte
     */
    private FilmSize filmSize;
    /**
     * film length in seconds.
     */
    private long filmLength;
    /**
     * Internal film number, used for storage in database
     */
    private int databaseFilmNumber;
    private Cleaner.Cleanable cleaner;
    private String websiteLink;
    private String description;
    private String urlKlein = "";
    /**
     * High Quality (formerly known as HD) URL if available.
     */
    private Optional<String> highQuality_url = Optional.empty();
    private String aboName = "";
    private String datumLong = "";
    private String sender = "";
    private String thema = "";
    private String titel = "";
    /**
     * String of countries where this entry can be viewed, if available.
     * Empty means viewable without restrictions.
     */
    private Optional<String> availableInCountries = Optional.empty();
    /**
     * URL to the subtitle file, if available.
     */
    private Optional<String> subtitle_url = Optional.empty();
    private String datum = "";
    private String sendeZeit = "";
    private String dauer = "";
    private String groesse = "";
    private String url = "";
    /**
     * film duration in seconds.
     * getDauer() stores the same info as a String
     */
    private int duration;

    public DatenFilm() {
        filmSize = new FilmSize(0); // Dateigröße in MByte
        databaseFilmNumber = FILM_COUNTER.getAndIncrement();

        setupDatabaseCleanup();
    }

    public int getDuration() {
        return duration;
    }

    public DatenAbo getAbo() {
        return abo;
    }

    public void setAbo(DatenAbo abo) {
        this.abo = abo;
    }

    public DatumFilm getDatumFilm() {
        return datumFilm;
    }

    public String getUrlKlein() {
        return urlKlein;
    }

    public void setUrlKlein(String urlKlein) {
        this.urlKlein = urlKlein;
    }

    public String getUrlHighQuality() {
        return highQuality_url.orElse("");
    }

    public void setUrlHighQuality(String urlHd) {
        if (!urlHd.isEmpty())
            highQuality_url = Optional.of(urlHd);
        else
            highQuality_url = Optional.empty();
    }

    public String getAboName() {
        return aboName;
    }

    public void setAboName(String aboName) {
        this.aboName = aboName;
    }

    public String getDatumLong() {
        return datumLong;
    }

    public void setDatumLong(String datumLong) {
        this.datumLong = datumLong;
    }

    public boolean isTrailerTeaser() {
        return flags.contains(DatenFilmFlags.TRAILER_TEASER);
    }

    public void setTrailerTeaser(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.TRAILER_TEASER);
        } else {
            flags.remove(DatenFilmFlags.TRAILER_TEASER);
        }
    }

    public boolean isAudioVersion() {
        return flags.contains(DatenFilmFlags.AUDIO_VERSION);
    }

    public void setAudioVersion(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.AUDIO_VERSION);
        } else {
            flags.remove(DatenFilmFlags.AUDIO_VERSION);
        }
    }

    public boolean isPlayList() {
        return flags.contains(DatenFilmFlags.PLAYLIST);
    }

    public void setPlayList(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.PLAYLIST);
        } else {
            flags.remove(DatenFilmFlags.PLAYLIST);
        }
    }

    public boolean isSignLanguage() {
        return flags.contains(DatenFilmFlags.SIGN_LANGUAGE);
    }

    public void setSignLanguage(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.SIGN_LANGUAGE);
        } else {
            flags.remove(DatenFilmFlags.SIGN_LANGUAGE);
        }
    }

    /**
     * Get the film number.
     * This is used internally for the database id AND
     * for the old MV code that might access it for various stuff.
     *
     * @return the original internal film number
     */
    public int getFilmNr() {
        return databaseFilmNumber;
    }

    private void setupDatabaseCleanup() {
        if (MemoryUtils.isLowMemoryEnvironment()) {
            final boolean useCleaner = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DATABASE_USE_CLEANER_INTERFACE, false);
            if (useCleaner)
                installCleanupTask();
        }
    }

    private void installCleanupTask() {
        DatenFilmCleanupTask task = new DatenFilmCleanupTask(databaseFilmNumber);
        cleaner = Daten.getInstance().getCleaner().register(this, task);
    }

    /**
     * Get the file size of this film.
     *
     * @return The size in MByte
     */
    public FilmSize getFilmSize() {
        return filmSize;
    }

    @Override
    public void close() {
        if (cleaner != null)
            cleaner.clean();
    }

    /**
     * Get the film description from database.
     *
     * @return the film description.
     */
    public String getDescription() {
        if (MemoryUtils.isLowMemoryEnvironment()) {
            try (var connection = PooledDatabaseConnection.INSTANCE.getDataSource().getConnection();
                 var statement = connection.createStatement();
                 var rs = statement.executeQuery("SELECT desc FROM mediathekview.description WHERE id = " + databaseFilmNumber)) {
                return (rs.next() ? rs.getString(1) : "");
            } catch (SQLException ex) {
                logger.error(ex);
                return "";
            }
        } else {
            return StringUtils.defaultString(description);
        }
    }

    /**
     * Store description in database.
     * Performs an UPSERT as we may want to update the description later again.
     *
     * @param desc String to be stored.
     */
    public void setDescription(final String desc) {
        if (desc != null && !desc.isEmpty()) {
            if (MemoryUtils.isLowMemoryEnvironment()) {
                try (var connection = PooledDatabaseConnection.INSTANCE.getDataSource().getConnection();
                     var ignored = new SqlAutoSetAutoCommit(connection, false);
                     var tm = new SqlAutoRollback(connection);
                     var mergeStatement = connection.prepareStatement("MERGE INTO mediathekview.description KEY(ID) VALUES (?,?)")) {
                    mergeStatement.setInt(1, databaseFilmNumber);
                    mergeStatement.setString(2, desc);
                    mergeStatement.executeUpdate();
                    tm.commit();
                } catch (SQLException ex) {
                    logger.error(ex);
                }
            } else
                description = desc;
        }
    }

    public String getWebsiteLink() {
        if (MemoryUtils.isLowMemoryEnvironment()) {
            try (var connection = PooledDatabaseConnection.INSTANCE.getDataSource().getConnection();
                 var statement = connection.createStatement();
                 var rs = statement.executeQuery("SELECT link FROM mediathekview.website_links WHERE id = " + databaseFilmNumber)) {
                return (rs.next() ? rs.getString(1) : "");
            } catch (SQLException ex) {
                logger.error(ex);
                return "";
            }
        } else
            return websiteLink != null ? websiteLink : "";
    }

    public void setWebsiteLink(String link) {
        if (link != null && !link.isEmpty()) {
            if (MemoryUtils.isLowMemoryEnvironment()) {
                try (var connection = PooledDatabaseConnection.INSTANCE.getDataSource().getConnection();
                     var ignored = new SqlAutoSetAutoCommit(connection, false);
                     var tm = new SqlAutoRollback(connection);
                     var mergeStatement = connection.prepareStatement("MERGE INTO mediathekview.website_links KEY(ID) VALUES (?,?)")
                ){
                    mergeStatement.setInt(1, databaseFilmNumber);
                    mergeStatement.setString(2, link);
                    mergeStatement.executeUpdate();
                    tm.commit();
                }
                catch (SQLException ex) {
                    logger.error(ex);
                }
            } else {
                websiteLink = link;
            }
        }
    }

    /**
     * Indicate whether this entry has been in the filmlist before.
     *
     * @return true if it is a new entry, false otherwise.
     */
    public boolean isNew() {
        return flags.contains(DatenFilmFlags.NEW_ENTRY);
    }

    public void setNew(final boolean newFilm) {
        if (newFilm) {
            flags.add(DatenFilmFlags.NEW_ENTRY);
        } else {
            flags.remove(DatenFilmFlags.NEW_ENTRY);
        }
    }

    public boolean isLivestream() {
        return flags.contains(DatenFilmFlags.LIVESTREAM);
    }

    public void setLivestream(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.LIVESTREAM);
        } else {
            flags.remove(DatenFilmFlags.LIVESTREAM);
        }
    }

    public void setBurnedInSubtitles(boolean val) {
        if (val) {
            flags.add(DatenFilmFlags.BURNED_IN_SUBTITLES);
        } else {
            flags.remove(DatenFilmFlags.BURNED_IN_SUBTITLES);
        }
    }

    /**
     * Indicate if the film has encoded aka. "burned in" subtitles"
     *
     * @return true if they are burned in, false othewise.
     */
    public boolean hasBurnedInSubtitles() {
        return flags.contains(DatenFilmFlags.BURNED_IN_SUBTITLES);
    }

    public boolean hasSubtitle() {
        return subtitle_url.isPresent();
    }

    //TODO This function might not be necessary as getUrlNormalOrRequested does almost the same
    public String getUrlFuerAufloesung(FilmResolution.Enum resolution) {
        return switch (resolution) {
            case LOW, HIGH_QUALITY -> getUrlNormalOrRequested(resolution);
            default -> getUrl();
        };
    }

    public String getDateigroesse(String url) {
        if (url.equalsIgnoreCase(getUrl())) {
            return getSize();
        } else {
            return FileSize.getFileLengthFromUrl(url);
        }
    }

    /**
     * Return a unique index for comparison during updating the filmlist from diff.
     *
     * @return a unique "hash" string
     */
    public String getUniqueHash() {
        return (getSender() + getThema()).toLowerCase() + getUrl() + getWebsiteLink();
    }

    /**
     * film entry contains non-empty HQ url.
     *
     * @return true if HQ url is not empty.
     */
    public boolean isHighQuality() {
        return highQuality_url.isPresent();
    }

    @Override
    public Object clone() throws CloneNotSupportedException {
        DatenFilm ret = (DatenFilm) super.clone();
        ret.datumFilm = this.datumFilm;
        ret.databaseFilmNumber = this.databaseFilmNumber;
        ret.filmSize = this.filmSize;
        ret.filmLength = this.filmLength;
        ret.abo = this.abo;
        ret.highQuality_url = this.highQuality_url;
        ret.urlKlein = this.urlKlein;
        ret.aboName = this.aboName;
        ret.datumLong = this.datumLong;
        ret.sender = this.sender;
        ret.thema = this.thema;
        ret.titel = this.titel;
        ret.availableInCountries = this.availableInCountries;
        ret.datum = this.datum;
        ret.sendeZeit = this.sendeZeit;
        ret.dauer = this.dauer;
        ret.groesse = this.groesse;
        ret.url = this.url;
        ret.subtitle_url = this.subtitle_url;

        return ret;
    }

    @Override
    public int compareTo(@NotNull DatenFilm other) {
        int ret;
        if ((ret = sorter.compare(getSender(), other.getSender())) == 0) {
            return sorter.compare(getThema(), other.getThema());
        }
        return ret;
    }

    /**
     * Get the filmlength in seconds.
     *
     * @return filmlength in seconds, or 0.
     */
    public long getFilmLength() {
        return filmLength;
    }

    /**
     * Convert HH:MM:SS string into seconds.
     * Or set to 0 in case of error.
     *
     * @return result in seconds or 0.
     */
    private long parseTimeToSeconds() {
        long seconds = 0;
        final String[] split = StringUtils.split(getDauer(), ':');

        try {
            seconds += Long.parseLong(split[0]) * 3600; //hour
            seconds += Long.parseLong(split[1]) * 60; //minute
            seconds += Long.parseLong(split[2]); //second
        } catch (Exception e) {
            seconds = 0;
        }

        return seconds;
    }

    private void setDatum() {
        if (!getSendeDatum().isEmpty()) {
            // nur dann gibts ein Datum
            try {
                final long l = Long.parseLong(getDatumLong());
                datumFilm = new DatumFilm(l * 1000); // sind SEKUNDEN!!
            } catch (Exception ex) {
                logger.error("Datum: {}, Zeit: {}, Datum_LONG: {}", getSendeDatum(), getSendeZeit(), getDatumLong(), ex);
                datumFilm = new DatumFilm(0);
                setSendeDatum("");
                setSendeZeit("");
            }
        }
    }

    public void init() {
        filmSize = new FilmSize(this);
        filmLength = parseTimeToSeconds();

        setDatum();
    }

    /**
     * Return unpacked url as string.
     * High quality URLs may be "compressed" in the filmlist and need to be unpacked before use.
     *
     * @param aufloesung One of FilmResolution.HIGH_QUALITY,FilmResolution.LOW,FilmResolution.NORMAL.
     * @return A unpacked version of the film url as string.
     */
    private String getUrlNormalOrRequested(@NotNull FilmResolution.Enum aufloesung) {
        String ret;
        // liefert die kleine normale URL oder die HD URL
        final String requestedUrl = getUrlByAufloesung(aufloesung);
        if (requestedUrl.isEmpty())
            ret = getUrl();
        else {
            try {
                // check if url contains pipe symbol...
                final int indexPipe = requestedUrl.indexOf('|');
                if (indexPipe == -1) { //No
                    ret = requestedUrl;
                } else { //Yes
                    ret = decompressUrl(requestedUrl, indexPipe);
                }
            } catch (Exception e) {
                ret = "";
                logger.error("getUrlNormalOrRequested(auflösung: {}, requestedUrl: {})", aufloesung, requestedUrl, e);
            }
        }

        return ret;
    }

    private String decompressUrl(@NotNull final String requestedUrl, final int indexPipe) {
        final int i = Integer.parseInt(requestedUrl.substring(0, indexPipe));
        return getUrl().substring(0, i) + requestedUrl.substring(indexPipe + 1);
    }

    /**
     * Return url based on requested resolution
     *
     * @param aufloesung One of FilmResolution.AUFLOESUNG_HD,FilmResolution.AUFLOESUNG_KLEIN,FilmResolution.AUFLOESUNG_NORMAL.
     * @return url as String.
     */
    private String getUrlByAufloesung(@NotNull final FilmResolution.Enum aufloesung) {
        return switch (aufloesung) {
            case HIGH_QUALITY -> getUrlHighQuality();
            case LOW -> getUrlKlein();
            default -> getUrl();
        };
    }

    public String getSender() {
        return sender;
    }

    public void setSender(String sender) {
        this.sender = sender;
    }

    public String getThema() {
        return thema;
    }

    public void setThema(String thema) {
        this.thema = thema;
    }

    public String getTitle() {
        return titel;
    }

    public void setTitle(String title) {
        this.titel = title;
    }

    public String getSendeDatum() {
        return datum;
    }

    public void setSendeDatum(String sendeDatum) {
        this.datum = sendeDatum;
    }

    public String getSendeZeit() {
        return sendeZeit;
    }

    public void setSendeZeit(String sendeZeit) {
        this.sendeZeit = sendeZeit;
    }

    public String getDauer() {
        return dauer;
    }

    public void setDauer(String dauer) {
        this.dauer = dauer;

        //FIXME gefällt mir nicht
        final String[] split = StringUtils.split(getDauer(), ':');

        try {
            duration += Integer.parseInt(split[0]) * 3600; //hour
            duration += Integer.parseInt(split[1]) * 60; //minute
            duration += Integer.parseInt(split[2]); //second
        } catch (Exception e) {
            duration = 0;
        }
    }

    public String getSize() {
        return groesse;
    }

    public void setSize(String size) {
        this.groesse = size;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getUrlSubtitle() {
        return subtitle_url.orElse("");
    }

    public void setUrlSubtitle(String urlSubtitle) {
        if (!urlSubtitle.isEmpty())
            subtitle_url = Optional.of(urlSubtitle);
        else
            subtitle_url = Optional.empty();
    }

    public Optional<String> getGeo() {
        return availableInCountries;
    }

    public void setGeo(Optional<String> availableInCountries) {
        this.availableInCountries = availableInCountries;
    }

    /**
     * Get bookmark entry
     *
     * @return BookmarkData entry
     */
    public BookmarkData getBookmark() {
        return this.bookmark;
    }

    /**
     * Link with bookmark entry
     *
     * @param bookmark Bookmark entry
     */
    public void setBookmark(BookmarkData bookmark) {
        this.bookmark = bookmark;
    }

    /**
     * check if movie is bookmarked
     *
     * @return boolean true
     */
    public boolean isBookmarked() {
        return this.bookmark != null;
    }


    public static class Database {
        private Database() {
        }

        public static void closeDatabase() {
            var ds = PooledDatabaseConnection.INSTANCE.getDataSource();
            ds.close();
        }

        public static void createIndices() {
            logger.trace("Creating SQL indices");
            try (var connection = PooledDatabaseConnection.INSTANCE.getDataSource().getConnection();
                 var ignored = new SqlAutoSetAutoCommit(connection, false);
                 var tm = new SqlAutoRollback(connection);
                 var statement = connection.createStatement()) {
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_DESC_ID ON mediathekview.description (id)");
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_WEBSITE_LINKS_ID ON mediathekview.website_links (id)");
                tm.commit();
            } catch (SQLException ex) {
                logger.error(ex);
            }
            logger.trace("Finished creating SQL indices");
        }

        public static void initializeDatabase() {
            logger.debug("initializeDatabase()");
            try (var connection = PooledDatabaseConnection.INSTANCE.getDataSource().getConnection();
                 var ignored = new SqlAutoSetAutoCommit(connection, false);
                 var tm = new SqlAutoRollback(connection);
                 var statement = connection.createStatement()) {
                statement.executeUpdate("SET WRITE_DELAY 5000");

                statement.executeUpdate("CREATE SCHEMA IF NOT EXISTS mediathekview");
                statement.executeUpdate("SET SCHEMA mediathekview");

                statement.executeUpdate("DROP INDEX IF EXISTS IDX_DESC_ID");
                statement.executeUpdate("DROP INDEX IF EXISTS IDX_WEBSITE_LINKS_ID");

                statement.executeUpdate("DROP TABLE IF EXISTS mediathekview.description");
                statement.executeUpdate("DROP TABLE IF EXISTS mediathekview.website_links");

                statement.executeUpdate("CREATE TABLE IF NOT EXISTS description (id INTEGER NOT NULL PRIMARY KEY, desc VARCHAR(1024))");
                statement.executeUpdate("CREATE TABLE IF NOT EXISTS website_links (id INTEGER NOT NULL PRIMARY KEY, link VARCHAR(2048))");
                tm.commit();
            }
            catch (SQLException ex) {
                logger.error(ex);
            }
            logger.debug("initializeDatabase() done.");
        }
    }

}
