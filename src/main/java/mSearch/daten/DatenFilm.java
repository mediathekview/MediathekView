/*
 *   MediathekView
 *   Copyright (C) 2008 W. Xaver
 *   W.Xaver[at]googlemail.com
 *   http://zdfmediathk.sourceforge.net/
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mSearch.daten;

import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.sansorm.SqlClosure;
import mSearch.tool.*;
import mediathek.config.Daten;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.lang.ref.Cleaner;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.concurrent.atomic.AtomicInteger;

public class DatenFilm implements AutoCloseable, Comparable<DatenFilm> {
    public static final String AUFLOESUNG_NORMAL = "normal";
    public static final String AUFLOESUNG_HD = "hd";
    public static final String AUFLOESUNG_KLEIN = "klein";
    public static final String GEO_DE = "DE"; // nur in .. zu sehen
    public static final String GEO_AT = "AT";
    public static final String GEO_CH = "CH";
    public static final String GEO_EU = "EU";
    public static final String GEO_WELT = "WELT";
    //
    public static final int FILM_NR = 0; // wird vor dem Speichern gelöscht!
    public static final int FILM_SENDER = 1;
    public static final int FILM_THEMA = 2;
    public static final int FILM_TITEL = 3;
    public static final int FILM_ABSPIELEN = 4;
    public static final int FILM_AUFZEICHNEN = 5;
    public static final int FILM_DATUM = 6;
    public static final int FILM_ZEIT = 7;
    public static final int FILM_DAUER = 8;
    public static final int FILM_GROESSE = 9;
    public static final int FILM_HD = 10;
    public static final int FILM_UT = 11;
    public static final int FILM_GEO = 12;// Geoblocking
    public static final int FILM_URL = 13;
    public static final int FILM_ABO_NAME = 14;// wird vor dem Speichern gelöscht!
    public static final int FILM_URL_SUBTITLE = 15;
    public static final int FILM_URL_KLEIN = 16;
    public static final int FILM_URL_HD = 17;
    public static final int FILM_URL_HISTORY = 18;
    public static final int FILM_DATUM_LONG = 19;// Datum als Long ABER Sekunden!!
    public static final int FILM_REF = 20;// Referenz auf this
    public static final int MAX_ELEM = 21;

    //Indices without storage context !!!
    public static final int FILM_NEU = 21;

    //TODO get rid out of DatenFilm
    public static final String[] COLUMN_NAMES = new String[MAX_ELEM];
    /**
     * The database instance for all descriptions.
     */
    private final static AtomicInteger FILM_COUNTER = new AtomicInteger(0);
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static final Logger logger = LogManager.getLogger(DatenFilm.class);
    public static boolean[] spaltenAnzeigen = new boolean[MAX_ELEM];

    static {
        COLUMN_NAMES[FILM_NR] = "Nr";
        COLUMN_NAMES[FILM_SENDER] = "Sender";
        COLUMN_NAMES[FILM_THEMA] = "Thema";
        COLUMN_NAMES[FILM_TITEL] = "Titel";
        COLUMN_NAMES[FILM_ABSPIELEN] = "";
        COLUMN_NAMES[FILM_AUFZEICHNEN] = "";
        COLUMN_NAMES[FILM_DATUM] = "Datum";
        COLUMN_NAMES[FILM_ZEIT] = "Zeit";
        COLUMN_NAMES[FILM_DAUER] = "Dauer";
        COLUMN_NAMES[FILM_GROESSE] = "Größe [MB]";
        COLUMN_NAMES[FILM_HD] = "HD";
        COLUMN_NAMES[FILM_UT] = "UT";
        COLUMN_NAMES[FILM_GEO] = "Geo";
        COLUMN_NAMES[FILM_URL] = "URL";
        COLUMN_NAMES[FILM_ABO_NAME] = "Abo";
        COLUMN_NAMES[FILM_URL_SUBTITLE] = "URL Untertitel";
        COLUMN_NAMES[FILM_URL_KLEIN] = "URL Klein";
        COLUMN_NAMES[FILM_URL_HD] = "URL HD";
        COLUMN_NAMES[FILM_URL_HISTORY] = "URL History";
        COLUMN_NAMES[FILM_REF] = "Ref";
        COLUMN_NAMES[FILM_DATUM_LONG] = "DatumL";
    }

    static {
        Database.initializeDatabase();
    }

    /**
     * The magic arr array.
     * Here all the film information with some minor exceptions.
     * Beware it is a dangerous string collection...
     */
    public final String[] arr = new String[MAX_ELEM];
    /**
     * film date stored IN SECONDS!!!
     */
    public DatumFilm datumFilm = new DatumFilm(0);
    public Object abo = null;
    /**
     * File size in MByte
     */
    private MSLong filmSize;
    /**
     * Is this film an audio version? (aka Hörfassung)
     */
    private boolean isAudioVersion = false;
    /**
     * film length in seconds.
     */
    private long filmLength = 0;
    /**
     * Internal film number, used for storage in database
     */
    private int databaseFilmNumber;
    private boolean neuerFilm = false;
    private Cleaner.Cleanable cleaner = null;
    /**
     * Flag that this entry is in sign language (aka Gebärdensprache).
     */
    private boolean isSignLanguage = false;
    /**
     * Flag indicating a trailer, teaser or german Vorschau.
     */
    private boolean isTrailerTeaser = false;

    public DatenFilm() {
        setupArr();

        filmSize = new MSLong(0); // Dateigröße in MByte
        databaseFilmNumber = FILM_COUNTER.getAndIncrement();
        writeFilmNumberToDatabase();

        setupDatabaseCleanup();
    }

    private void writeFilmNumberToDatabase() {
        SqlClosure.sqlExecute(connection -> {
            PreparedStatement insertStatement = connection.prepareStatement("INSERT INTO mediathekview.film VALUES (?)");
            insertStatement.setInt(1, databaseFilmNumber);
            insertStatement.executeUpdate();

            return null;
        });
    }

    public boolean isTrailerTeaser() {
        return isTrailerTeaser;
    }

    public void setTrailerTeaser(boolean val) {
        isTrailerTeaser = val;
    }

    public boolean isAudioVersion() {
        return isAudioVersion;
    }

    public void setAudioVersion(boolean val) {
        isAudioVersion = val;
    }

    public boolean isSignLanguage() {
        return isSignLanguage;
    }

    public void setSignLanguage(boolean val) {
        isSignLanguage = val;
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
        final boolean useCleaner = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.DATABASE_USE_CLEANER_INTERFACE, false);
        if (useCleaner)
            installCleanupTask();
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
    public MSLong getFilmSize() {
        return filmSize;
    }

    private void setupArr() {
        for (int i = 0; i < MAX_ELEM; i++)
            arr[i] = "";
    }

    /**
     * Return the title of the film.
     */
    public String getTitle() {
        return arr[FILM_TITEL];
    }

    public void setTitle(String title) {
        arr[FILM_TITEL] = title;
    }

    public String getThema() {
        return arr[FILM_THEMA];
    }

    /**
     * Return the film size.
     *
     * @return size as a string
     */
    public String getSize() {
        return arr[FILM_GROESSE];
    }

    public String getSender() {
        return arr[FILM_SENDER];
    }

    public void setSender(String sender) {
        arr[DatenFilm.FILM_SENDER] = sender;
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
        return SqlClosure.sqlExecute(connection -> {
            PreparedStatement statement = connection.prepareStatement("SELECT desc FROM mediathekview.description WHERE id = ?");
            statement.setLong(1, databaseFilmNumber);
            ResultSet rs = statement.executeQuery();

            return (rs.next() ? rs.getString(1) : "");
        });
    }

    /**
     * Store description in database.
     * Performs an UPSERT as we may want to update the description later again.
     *
     * @param desc String to be stored.
     */
    public void setDescription(final String desc) {
        if (desc != null && !desc.isEmpty()) {
            SqlClosure.sqlExecute(connection -> {
                PreparedStatement mergeStatement = connection.prepareStatement("MERGE INTO mediathekview.description KEY(ID) VALUES (?,?)");
                mergeStatement.setInt(1, databaseFilmNumber);
                mergeStatement.setString(2, desc);
                mergeStatement.executeUpdate();

                return null;
            });
        }
    }

    public String getWebsiteLink() {
        return SqlClosure.sqlExecute(connection -> {
            PreparedStatement statement = connection.prepareStatement("SELECT link FROM mediathekview.website_links WHERE id = ?");
            statement.setLong(1, databaseFilmNumber);
            ResultSet rs = statement.executeQuery();
            return (rs.next() ? rs.getString(1) : "");
        });
    }

    public void setWebsiteLink(String link) {
        if (link != null && !link.isEmpty()) {
            SqlClosure.sqlExecute(connection -> {
                PreparedStatement mergeStatement = connection.prepareStatement("MERGE INTO mediathekview.website_links KEY(ID) VALUES (?,?)");
                mergeStatement.setInt(1, databaseFilmNumber);
                mergeStatement.setString(2, link);
                mergeStatement.executeUpdate();

                return null;
            });
        }
    }

    public boolean isNew() {
        return neuerFilm;
    }

    public void setNew(final boolean newFilm) {
        neuerFilm = newFilm;
    }

    public String getUrlSubtitle() {
        return arr[FILM_URL_SUBTITLE];
    }

    public boolean hasSubtitle() {
        //Film hat Untertitel
        return !arr[DatenFilm.FILM_URL_SUBTITLE].isEmpty();
    }

    public String getUrlFuerAufloesung(String aufloesung) {
        final String ret;
        switch (aufloesung) {
            case AUFLOESUNG_KLEIN:
                ret = getUrlNormalOrRequested(DatenFilm.FILM_URL_KLEIN);
                break;

            case AUFLOESUNG_HD:
                ret = getUrlNormalOrRequested(DatenFilm.FILM_URL_HD);
                break;

            default://AUFLOESUNG_NORMAL
                ret = arr[DatenFilm.FILM_URL];
                break;
        }

        return ret;
    }

    public String getDateigroesse(String url) {
        if (url.equals(arr[DatenFilm.FILM_URL])) {
            return arr[DatenFilm.FILM_GROESSE];
        } else {
            return FileSize.laengeString(url);
        }
    }

    public String getUrlHistory() {
        if (arr[DatenFilm.FILM_URL_HISTORY].isEmpty()) {
            return arr[DatenFilm.FILM_URL];
        } else {
            return arr[DatenFilm.FILM_URL_HISTORY];
        }
    }

    public String getIndex() {
        // liefert einen eindeutigen Index für die Filmliste
        // URL beim KiKa und ORF ändern sich laufend!
        return (getSender() + arr[FILM_THEMA]).toLowerCase() + getUrl();
    }

    public String getUrl() {
        return arr[DatenFilm.FILM_URL];
    }

    public boolean isHD() {
        //Film gibts in HD
        return !arr[DatenFilm.FILM_URL_HD].isEmpty();
    }

    public DatenFilm getCopy() {
        DatenFilm ret = new DatenFilm();
        System.arraycopy(this.arr, 0, ret.arr, 0, arr.length);
        ret.datumFilm = this.datumFilm;
        ret.databaseFilmNumber = this.databaseFilmNumber;
        ret.filmSize = this.filmSize;
        ret.filmLength = this.filmLength;
        ret.abo = this.abo;
        return ret;
    }

    @Override
    public int compareTo(@NotNull DatenFilm other) {
        int ret;
        if ((ret = sorter.compare(getSender(), other.getSender())) == 0) {
            return sorter.compare(arr[FILM_THEMA], other.arr[FILM_THEMA]);
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
     *
     * @return result in seconds or 0.
     */
    private long parseTimeToSeconds() {
        long seconds = 0;
        final String[] split = StringUtils.split(arr[FILM_DAUER], ':');

        try {
            seconds += Long.parseLong(split[0]) * 3600; //hour
            seconds += Long.parseLong(split[1]) * 60; //minute
            seconds += Long.parseLong(split[2]); //second
        } catch (Exception e) {
            seconds = 0;
        }

        return seconds;
    }

    /**
     * Set the film length.
     * Convert dauer string from format HH:MM:SS into seconds.
     */
    private void calculateFilmLength() {
        filmLength = parseTimeToSeconds();
    }

    private void setDatum() {
        if (!arr[DatenFilm.FILM_DATUM].isEmpty()) {
            // nur dann gibts ein Datum
            try {
                final long l = Long.parseLong(arr[DatenFilm.FILM_DATUM_LONG]);
                datumFilm = new DatumFilm(l * 1000); // sind SEKUNDEN!!
            } catch (Exception ex) {
                logger.debug("Datum: {}, Zeit: {}, Datum_LONG: {}", arr[DatenFilm.FILM_DATUM], arr[DatenFilm.FILM_ZEIT], arr[DatenFilm.FILM_DATUM_LONG], ex);
                datumFilm = new DatumFilm(0);
                arr[DatenFilm.FILM_DATUM] = "";
                arr[DatenFilm.FILM_ZEIT] = "";
            }
        }
    }

    public void init() {
        filmSize = new MSLong(this);

        calculateFilmLength();

        setDatum();
    }

    private String getUrlNormalOrRequested(int indexUrl) {
        // liefert die kleine normale URL
        if (!arr[indexUrl].isEmpty()) {
            try {
                // Prüfen, ob Pipe auch in URL enthalten ist. Beim ZDF ist das nicht der Fall.
                final int indexPipe = arr[indexUrl].indexOf('|');
                if (indexPipe < 0) {
                    return arr[indexUrl];
                }

                final int i = Integer.parseInt(arr[indexUrl].substring(0, indexPipe));
                return arr[DatenFilm.FILM_URL].substring(0, i) + arr[indexUrl].substring(arr[indexUrl].indexOf('|') + 1);
            } catch (Exception e) {
                Log.errorLog(915236703, e, arr[indexUrl]);
            }
        }
        return arr[DatenFilm.FILM_URL];
    }

    public static class Database {
        private Database() {
        }

        public static void closeDatabase() {
            HikariDataSource ds = PooledDatabaseConnection.getInstance().getDataSource();
            ds.close();
        }

        public static void createIndices() {
            logger.debug("Creating SQL indices");
            SqlClosure.sqlExecute(connection -> {
                Statement statement = connection.createStatement();
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_FILM_ID ON mediathekview.film (id)");
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_DESC_ID ON mediathekview.description (id)");
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_WEBSITE_LINKS_ID ON mediathekview.website_links (id)");

                return null;
            });
            logger.debug("Finished creating SQL indices");
        }

        private static void initializeDatabase() {
            logger.debug("initializeDatabase()");
            SqlClosure.sqlExecute(connection -> {
                Statement statement = connection.createStatement();
                if (!MemoryUtils.isLowMemoryEnvironment()) {
                    statement.executeUpdate("SET WRITE_DELAY 5000");
                    statement.executeUpdate("SET MAX_OPERATION_MEMORY 0");
                }

                statement.executeUpdate("SET LOG 0");

                statement.executeUpdate("CREATE SCHEMA IF NOT EXISTS mediathekview");
                statement.executeUpdate("SET SCHEMA mediathekview");

                statement.executeUpdate("DROP INDEX IF EXISTS IDX_FILM_ID");
                statement.executeUpdate("DROP INDEX IF EXISTS IDX_DESC_ID");
                statement.executeUpdate("DROP INDEX IF EXISTS IDX_WEBSITE_LINKS_ID");

                statement.executeUpdate("DROP TABLE IF EXISTS mediathekview.description");
                statement.executeUpdate("DROP TABLE IF EXISTS mediathekview.website_links");
                statement.executeUpdate("DROP TABLE IF EXISTS mediathekview.film");

                statement.executeUpdate("CREATE TABLE IF NOT EXISTS film (id INTEGER NOT NULL PRIMARY KEY)");
                statement.executeUpdate("CREATE TABLE IF NOT EXISTS description (id INTEGER NOT NULL PRIMARY KEY REFERENCES mediathekview.film ON DELETE CASCADE, desc VARCHAR(1024))");
                statement.executeUpdate("CREATE TABLE IF NOT EXISTS website_links (id INTEGER NOT NULL PRIMARY KEY REFERENCES mediathekview.film ON DELETE CASCADE, link VARCHAR(1024))");

                return null;
            });
            logger.debug("initializeDatabase() done.");
        }
    }

}
