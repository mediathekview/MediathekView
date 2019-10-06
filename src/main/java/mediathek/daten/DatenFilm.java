package mediathek.daten;

import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.sansorm.SqlClosure;
import mediathek.config.Daten;
import mediathek.tool.*;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.lang.ref.Cleaner;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicInteger;

/*
 * TODO: 8 Step plan
 * + DONE: Introduce Setters and Getters for each Field
 * + DONE: Each field gets an "get<FieldName>Title" to get the German title of the field (see DatenFilmCaptions)
 * + DONE: replace all access to arr to a getter or a setter respectively
 * 
 * - Make a Real Entity. Remove the Array
 * - Remove the Database Stuff from this Class to own Classes and a real OR-Mapping
 * - Finalize a Real Entity
 * - Write test cases for each Method
 * - Write JavaDoc for each of the new Methods that were split from this moloch
 */

public final class DatenFilm implements AutoCloseable, Comparable<DatenFilm>, Cloneable {
    public static final int FILM_NR = 0;      // wird vor dem Speichern gelöscht!
    public static final int FILM_SENDER = 1;  
    public static final int FILM_THEMA = 2;   
    public static final int FILM_TITEL = 3;   
    public static final int FILM_ABSPIELEN = 4; // no getter/setter access
    public static final int FILM_AUFZEICHNEN = 5; // no getter/setter access
    public static final int FILM_DATUM = 6;  
    public static final int FILM_ZEIT = 7;   
    public static final int FILM_DAUER = 8;  
    public static final int FILM_GROESSE = 9;  
    public static final int FILM_HD = 10; // no getter/setter access
    public static final int FILM_UT = 11; // no getter/setter access
    public static final int FILM_GEO = 12; // Geoblocking
    public static final int FILM_URL = 13; 
    public static final int FILM_ABO_NAME = 14; // wird vor dem Speichern gelöscht!
    public static final int FILM_DATUM_LONG = 15; // Datum als Long ABER Sekunden!!
    public static final int FILM_URL_HISTORY = 16; // set null only
    public static final int FILM_REF = 17; // no getter/setter access // Referenz auf this
    public static final int FILM_URL_HD = 18; 
    public static final int FILM_URL_SUBTITLE = 19; 
    public static final int FILM_URL_KLEIN = 20;
    public static final int MAX_ELEM = 21;
    //Indices without storage context !!!
    public static final int FILM_NEU = 21;
    /**
     * The database instance for all descriptions.
     */
    private final static AtomicInteger FILM_COUNTER = new AtomicInteger(0);
    private static final GermanStringSorter sorter = GermanStringSorter.getInstance();
    private static final Logger logger = LogManager.getLogger(DatenFilm.class);
    /**
     * The magic arr array.
     * Here all the film information with some minor exceptions.
     * Beware it is a dangerous string collection...
     */
    private final String[] arr = new String[MAX_ELEM];

    public DatenAbo getAbo() {
        return abo;
    }

    public void setAbo(DatenAbo abo) {
        this.abo = abo;
    }

    private DatenAbo abo = null;
    /**
     * film date stored IN SECONDS!!!
     */
    private DatumFilm datumFilm = new DatumFilm(0);
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
    private String websiteLink = null;
    private String description = null;
    private boolean livestream = false;
    public DatenFilm() {
        setupArr();

        filmSize = new MSLong(0); // Dateigröße in MByte
        databaseFilmNumber = FILM_COUNTER.getAndIncrement();
        writeFilmNumberToDatabase();

        setupDatabaseCleanup();
    }

    public DatumFilm getDatumFilm() {
        return datumFilm;
    }

    public String getUrlKlein() {
        return arr[FILM_URL_KLEIN];
    }

    public void setUrlKlein(String urlKlein) {
		arr[FILM_URL_KLEIN] = urlKlein;
	}

	public String getUrlHd() {
        return arr[FILM_URL_HD];
    }

    public void setUrlHd(String urlHd) {
		arr[FILM_URL_HD] = urlHd;
	}

	public String getAboName() {
        return arr[FILM_ABO_NAME];
    }

    public void setAboName(String aboName) {
		arr[FILM_ABO_NAME] = aboName;
	}

	public String getDatumLong() {
        return arr[FILM_DATUM_LONG];
    }

	public void setDatumLong(String datumLong) {
        arr[FILM_DATUM_LONG] = datumLong;
    }
	
    private void writeFilmNumberToDatabase() {
        if (MemoryUtils.isLowMemoryEnvironment()) {
            SqlClosure.sqlExecute(connection -> {
                PreparedStatement insertStatement = connection.prepareStatement("INSERT INTO mediathekview.film VALUES (?)");
                insertStatement.setInt(1, databaseFilmNumber);
                insertStatement.executeUpdate();

                return null;
            });
        }
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
    public MSLong getFilmSize() {
        return filmSize;
    }

    private void setupArr() {
        Arrays.fill(arr, "");

        clearUrlHistory();
    }

	private void clearUrlHistory() {
		arr[FILM_URL_HISTORY] = null;
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
            return SqlClosure.sqlExecute(connection -> {
                PreparedStatement statement = connection.prepareStatement("SELECT desc FROM mediathekview.description WHERE id = ?");
                statement.setLong(1, databaseFilmNumber);
                ResultSet rs = statement.executeQuery();

                return (rs.next() ? rs.getString(1) : "");
            });
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
                SqlClosure.sqlExecute(connection -> {
                    PreparedStatement mergeStatement = connection.prepareStatement("MERGE INTO mediathekview.description KEY(ID) VALUES (?,?)");
                    mergeStatement.setInt(1, databaseFilmNumber);
                    mergeStatement.setString(2, desc);
                    mergeStatement.executeUpdate();

                    return null;
                });
            } else
                description = desc;
        }
    }

    public String getWebsiteLink() {
        if (MemoryUtils.isLowMemoryEnvironment()) {
            return SqlClosure.sqlExecute(connection -> {
                PreparedStatement statement = connection.prepareStatement("SELECT link FROM mediathekview.website_links WHERE id = ?");
                statement.setLong(1, databaseFilmNumber);
                ResultSet rs = statement.executeQuery();
                return (rs.next() ? rs.getString(1) : "");
            });
        } else
            return websiteLink != null ? websiteLink : "";
    }

    public void setWebsiteLink(String link) {
        if (link != null && !link.isEmpty()) {
            if (MemoryUtils.isLowMemoryEnvironment()) {
                SqlClosure.sqlExecute(connection -> {
                    PreparedStatement mergeStatement = connection.prepareStatement("MERGE INTO mediathekview.website_links KEY(ID) VALUES (?,?)");
                    mergeStatement.setInt(1, databaseFilmNumber);
                    mergeStatement.setString(2, link);
                    mergeStatement.executeUpdate();

                    return null;
                });
            } else {
                websiteLink = link;
            }
        }
    }

    public boolean isNew() {
        return neuerFilm;
    }

    public void setNew(final boolean newFilm) {
        neuerFilm = newFilm;
    }

    public boolean isLivestream() {
        return livestream;
    }

    public void setLivestream(boolean val) {
        livestream = val;
    }

    public boolean hasSubtitle() {
        return !getUrlSubtitle().isEmpty();
    }

    public String getUrlFuerAufloesung(String aufloesung) {
        final String ret;
        switch (aufloesung) {
            case FilmResolution.AUFLOESUNG_KLEIN:
            case FilmResolution.AUFLOESUNG_HD:
                ret = getUrlNormalOrRequested(aufloesung);
                break;

            default://AUFLOESUNG_NORMAL
                ret = getUrl();
                break;
        }

        return ret;
    }

    public String getDateigroesse(String url) {
        if (url.equals(getUrl())) {
            return getSize();
        } else {
            return FileSize.laengeString(url);
        }
    }

    public String getIndex() {
        // liefert einen eindeutigen Index für die Filmliste
        // URL beim KiKa und ORF ändern sich laufend!
        return (getSender() + getThema()).toLowerCase() + getUrl();
    }

    public boolean isHD() {
        //Film gibts in HD
        return !getUrlHd().isEmpty();
    }

    @Override public Object clone() throws CloneNotSupportedException
    {
        DatenFilm ret = (DatenFilm) super.clone();
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

    /**
     * Set the film length.
     * Convert dauer string from format HH:MM:SS into seconds.
     */
    private void calculateFilmLength() {
        filmLength = parseTimeToSeconds();
    }

    private void setDatum() {
        if (!getSendeDatum().isEmpty()) {
            // nur dann gibts ein Datum
            try {
                final long l = Long.parseLong(getDatumLong());
                datumFilm = new DatumFilm(l * 1000); // sind SEKUNDEN!!
            } catch (Exception ex) {
                logger.debug("Datum: {}, Zeit: {}, Datum_LONG: {}", getSendeDatum(), getSendeZeit(), getDatumLong(), ex);
                datumFilm = new DatumFilm(0);
                setSendeDatum("");
                setSendeZeit("");
            }
        }
    }

    public void init() {
        filmSize = new MSLong(this);

        calculateFilmLength();

        setDatum();
    }

    // there is only one caller: DatenFilm.getUrlFuerAufloesung(String aufloesung)
    // 
    /* @param aufloesung: one of 
	/*     FilmResolution.AUFLOESUNG_HD
	 *     FilmResolution.AUFLOESUNG_KLEIN
	 *     FilmResolution.AUFLOESUNG_NORMAL
	 */
    private String getUrlNormalOrRequested(final String aufloesung) {
        // liefert die kleine normale URL oder die HD URL
        final String requestedUrl = getUrlByAufloesung(aufloesung);
		if (!requestedUrl.isEmpty()) {
            try {
                // Prüfen, ob Pipe auch in URL enthalten ist. Beim ZDF ist das nicht der Fall.
                final int indexPipe = requestedUrl.indexOf('|');
                if (indexPipe < 0) {
                    return requestedUrl;
                }

                final int i = Integer.parseInt(requestedUrl.substring(0, indexPipe));
                return getUrl().substring(0, i) + requestedUrl.substring(requestedUrl.indexOf('|') + 1);
            } catch (Exception e) {
                Log.errorLog(915236703, e, requestedUrl);
            }
        }
        return getUrl();
    }

    // there is only one caller: DatenFilm.getUrlNormalOrRequested(String aufloesung)
    // 
    /* @param aufloesung: one of 
	/*     FilmResolution.AUFLOESUNG_HD
	 *     FilmResolution.AUFLOESUNG_KLEIN
	 *     FilmResolution.AUFLOESUNG_NORMAL
	 */
	private String getUrlByAufloesung(final String aufloesung) {
		// nobody will call this function with a null value.
		// the only caller, getUrlFuerAufloesung, doesn't check for null
		// and hence, any NPE would occur there.
		switch (aufloesung)
		{
		case FilmResolution.AUFLOESUNG_HD: return getUrlHd();
		case FilmResolution.AUFLOESUNG_KLEIN: return getUrlKlein();
		
		case FilmResolution.AUFLOESUNG_NORMAL: // return getUrl(); // TODO preferably use intentional fall-through or duplication here?   
		default: return getUrl();
		}
	}

    public String getNr() {
    	return arr[FILM_NR];
    }

    public void setNr(final String nr) {
    	arr[FILM_NR] = nr;
    }
    
    public String getSender() {
    	return arr[FILM_SENDER];
    }

    public void setSender(String sender) {
        arr[FILM_SENDER] = sender;
    }

    public String getThema() {
        return arr[FILM_THEMA];
    }

    public void setThema(String thema) {
        arr[FILM_THEMA] = thema;
    }

    public String getTitle() {
        return arr[FILM_TITEL];
    }

    public void setTitle(String title) {
        arr[FILM_TITEL] = title;
    }

    public String getSendeDatum() {
        return arr[FILM_DATUM];
    }

    public void setSendeDatum(String sendeDatum) {
        arr[FILM_DATUM] = sendeDatum;
    }

    public String getSendeZeit() {
        return arr[FILM_ZEIT];
    }

    public void setSendeZeit(String sendeZeit) {
		arr[FILM_ZEIT] = sendeZeit;
	}

	public String getDauer() {
        return arr[FILM_DAUER];
    }

    public void setDauer(String dauer) {
		arr[FILM_DAUER] = dauer;
	}

	public String getSize() {
        return arr[FILM_GROESSE];
    }

    public void setSize(String size) {
        arr[FILM_GROESSE] = size;
    }

    public String getUrl() {
        return arr[FILM_URL];
    }

    public void setUrl(String url) {
        arr[FILM_URL] = url;
    }

    public String getUrlSubtitle() {
        return arr[FILM_URL_SUBTITLE];
    }

    public void setUrlSubtitle(String urlSubtitle) {
        arr[FILM_URL_SUBTITLE] = urlSubtitle;
    }
    
    public String getGeo() {
        return arr[FILM_GEO];
    }

    public void setGeo(String geo) {
        arr[FILM_GEO] = geo;
    }

    public static class Database {
        private Database() {
        }

        public static void closeDatabase() {
            HikariDataSource ds = PooledDatabaseConnection.getInstance().getDataSource();
            ds.close();
        }

        public static void createIndices() {
            logger.trace("Creating SQL indices");
            SqlClosure.sqlExecute(connection -> {
                Statement statement = connection.createStatement();
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_FILM_ID ON mediathekview.film (id)");
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_DESC_ID ON mediathekview.description (id)");
                statement.executeUpdate("CREATE INDEX IF NOT EXISTS IDX_WEBSITE_LINKS_ID ON mediathekview.website_links (id)");

                return null;
            });
            logger.trace("Finished creating SQL indices");
        }

        public static void initializeDatabase() {
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
