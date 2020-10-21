package mediathek.tool;

import com.sun.jna.platform.FileUtils;
import mediathek.config.Daten;
import mediathek.controller.history.MVUsedUrl;
import okhttp3.HttpUrl;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.LineNumberReader;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.*;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;

/**
 * Migrates the old history.txt into a sqlite database.
 */
public class SeenHistoryMigrator implements AutoCloseable {
    public static final String PRAGMA_ENCODING_STMT = "PRAGMA encoding='UTF-8'";
    public static final String CREATE_TABLE_STMT = "CREATE TABLE IF NOT EXISTS seen_history (id INTEGER PRIMARY KEY ASC, datum DATE NOT NULL DEFAULT (date('now')), thema TEXT, titel TEXT, url TEXT NOT NULL)";
    public static final String DROP_TABLE_STMT = "DROP TABLE IF EXISTS seen_history";
    public static final String INSERT_STMT = "INSERT INTO seen_history(datum,thema,titel,url) values (?,?,?,?)";
    public static final String CREATE_INDEX_STMT = "CREATE INDEX IF NOT EXISTS IDX_SEEN_HISTORY_URL ON seen_history(url)";
    public static final String DROP_INDEX_STMT = "DROP INDEX IF EXISTS IDX_SEEN_HISTORY_URL";
    private static final Logger logger = LogManager.getLogger();
    private static final String fileName = "history.txt";
    private final Path historyFilePath;
    private final List<MVUsedUrl> historyEntries = new ArrayList<>();

    public SeenHistoryMigrator() throws InvalidPathException {
        historyFilePath = Paths.get(Daten.getSettingsDirectory_String()).resolve(fileName);
    }

    /**
     * Do we need history migration?
     *
     * @return true if old text file exists, false otherwise
     */
    public boolean needsMigration() {
        return Files.exists(historyFilePath);
    }

    /**
     * Migrate from old text format to database.
     */
    public void migrate() throws Exception {
        logger.info("Start old history migration.");
        readOldEntries();
        if (!historyEntries.isEmpty()) {
            PreparedStatement insertStmt = null;
            Statement statement = null;
            Connection connection = null;
            try {
                // create database connection
                var historyDbPath = Paths.get(Daten.getSettingsDirectory_String()).resolve("history.db");
                final var dbPathStr = historyDbPath.toAbsolutePath().toString();
                connection = DriverManager.getConnection("jdbc:sqlite:" + dbPathStr);
                connection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);
                connection.setAutoCommit(false);

                statement = connection.createStatement();
                statement.setQueryTimeout(30);  // set timeout to 30 sec.
                statement.executeUpdate(PRAGMA_ENCODING_STMT);
                // drop old tables and indices if existent
                statement.executeUpdate(DROP_INDEX_STMT);
                statement.executeUpdate(DROP_TABLE_STMT);
                // create tables and indices
                statement.executeUpdate(CREATE_TABLE_STMT);
                statement.executeUpdate(CREATE_INDEX_STMT);

                //create entries in database
                insertStmt = connection.prepareStatement(INSERT_STMT);
                final var formatter = DateTimeFormatter.ofPattern("d.MM.yyyy");
                for (var entry : historyEntries) {
                    LocalDate locDate = LocalDate.parse(entry.getDatum(), formatter);
                    insertStmt.setObject(1, locDate);
                    insertStmt.setString(2, entry.getThema());
                    insertStmt.setString(3, entry.getTitel());
                    insertStmt.setString(4, entry.getUrl());
                    // write each entry into database
                    insertStmt.executeUpdate();
                }
                connection.commit();
            } catch (Exception ex) {
                try {
                    if (connection != null) {
                        connection.rollback();
                    }
                } catch (SQLException ignored) {
                }

                //rethrow so we can handle it at caller site
                throw ex;
            } finally {
                try {
                    if (insertStmt != null) {
                            insertStmt.close();
                    }

                    if (statement != null) {
                        statement.close();
                    }

                    if (connection != null)
                        connection.close();
                }
                catch (Exception e) {
                    logger.error("cleanup failed", e);
                }
            }
        }

        final var fileUtils = FileUtils.getInstance();
        if (fileUtils.hasTrash())
            fileUtils.moveToTrash(new File[]{historyFilePath.toFile()});
        else
            Files.deleteIfExists(historyFilePath);
        logger.info("Finished old history migration.");
    }

    private void readOldEntries() {
        logger.trace("Reading old entries");
        try (InputStream is = Files.newInputStream(historyFilePath);
             InputStreamReader isr = new InputStreamReader(is);
             LineNumberReader in = new LineNumberReader(isr)) {
            String entryLine;
            while ((entryLine = in.readLine()) != null) {
                MVUsedUrl oldHistoryEntry = MVUsedUrl.getUrlAusZeile(entryLine);
                final var url = oldHistoryEntry.getUrl();
                if (url.startsWith("rtmp:")) {
                    continue;
                }

                var okHttpUrl = HttpUrl.parse(url);
                if (okHttpUrl == null) {
                    continue;
                }

                // we cannot convert entries without date
                if (oldHistoryEntry.getDatum().isBlank())
                    continue;

                // so far so good, add to lists
                historyEntries.add(oldHistoryEntry);
            }
        } catch (Exception ex) {
            logger.error("listeBauen()", ex);
        }

        logger.trace("historyEntries size: {}", historyEntries.size());
    }

    @Override
    public void close() {
        historyEntries.clear();
    }
}
