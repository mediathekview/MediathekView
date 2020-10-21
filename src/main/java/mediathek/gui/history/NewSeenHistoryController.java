package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import mediathek.tool.SeenHistoryMigrator;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.sqlite.SQLiteConfig;
import org.sqlite.SQLiteDataSource;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.*;
import java.util.List;

/**
 * Database based seen history controller.
 */
public class NewSeenHistoryController implements AutoCloseable {
    private static final Logger logger = LogManager.getLogger();
    private static final String INSERT_SQL = "INSERT INTO seen_history(thema,titel,url) values (?,?,?)";
    private static final String DELETE_SQL = "DELETE FROM seen_history WHERE url = ?";
    private static final String SEEN_SQL = "SELECT COUNT(url) AS total FROM seen_history WHERE url = ?";
    private static final String MANUAL_INSERT_SQL = "INSERT INTO seen_history(thema, titel, url) VALUES (?,?,?)";
    private Connection connection;
    private PreparedStatement INSERT_STATEMENT;
    private SQLiteDataSource dataSource;
    private PreparedStatement DELETE_STATEMENT;
    private PreparedStatement SEEN_STATEMENT;
    private PreparedStatement MANUAL_INSERT_STATEMENT;

    public NewSeenHistoryController(boolean readOnly) {
        try {
            var historyDbPath = Paths.get(Daten.getSettingsDirectory_String()).resolve("history.db");
            if (!Files.exists(historyDbPath)) {
                // create new empty database
                createEmptyDatabase();
            }

            setupDataSource(readOnly, historyDbPath);
            // open and use database
            connection = dataSource.getConnection();
            connection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);

            INSERT_STATEMENT = connection.prepareStatement(INSERT_SQL);
            DELETE_STATEMENT = connection.prepareStatement(DELETE_SQL);
            SEEN_STATEMENT = connection.prepareStatement(SEEN_SQL);
            MANUAL_INSERT_STATEMENT = connection.prepareStatement(MANUAL_INSERT_SQL);
        } catch (SQLException ex) {
            logger.error("ctor", ex);
            System.exit(99);
        }
    }

    /**
     * Setup the SQLite data source.
     *
     * @param readOnly True for read-only, false for R/W
     * @param dbPath   Path to database location
     */
    private void setupDataSource(boolean readOnly, @NotNull Path dbPath) {
        SQLiteConfig conf = new SQLiteConfig();
        if (readOnly)
            conf.setReadOnly(true);
        conf.setEncoding(SQLiteConfig.Encoding.UTF8);
        conf.setLockingMode(SQLiteConfig.LockingMode.NORMAL);
        conf.setSharedCache(true);

        dataSource = new SQLiteDataSource(conf);
        dataSource.setUrl("jdbc:sqlite:" + dbPath.toAbsolutePath().toString());
    }

    /**
     * Remove all entries from the database.
     */
    public void removeAll() {
        try (var stmt = connection.createStatement()) {
            stmt.executeUpdate("DELETE FROM seen_history");
        } catch (SQLException ex) {
            logger.error("removeAll", ex);
        }

        sendChangeMessage();
    }

    public synchronized void markUnseen(List<DatenFilm> list) {
        try {
            for (var film : list) {
                DELETE_STATEMENT.setString(1, film.getUrl());
                DELETE_STATEMENT.executeUpdate();
            }
        } catch (SQLException ex) {
            logger.error("markUnseen", ex);
        }

        sendChangeMessage();
    }

    public synchronized void markSeen(List<DatenFilm> list) {
        try {
            for (var film : list) {
                //skip livestreams
                if (film.isLivestream())
                    continue;
                if (hasBeenSeen(film))
                    continue;

                writeToDatabase(film);
            }

            // Update bookmarks with seen information
            Daten.getInstance().getListeBookmarkList().updateSeen(true, list);

            //send one change for all...
            sendChangeMessage();
        } catch (SQLException ex) {
            logger.error("markSeen", ex);
        }
    }

    public synchronized void writeManualEntry(String thema, String title, String url) {
        try {
            MANUAL_INSERT_STATEMENT.setString(1, thema);
            MANUAL_INSERT_STATEMENT.setString(2, title);
            MANUAL_INSERT_STATEMENT.setString(3, url);
            MANUAL_INSERT_STATEMENT.executeUpdate();

            sendChangeMessage();
        } catch (SQLException ex) {
            logger.error("writeManualEntry", ex);
        }
    }

    public synchronized boolean hasBeenSeen(@NotNull DatenFilm film) {
        boolean result;
        ResultSet rs = null;
        try {
            SEEN_STATEMENT.setString(1, film.getUrl());
            rs = SEEN_STATEMENT.executeQuery();
            rs.next();
            final int total = rs.getInt("total");
            result = total != 0;
        } catch (SQLException e) {
            logger.error("SQL error:", e);
            result = false;
        } finally {
            if (rs != null) {
                try {
                    rs.close();
                } catch (SQLException ignore) {
                }
            }
        }

        return result;
    }


    /**
     * Creates a empty database and all table, indices necessary for use.
     *
     * @throws SQLException Let the caller handle all errors
     */
    private void createEmptyDatabase() throws SQLException {
        try (Connection tempConnection = dataSource.getConnection();
             Statement statement = tempConnection.createStatement()) {
            tempConnection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);

            statement.executeUpdate(SeenHistoryMigrator.PRAGMA_ENCODING_STMT);
            // drop old tables and indices if existent
            statement.executeUpdate(SeenHistoryMigrator.DROP_INDEX_STMT);
            statement.executeUpdate(SeenHistoryMigrator.DROP_TABLE_STMT);
            // create tables and indices
            statement.executeUpdate(SeenHistoryMigrator.CREATE_TABLE_STMT);
            statement.executeUpdate(SeenHistoryMigrator.CREATE_INDEX_STMT);
        }
    }

    /**
     * Write an entry to the database.
     *
     * @param film the film data to be written.
     * @throws SQLException .
     */
    private void writeToDatabase(@NotNull DatenFilm film) throws SQLException {
        INSERT_STATEMENT.setString(1, film.getThema());
        INSERT_STATEMENT.setString(2, film.getTitle());
        INSERT_STATEMENT.setString(3, film.getUrl());
        // write each entry into database
        INSERT_STATEMENT.executeUpdate();
    }

    /**
     * Send notification that the number of entries in the history has been changed.
     */
    private void sendChangeMessage() {
        Daten.getInstance().getMessageBus().publishAsync(new DownloadHistoryChangedEvent());
    }

    @Override
    public void close() throws Exception {
        if (INSERT_STATEMENT != null)
            INSERT_STATEMENT.close();

        if (DELETE_STATEMENT != null)
            DELETE_STATEMENT.close();

        if (SEEN_STATEMENT != null)
            SEEN_STATEMENT.close();

        if (MANUAL_INSERT_STATEMENT != null)
            MANUAL_INSERT_STATEMENT.close();

        if (connection != null)
            connection.close();
    }
}
