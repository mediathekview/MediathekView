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
import java.nio.file.Paths;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Database based seen history controller.
 */
public class NewSeenHistoryController implements AutoCloseable {
    private static final Logger logger = LogManager.getLogger();
    private static final String INSERT_STMT = "INSERT INTO seen_history(thema,titel,url) values (?,?,?)";
    private Connection connection;
    private PreparedStatement INSERT_STATEMENT;

    public NewSeenHistoryController(boolean readOnly) {
        try {
            var historyDbPath = Paths.get(Daten.getSettingsDirectory_String()).resolve("history.db");
            final var dbPathStr = historyDbPath.toAbsolutePath().toString();

            if (!Files.exists(historyDbPath)) {
                // create new empty database
                createEmptyDatabase(dbPathStr);
            }

            var conf = new SQLiteConfig();
            if (readOnly)
                conf.setReadOnly(true);
            conf.setEncoding(SQLiteConfig.Encoding.UTF8);
            conf.setLockingMode(SQLiteConfig.LockingMode.NORMAL);
            conf.setSharedCache(true);

            var ds = new SQLiteDataSource(conf);
            ds.setUrl("jdbc:sqlite:" + dbPathStr);
            // open and use database
            connection = ds.getConnection();
            connection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);

            INSERT_STATEMENT = connection.prepareStatement(INSERT_STMT);
        }
        catch (SQLException ex) {
            logger.error("ctor", ex);
            System.exit(99);
        }
    }

    public void removeAll() {
        //FIXME not implemented!
        throw new UnsupportedOperationException("not yet implemented!");
    }

    public synchronized void markUnseen(List<DatenFilm> list) {
        //FIXME not implemented!
        throw new UnsupportedOperationException("not yet implemented!");
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
        }
        catch (SQLException ex) {
            logger.error("markSeen", ex);
        }
    }

    public synchronized void markSeen(@NotNull DatenFilm film) {
        if (film.isLivestream())
            return;
        if (hasBeenSeen(film))
            return;

        try {
            writeToDatabase(film);
        }
        catch (SQLException ex) {
            logger.error("markSeen", ex);
        }
        // Update bookmarks with seen information
        //FIXME update bookmark for single update
        List<DatenFilm> list = new ArrayList<>();
        list.add(film);
        Daten.getInstance().getListeBookmarkList().updateSeen(true, list);

        sendChangeMessage();
    }

    public synchronized void writeManualEntry(String thema, String title, String url) {
        //FIXME not implemented!
        throw new UnsupportedOperationException("not yet implemented!");
    }

    public synchronized boolean hasBeenSeen(@NotNull DatenFilm film) {
        boolean result;
        try (var stmt = connection.prepareStatement("SELECT COUNT(url) AS total FROM seen_history WHERE url = ?")){
            stmt.setString(1, film.getUrl());
            if (!stmt.execute()) {
                //error occured
                result = false;
            }
            else {
                //success
                var rs = stmt.getResultSet();
                rs.next();
                final int total = rs.getInt("total");
                result = total != 0;
            }
        }
        catch (SQLException e) {
            logger.error("SQL error:", e);
            result = false;
        }

        return result;
    }


    /**
     * Creates a empty database and all table, indices necessary for use.
     *
     * @param dbPathStr The local file system path for storage
     * @throws SQLException Let the caller handle all errors
     */
    private void createEmptyDatabase(@NotNull String dbPathStr) throws SQLException {
        try (Connection tempConnection = DriverManager.getConnection("jdbc:sqlite:" + dbPathStr);
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

        if (connection != null)
            connection.close();
    }
}
