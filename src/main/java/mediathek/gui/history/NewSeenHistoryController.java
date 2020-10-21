package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.daten.DatenFilm;
import mediathek.gui.messages.history.DownloadHistoryChangedEvent;
import mediathek.tool.SeenHistoryMigrator;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

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
    private final Connection connection;
    private final PreparedStatement INSERT_STATEMENT;

    public NewSeenHistoryController() throws SQLException {
        var historyDbPath = Paths.get(Daten.getSettingsDirectory_String()).resolve("history.db");
        final var dbPathStr = historyDbPath.toAbsolutePath().toString();

        if (!Files.exists(historyDbPath)) {
            // create new empty database
            createEmptyDatabase(dbPathStr);
        }

        // open and use database
        connection = DriverManager.getConnection("jdbc:sqlite:" + dbPathStr);
        connection.setTransactionIsolation(Connection.TRANSACTION_SERIALIZABLE);

        INSERT_STATEMENT = connection.prepareStatement(INSERT_STMT);
    }

    public void removeAll() {
        throw new UnsupportedOperationException("not yet implemented!");
    }

    public void markSeen(List<DatenFilm> list) throws SQLException {
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

    public void markSeen(@NotNull DatenFilm film) throws SQLException {
        if (film.isLivestream())
            return;
        if (hasBeenSeen(film))
            return;

        writeToDatabase(film);

        // Update bookmarks with seen information
        //FIXME update bookmark for single update
        List<DatenFilm> list = new ArrayList<>();
        list.add(film);
        Daten.getInstance().getListeBookmarkList().updateSeen(true, list);

        sendChangeMessage();
    }

    public boolean hasBeenSeen(@NotNull DatenFilm film) {
        boolean result;
        try (var stmt = connection.prepareStatement("SELECT COUNT(url) FROM seen_history WHERE url = ?")){
            stmt.setString(1, "hello");
            if (!stmt.execute()) {
                //error occured
                result = false;
            }
            else {
                //success
                var rs = stmt.getResultSet();
                rs.first();
                //FIXME has been seen is incorrect!!!

                result = true;
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
