package mSearch.daten;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

public class DatenFilmCleanupTask implements Runnable {
    private final int filmNr;

    public DatenFilmCleanupTask(int film) {
        filmNr = film;
    }

    @Override
    public void run() {
            try (Connection connection = PooledDatabaseConnection.getInstance().getConnection();
                 Statement statement = connection.createStatement()) {
                statement.addBatch("DELETE FROM mediathekview.website_links WHERE id = " + filmNr);
                statement.addBatch("DELETE FROM mediathekview.description WHERE id = " + filmNr);
                statement.executeBatch();
            } catch (SQLException | IllegalStateException | NullPointerException ignored) {
            }
    }
}
