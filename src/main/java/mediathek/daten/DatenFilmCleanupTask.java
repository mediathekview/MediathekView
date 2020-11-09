package mediathek.daten;

import java.sql.SQLException;

public class DatenFilmCleanupTask implements Runnable {
    private final long filmNr;

    public DatenFilmCleanupTask(long film) {
        filmNr = film;
    }

    @Override
    public void run() {
        try (var connection = PooledDatabaseConnection.INSTANCE.getDataSource().getConnection();
             var stmt = connection.createStatement()) {
            stmt.executeUpdate("DELETE FROM mediathekview.description WHERE id = " + filmNr);
            stmt.executeUpdate("DELETE FROM mediathekview.website_links WHERE id = " + filmNr);

        } catch (SQLException ignored) {
        }
    }
}
