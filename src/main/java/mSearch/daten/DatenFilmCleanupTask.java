package mSearch.daten;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.concurrent.CompletableFuture;

public class DatenFilmCleanupTask implements Runnable {
    private final int filmNr;

    public DatenFilmCleanupTask(int film) {
        filmNr = film;
    }

    @Override
    public void run() {
        CompletableFuture.runAsync(() -> {
            try (Connection connection = PooledDatabaseConnection.getInstance().getConnection();
                 Statement statement = connection.createStatement()) {
                //System.out.println("state cleaner called for DatenFilm + " + filmNr);
                statement.setPoolable(true);
                statement.addBatch("DELETE FROM mediathekview.website_links WHERE id = " + filmNr);
                statement.addBatch("DELETE FROM mediathekview.description WHERE id = " + filmNr);
                statement.executeBatch();
            } catch (SQLException e) {
                e.printStackTrace();
            }
        });
    }
}
