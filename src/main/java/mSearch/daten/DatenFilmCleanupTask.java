package mSearch.daten;

import com.codahale.metrics.Counter;
import mediathek.config.Daten;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

import static com.codahale.metrics.MetricRegistry.name;

public class DatenFilmCleanupTask implements Runnable {
    private final int filmNr;

    private final Counter pendingJobs;

    public DatenFilmCleanupTask(int film) {
        filmNr = film;
        pendingJobs = Daten.getInstance().getMetricRegistry().counter(name(DatenFilmCleanupTask.class, "pending-cleanup-jobs"));
        pendingJobs.inc();
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

        pendingJobs.dec();
    }
}
