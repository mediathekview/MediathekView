package mSearch.daten;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;

public class DatenFilmCleanupTask implements Runnable {
    private final long filmNr;

    public DatenFilmCleanupTask(long film) {
        filmNr = film;
    }

    @Override
    public void run() {
        try (Connection connection = PooledDatabaseConnection.getInstance().getConnection();
             PreparedStatement preparedStatement = connection.prepareStatement("DELETE FROM mediathekview.film where ID = ?")) {
            preparedStatement.setLong(1, filmNr);
            preparedStatement.executeUpdate();
        } catch (SQLException | IllegalStateException | NullPointerException ignored) {
        }
    }
}
