package mediathek.daten;

import com.zaxxer.sansorm.SqlClosure;

public class DatenFilmCleanupTask implements Runnable {
    private final long filmNr;

    public DatenFilmCleanupTask(long film) {
        filmNr = film;
    }

    @Override
    public void run() {
        SqlClosure.sqlExecute(connection -> {
            var stmt = connection.createStatement();
            stmt.executeUpdate("DELETE FROM mediathekview.description WHERE id = " + filmNr);
            stmt.executeUpdate("DELETE FROM mediathekview.website_links WHERE id = " + filmNr);

            return null;
        });
    }
}
