package mediathek.daten;

import com.zaxxer.sansorm.SqlClosure;

import java.sql.PreparedStatement;

public class DatenFilmCleanupTask implements Runnable {
    private final long filmNr;

    public DatenFilmCleanupTask(long film) {
        filmNr = film;
    }

    @Override
    public void run() {
        SqlClosure.sqlExecute(connection -> {
            PreparedStatement preparedStatement = connection.prepareStatement("DELETE FROM mediathekview.film where ID = ?");
            preparedStatement.setLong(1, filmNr);
            preparedStatement.executeUpdate();

            return null;
        });
    }
}
