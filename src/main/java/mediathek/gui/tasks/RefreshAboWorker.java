package mediathek.gui.tasks;

import mediathek.config.Daten;
import org.jspecify.annotations.NonNull;

import javax.swing.*;

public class RefreshAboWorker extends SwingWorker<Void, Integer> {

    public RefreshAboWorker(@NonNull JLabel progLabel, @NonNull JProgressBar progressBar) {
        SwingUtilities.invokeLater(() -> {
            progLabel.setText("Abos eintragen");
            progressBar.setIndeterminate(true);
        });
    }

    @Override
    protected Void doInBackground() {
        var daten = Daten.getInstance();
        daten.getListeAbo().setAboFuerFilm(daten.getListeFilme(), false);

        return null;
    }
}
