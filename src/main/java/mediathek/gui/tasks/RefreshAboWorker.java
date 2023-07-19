package mediathek.gui.tasks;

import mediathek.config.Daten;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class RefreshAboWorker extends SwingWorker<Void, Integer> {

    public RefreshAboWorker(@NotNull JLabel progLabel, @NotNull JProgressBar progressBar) {
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
