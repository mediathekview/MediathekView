package mediathek.gui.tasks;

import mediathek.config.Daten;
import org.jspecify.annotations.NonNull;

import javax.swing.*;

public class BlacklistFilterWorker extends SwingWorker<Void, Void> {

    public BlacklistFilterWorker(@NonNull JLabel progLabel, @NonNull JProgressBar progressBar) {
        SwingUtilities.invokeLater(() -> {
            progLabel.setText("Blacklist anwenden");
            progressBar.setIndeterminate(true);
        });
    }

    @Override
    protected Void doInBackground() {
        Daten.getInstance().getListeBlacklist().filterListe();

        return null;
    }
}
