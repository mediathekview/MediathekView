package mediathek.gui.tasks;

import mediathek.config.Daten;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class BlacklistFilterWorker extends SwingWorker<Void, Void> {

    public BlacklistFilterWorker(@NotNull JLabel progLabel, @NotNull JProgressBar progressBar) {
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
