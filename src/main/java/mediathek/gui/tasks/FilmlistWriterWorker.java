package mediathek.gui.tasks;

import mediathek.config.Daten;
import mediathek.config.StandardLocations;
import mediathek.filmlisten.writer.FilmListWriter;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

public class FilmlistWriterWorker extends SwingWorker<Void, Integer> implements PropertyChangeListener {
    private final JProgressBar progressBar;

    public FilmlistWriterWorker(@NotNull JLabel progLabel, @NotNull JProgressBar progressBar) {
        this.progressBar = progressBar;

        addPropertyChangeListener(this);

        SwingUtilities.invokeLater(() -> {
            progLabel.setText("Schreibe Filmliste");
            progressBar.setIndeterminate(false);
            progressBar.setMinimum(0);
            progressBar.setMaximum(100);
            progressBar.setValue(0);
        });
    }

    @Override
    protected Void doInBackground() {
        FilmListWriter writer = new FilmListWriter(false);
        writer.writeFilmList(StandardLocations.getFilmlistFilePath(),
                Daten.getInstance().getListeFilme(),
                prog -> setProgress((int) (100.0 * prog)));

        return null;
    }

    @Override
    public void propertyChange(PropertyChangeEvent evt) {
        if (evt.getPropertyName().equalsIgnoreCase("progress"))
            SwingUtilities.invokeLater(() -> progressBar.setValue((int) evt.getNewValue()));
    }
}
