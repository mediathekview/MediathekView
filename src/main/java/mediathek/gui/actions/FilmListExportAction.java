package mediathek.gui.actions;

import javafx.application.Platform;
import javafx.concurrent.Task;
import javafx.scene.control.Alert;
import javafx.scene.control.ProgressBar;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import mSearch.filmlisten.writer.FilmListWriter;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import org.controlsfx.control.StatusBar;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.File;

/**
 * Exports the current film list to JSON file.
 */
public class FilmListExportAction extends AbstractAction {
    private final MediathekGui gui;

    public FilmListExportAction(MediathekGui gui) {
        super();
        this.gui = gui;

        putValue(NAME, "Filmliste exportieren...");
    }

    private void export(File selectedFile) {
        StatusBar bar = gui.getStatusBarController().getStatusBar();
        ProgressBar progBar = new ProgressBar();
        progBar.setProgress(0d);
        progBar.setVisible(true);
        bar.getRightItems().add(progBar);

        WorkerTask task = new WorkerTask(selectedFile);
        task.stateProperty().addListener((observableValue, oldState, newState) -> {
            switch (newState) {
                case SUCCEEDED:
                    bar.getRightItems().remove(progBar);
                    showSuccess();
                    break;
            }

        });
        progBar.progressProperty().bind(task.progressProperty());

        new Thread(task).start();
    }

    private void showSuccess() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle("MediathekView");
        alert.setHeaderText("Export der Filmliste");
        alert.setContentText("Der Export wurde erfolgreich beendet.");
        alert.initModality(Modality.APPLICATION_MODAL);
        alert.showAndWait();
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        setEnabled(false);

        Platform.runLater(() -> {
            FileChooser fileChooser = new FileChooser();
            fileChooser.setTitle("Filmliste sichern");
            fileChooser.setInitialFileName("filme");
            fileChooser.getExtensionFilters().addAll(
                    new FileChooser.ExtensionFilter("Unkomprimiert", "*.json"),
                    new FileChooser.ExtensionFilter("XZ Komprimiert (Standard)", "*.xz")
            );
            File selectedFile = fileChooser.showSaveDialog(null);
            if (selectedFile != null) {
                export(selectedFile);
            }
        });

        setEnabled(true);
    }

    class WorkerTask extends Task<Void> {
        private final File selectedFile;

        public WorkerTask(File selectedFile) {
            super();
            this.selectedFile = selectedFile;
        }

        @Override
        protected Void call() {
            FilmListWriter writer = new FilmListWriter();
            writer.disableEvents();
            writer.writeFilmList(selectedFile.getAbsolutePath(),
                    Daten.getInstance().getListeFilme(),
                    prog -> updateProgress(prog, 1d));
            return null;
        }
    }
}
