package mediathek.gui.actions.export;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.HBox;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import mediathek.MediathekGui;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.VerticalSeparator;
import org.controlsfx.control.StatusBar;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.File;

/**
 * Exports the current film list to JSON file.
 */
public class FilmListExportAction extends AbstractAction {
    private final static String TITLE = "MediathekView";
    private final static String HEADER = "Export der Filmliste";
    private final MediathekGui gui;

    public FilmListExportAction(MediathekGui gui) {
        super();
        this.gui = gui;

        putValue(NAME, "Filmliste exportieren...");
    }

    private void export(File selectedFile) {
        StatusBar bar = gui.getStatusBarController().getStatusBar();
        ProgressBar progBar = new ProgressBar();

        HBox hb = new HBox();
        hb.setSpacing(4d);
        hb.getChildren().addAll(new VerticalSeparator(),
                new CenteredBorderPane(new Label("Exportiere FilmListe...")),
                new CenteredBorderPane(progBar));
        bar.getRightItems().add(hb);

        FilmListExportWorkerTask task = new FilmListExportWorkerTask(selectedFile);
        task.setOnSucceeded(e -> {
            bar.getRightItems().remove(hb);
            showSuccess();
        });
        task.setOnFailed(e -> {
            bar.getRightItems().remove(hb);
            showError();
        });

        progBar.progressProperty().bind(task.progressProperty());

        new Thread(task).start();
    }

    private void showError() {
        Alert alert = new Alert(Alert.AlertType.ERROR);
        alert.setTitle(TITLE);
        alert.setHeaderText(HEADER);
        alert.setContentText("Es gab einen Fehler beim Export der Filmliste.");
        alert.initModality(Modality.APPLICATION_MODAL);
        alert.showAndWait();
    }

    private void showSuccess() {
        Alert alert = new Alert(Alert.AlertType.INFORMATION);
        alert.setTitle(TITLE);
        alert.setHeaderText(HEADER);
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

}
