package mediathek.gui.actions.import_actions;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.stage.FileChooser;
import mediathek.config.Konstanten;
import mediathek.tool.javafx.FXErrorDialog;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ImportOldReplacementListAction extends AbstractAction {
    public ImportOldReplacementListAction() {
        putValue(Action.NAME, "Alte Ersetzungstabelle...");
        putValue(Action.SHORT_DESCRIPTION, "Ermöglicht den Import der Ersetzungstabelle aus einer alten Konfigurationsdatei.");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        Platform.runLater(() -> {
            var fileChooser = new FileChooser();
            fileChooser.setTitle("Konfigurationsdatei öffnen");
            var selectedFile = fileChooser.showOpenDialog(null);
            if (selectedFile != null) {
                try {
                    var configReader = new OldConfigFileImporter();
                    var result = configReader.importAboBlacklist(selectedFile.getAbsolutePath(), false, false, true);
                    var alert = new ImportSettingsAlert(Alert.AlertType.INFORMATION);
                    String text = "Es wurden " + result.right + " Einträge importiert.";
                    alert.setContentText(text);
                    alert.showAndWait();
                } catch (Exception ex) {
                    Platform.runLater(() -> FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME,
                            "Fehler beim Importieren der Ersetzungstabelle",
                            "Es trat ein Fehler beim Import der Ersetzungstabelle auf.\nSollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                            ex));
                }
            } else {
                var alert = new ImportSettingsAlert(Alert.AlertType.WARNING);
                alert.setContentText("Der Import der Ersetzungstabelle wurde abgebrochen.");
                alert.showAndWait();
            }
        });
    }
}
