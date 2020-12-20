package mediathek.gui.actions.import_actions;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.stage.FileChooser;
import mediathek.config.Konstanten;
import mediathek.tool.javafx.FXErrorDialog;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ImportOldAbosAction extends AbstractAction {
    public ImportOldAbosAction() {
        putValue(Action.NAME, "Alte Abos...");
        putValue(Action.SHORT_DESCRIPTION, "Ermöglicht den Import der Abos aus einer alten Konfigurationsdatei.");
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
                    var result = configReader.importAboBlacklist(selectedFile.getAbsolutePath(), true, false, false);
                    var alert = new ImportSettingsAlert(Alert.AlertType.INFORMATION);
                    String text = "Es wurden " + result.left + " Einträge importiert.";
                    alert.setContentText(text);
                    alert.showAndWait();
                } catch (Exception ex) {
                    Platform.runLater(() -> FXErrorDialog.showErrorDialog(Konstanten.PROGRAMMNAME,
                            "Fehler beim Importieren der Abos",
                            "Es trat ein Fehler beim Import der Abos auf.\n" +
                                    "Sollte dies häufiger auftreten kontaktieren Sie bitte das Entwicklerteam.",
                            ex));
                }
            } else {
                var alert = new ImportSettingsAlert(Alert.AlertType.WARNING);
                alert.setContentText("Der Import der Abos wurde abgebrochen.");
                alert.showAndWait();
            }
        });
    }
}
