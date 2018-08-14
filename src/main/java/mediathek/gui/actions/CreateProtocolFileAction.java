package mediathek.gui.actions;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.stage.FileChooser;
import javafx.stage.Modality;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.tool.Logfile;
import mediathek.tool.MVFunctionSys;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.io.File;

public class CreateProtocolFileAction extends AbstractAction {

    public CreateProtocolFileAction() {
        super();
        putValue(NAME, "Protokolldatei erstellen...");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        Platform.runLater(() -> {
            FileChooser fileChooser = new FileChooser();
            fileChooser.setTitle("Protokoll sichern");
            fileChooser.setInitialFileName("protokoll");
            File selectedFile = fileChooser.showSaveDialog(null);
            if (selectedFile != null) {
                //FIXME create ZIP bundle with old log and new log
                if (!Logfile.LogDateiSchreiben(selectedFile.toString(), MVFunctionSys.getProgVersionString(), Daten.getSettingsDirectory_String(), Daten.listePset.getListProg(), MVConfig.getAll())) {
                    Alert alert = new Alert(Alert.AlertType.ERROR);
                    alert.setTitle("MediathekView");
                    alert.setHeaderText("Fehler beim Schreiben");
                    alert.setContentText("Protokolldatei konnte nicht geschrieben werden!");
                    alert.initModality(Modality.APPLICATION_MODAL);
                    alert.showAndWait();
                }
            }
        });
    }
}
