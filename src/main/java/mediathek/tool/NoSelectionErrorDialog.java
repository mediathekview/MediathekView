package mediathek.tool;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import mediathek.config.Konstanten;

public class NoSelectionErrorDialog {

    public static void show() {
        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.ERROR);
            alert.setTitle(Konstanten.PROGRAMMNAME);
            alert.setHeaderText("Befehl kann nicht ausgeführt werden.");
            alert.setContentText("Sie haben keinen Tabelleneintrag ausgewählt.");
            alert.show();
        });
    }
}
