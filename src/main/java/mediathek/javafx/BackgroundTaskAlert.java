package mediathek.javafx;

import javafx.application.Platform;
import javafx.scene.control.Alert;
import javafx.stage.Modality;

import java.util.Optional;

public class BackgroundTaskAlert {
    /**
     * An optional alert that may be displayed when there is a filmlist write running.
     */
    private Optional<Alert> waitAlert = Optional.empty();

    public void show() {
        createBackgroundTaskAlert();
    }

    private void createBackgroundTaskAlert() {
        Platform.runLater(() -> {
            Alert alert = new Alert(Alert.AlertType.INFORMATION);
            alert.setTitle("MediathekView");
            alert.setHeaderText("Abschluss der Hintergrund-Tasks");
            alert.setContentText("MediathekView muss auf den Abschluss der laufenden Hintergrund-Tasks warten.\n" +
                    "Dies kann einige Sekunden dauern");
            alert.initModality(Modality.APPLICATION_MODAL);
            alert.getButtonTypes().clear();

            waitAlert = Optional.of(alert);

            alert.show();
        });
    }

    public void hide() {
        closeBackgroundTaskAlert();
    }

    private void closeBackgroundTaskAlert() {
        waitAlert.ifPresent((alert) -> {
            Platform.runLater(alert::hide);
        });
    }
}
