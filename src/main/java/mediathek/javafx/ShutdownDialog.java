package mediathek.javafx;

import javafx.application.Platform;
import javafx.event.Event;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mediathek.javafx.tool.JFXHiddenApplication;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ShutdownState;
import org.tbee.javafx.scene.layout.MigPane;

import java.util.EnumSet;

/**
 * Display a wait dialog with some status message to inform user what is happening currently.
 */
public class ShutdownDialog {
    private static final double MAXIMUM_STEPS = EnumSet.allOf(ShutdownState.class).size();
    private final MediathekGui gui;
    private Label lblStatusText;
    private Stage stage;
    private ProgressBar progress;
    private boolean hidden;
    private double curSteps;

    public ShutdownDialog(MediathekGui gui) {
        this.gui = gui;

        Platform.runLater(() -> {
            stage = new Stage();
            stage.setOnHidden(e -> hidden = true);
            stage.setAlwaysOnTop(true);
            stage.initOwner(JFXHiddenApplication.getPrimaryStage());
            stage.setResizable(false);
            stage.setOnCloseRequest(Event::consume);
            stage.initStyle(StageStyle.UNDECORATED);
            stage.setTitle("Programm beenden");
            stage.setScene(createScene());
        });

    }

    public void show() {
        Platform.runLater(() -> {
            stage.show();
            stage.centerOnScreen();
        });
        gui.setEnabled(false);
    }

    public void hide() {
        Platform.runLater(() -> {
            if (!hidden)
                stage.hide();
        });
        gui.setEnabled(true);
    }

    public void setStatusText(ShutdownState state) {
        Platform.runLater(() -> {
            curSteps++;
            final double p = (curSteps / MAXIMUM_STEPS);
            progress.setProgress(p);
            lblStatusText.setText(state.toString());
        });
/*
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
*/
    }

    private Scene createScene() {
        MigPane migPane = new MigPane(
                "hidemode 3",
                "[fill]" +
                        "[fill]",
                "[]" +
                        "[]" +
                        "[]");

        progress = new ProgressBar();
        progress.setProgress(0d);
        progress.setPrefWidth(450d);
        progress.setMinWidth(350d);

        migPane.add(new ProgressIndicator(), "cell 0 0 1 3");
        lblStatusText = new Label("Offene Operationen müssen noch beendet werden.");
        migPane.add(lblStatusText, "cell 1 0");
        migPane.add(progress, "cell 1 1");
        migPane.add(new Label(""), "cell 1 2");

        return new Scene(migPane);
    }
}
