package mediathek.javafx;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressIndicator;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import org.tbee.javafx.scene.layout.MigPane;

/**
 * This will display a JFXPanel with a indefinite progress indicator and some status
 * messages used as a glass pane overlay during app termination.
 */
public class AppTerminationIndefiniteProgress extends JFXPanel {
    private final boolean willBeShutDown;
    private Label lblMessage;

    public AppTerminationIndefiniteProgress(boolean willbeShutDown) {
        super();
        this.willBeShutDown = willbeShutDown;

        Platform.runLater(this::initFX);
    }

    public void setMessage(String text) {
        Platform.runLater(() -> lblMessage.setText(text));
    }

    private void initFX() {
        setScene(createScene());
    }

    private Scene createScene() {
        var migPane = new MigPane(new LC().hideMode(3),
                new AC().fill().fill(),new AC());
        migPane.add(new ProgressIndicator(),new CC().cell(0,0).span(1,3));
        lblMessage = new Label("Warte auf Abschluss der Downloads...");
        migPane.add(lblMessage, new CC().cell(1,0));
        if (willBeShutDown) {
            Label lblShutdown = new Label("Der Rechner wird danach heruntergefahren.");
            migPane.add(lblShutdown, new CC().cell(1,1));
        }
        migPane.add(new Label("Sie können den Vorgang mit Escape abbrechen."), new CC().cell(1,2));

        return new Scene(migPane);
    }
}
