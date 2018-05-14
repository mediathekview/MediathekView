package mediathek.javafx;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.layout.BorderPane;
import javafx.scene.paint.Color;

public class FXProgressPanel extends JFXPanel {
    private static final long serialVersionUID = -1753371357306901205L;
    private ProgressIndicator progress;
    private final BorderPane rootPane = new BorderPane();
    private boolean infiniteProgress = false;

    private FXProgressPanel() {
        super();
        Platform.runLater(this::initFX);
    }

    public FXProgressPanel(boolean infiniteProgress) {
        this();
        this.infiniteProgress = infiniteProgress;
    }

    private void initFX() {
        setScene(createScene());
    }

    private Scene createScene() {
        Scene scene = new Scene(rootPane, Color.TRANSPARENT);

        if (infiniteProgress) {
            progress = new ProgressIndicator();
        } else {
            progress = new ProgressIndicator(0.0);
        }

        progress.setMaxSize(256, 256);
        rootPane.setCenter(progress);
        rootPane.setStyle("-fx-background-color: white");
        rootPane.setOpacity(0.3);

        return scene;
    }

    public void increaseProgress(double value) {
        progress.setProgress(value);
    }

    public boolean isInfiniteProgress() {
        return infiniteProgress;
    }
}