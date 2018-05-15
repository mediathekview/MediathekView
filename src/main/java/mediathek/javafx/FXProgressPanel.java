package mediathek.javafx;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.layout.BorderPane;
import javafx.scene.paint.Color;
import mSearch.tool.ApplicationConfiguration;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.Configuration;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class FXProgressPanel extends JFXPanel {
    public static final String CONFIG_STRING = "application.last_filmlist_size";
    private static final long serialVersionUID = -1753371357306901205L;
    private static final Logger logger = LogManager.getLogger(FXProgressPanel.class);
    private static final double STEP_SIZE = 0.01;
    private final BorderPane rootPane = new BorderPane();
    private final Daten daten = Daten.getInstance();
    private ProgressIndicator progress;
    private boolean infiniteProgress = false;
    private final double lastSize;

    public FXProgressPanel() {
        super();

        daten.getMessageBus().subscribe(this);

        Configuration config = ApplicationConfiguration.getConfiguration();

        lastSize = config.getInt(CONFIG_STRING, 0);
        if (lastSize == 0.0)
            infiniteProgress = true;

        Platform.runLater(this::initFX);
    }

    @Handler
    private void handleTimerEvent(TimerEvent msg) {
        Platform.runLater(() -> {
            final int currentSize = daten.getListeFilme().size();
            if (currentSize > 0 && lastSize != 0.0) {
                final double percent = currentSize / lastSize;
                increaseProgress(percent);
            }
        });
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