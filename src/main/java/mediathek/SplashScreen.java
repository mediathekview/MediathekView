package mediathek;

import javafx.application.Platform;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.image.Image;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.tool.UIProgressState;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.net.URL;
import java.util.EnumSet;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class SplashScreen {
    private static final Logger LOG = LogManager.getLogger(SplashScreen.class);
    private static final double MAXIMUM_STEPS = EnumSet.allOf(UIProgressState.class).size() - 1d;

    @FXML
    private Label appName;

    @FXML
    private Label appVersion;

    @FXML
    private Label progressText;

    @FXML
    private ProgressBar progressBar;

    private double curSteps = 0d;
    private Stage window;

    /**
     * Return "modern" macOS string for mac instead of legacy "Mac OS X".
     * According to apple dev docs even "old" 10.6 is now named macOS.
     * @return "macOS" for mac otherwise the java OS name
     */
    private String getOsName() {
        final String osName;
        if (SystemUtils.IS_OS_MAC_OSX)
            osName = "macOS";
        else
            osName = SystemUtils.OS_NAME;

        return osName;
    }

    public void show() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            window = new Stage(StageStyle.UNDECORATED);
            window.getIcons().add(new Image("/mediathek/res/MediathekView.png"));

            URL url = getClass().getResource("/mediathek/res/programm/fxml/splashscreen.fxml");

            FXMLLoader fxmlLoader = new FXMLLoader();
            fxmlLoader.setLocation(url);
            fxmlLoader.setController(this);

            try {
                Scene scene = new Scene(fxmlLoader.load());
                window.setScene(scene);

                appName.setText(Konstanten.PROGRAMMNAME);
                appVersion.setText("Version: " + Konstanten.MVVERSION.toString() + " (" + getOsName() + ")");
                progressBar.prefWidthProperty().bind(scene.widthProperty());

                window.setScene(scene);
                window.show();
                window.centerOnScreen();
            } catch (IOException ioException) {
                LOG.error("Can't find/load the splash screen FXML description!", ioException);
            }
        });
    }

    public void close() {
        Daten.getInstance().getTimerPool().schedule(() -> JavaFxUtils.invokeInFxThreadAndWait(() -> {
            window.close();
            Main.splashScreen = Optional.empty(); // delete reference as we are not working anymore
        }),2, TimeUnit.SECONDS);
    }

    public void update(UIProgressState state) {
        Platform.runLater(() -> {
            curSteps++;
            final double p = (curSteps / MAXIMUM_STEPS);
            progressBar.setProgress(p);
            progressText.setText(state.toString());
        });
    }
}
