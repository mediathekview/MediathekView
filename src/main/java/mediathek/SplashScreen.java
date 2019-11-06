package mediathek;

import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.javafx.tool.JavaFxUtils;
import mediathek.tool.UIProgressState;
import org.apache.commons.lang3.SystemUtils;

import java.util.EnumSet;
import java.util.Optional;
import java.util.concurrent.TimeUnit;

public class SplashScreen {
    private final ProgressBar progress;
    private final Label progressText;
    private final Label versionText;
    private final double MAXIMUM_STEPS = EnumSet.allOf(UIProgressState.class).size() - 1;
    private final Image image = new Image("/splash.png");
    private Stage window;
    private double curSteps = 0d;

    public SplashScreen() {
        progress = new ProgressBar();
        progress.setId("progressBar");

        progressText = new Label();
        progressText.setId("progressText");

        versionText = new Label(Konstanten.MVVERSION.toString());
        versionText.setId("progressText");
    }

    private ImageView createImageView() {
        ImageView imageView = new ImageView();
        imageView.setSmooth(true);
        imageView.setPreserveRatio(true);
        imageView.setImage(image);
        imageView.setFitHeight(image.getHeight() / 2d);

        return imageView;
    }

    public void show() {
        JavaFxUtils.invokeInFxThreadAndWait(() -> {
            window = new Stage(StageStyle.UNDECORATED);
            window.getIcons().add(new Image("/mediathek/res/MediathekView.png"));

            VBox vb = new VBox();
            vb.setPadding(new Insets(5,5,5,5));

            progress.setProgress(0d);
            progress.setPrefWidth(image.getWidth());

            Label appName = new Label(Konstanten.PROGRAMMNAME + " für " + SystemUtils.OS_NAME.replace("Mac OS X","macOS"));
            appName.setId("appName");

            vb.getChildren().addAll(
                    createImageView(),
                    new Label(),
                    appName,
                    versionText,
                    new Label(),
                    progressText,
                    progress
            );
            vb.setId("vbox");

            Scene scene = new Scene(vb);
            scene.setFill(null);
            scene.getStylesheets().add("css/splashscreen/splashscreen.css");

            window.setScene(scene);
            window.setAlwaysOnTop(true);
            window.show();
            window.centerOnScreen();
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
            progress.setProgress(p);
            progressText.setText(state.toString());
        });
    }
}
