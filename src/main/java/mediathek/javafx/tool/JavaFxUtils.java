package mediathek.javafx.tool;

import javafx.application.Platform;
import javafx.scene.paint.Color;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;

public class JavaFxUtils {
    private static final Logger logger = LogManager.getLogger(JavaFxUtils.class);

    /**
     * Invoke on the JavaFx thread and wait for it to return. Be very careful
     * with this because this can cause deadlocks.
     */
    static public void invokeInFxThreadAndWait(final Runnable run) {
        if (Platform.isFxApplicationThread()) {
            run.run();
            return;
        }

        try {
            FutureTask<Void> future = new FutureTask<>(run, null);
            Platform.runLater(future);
            future.get();
        } catch (ExecutionException | InterruptedException e) {
            logger.error("invokeInFxThreadAndWait() failed", e);
        }
    }

    /**
     * Convert a swing color to JavaFX.
     * @param awtColor the swing color.
     * @return the JavaFX color.
     */
    public static Color toFXColor(@NotNull java.awt.Color awtColor) {
        int r = awtColor.getRed();
        int g = awtColor.getGreen();
        int b = awtColor.getBlue();
        int a = awtColor.getAlpha();
        double opacity = a / 255.0;
        return javafx.scene.paint.Color.rgb(r, g, b, opacity);
    }
}
