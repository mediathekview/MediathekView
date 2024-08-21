package mediathek.javafx.tool;

import javafx.application.Platform;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

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
}
