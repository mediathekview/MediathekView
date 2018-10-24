package mediathek.update;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.TimeUnit;

/**
 * Perform one filmlist update automatically every 24 hours if program is running long enough.
 */
public class AutomaticFilmlistUpdate implements AutoCloseable {
    private static final Logger logger = LogManager.getLogger(AutomaticFilmlistUpdate.class);
    private final IUpdateAction action;
    /**
     * 24 hour timer for repeating update checks
     */
    private final Timer updateCheckTimer;

    public AutomaticFilmlistUpdate(IUpdateAction action) {
        this.action = action;
        final int load_delay = (int) TimeUnit.MILLISECONDS.convert(12, TimeUnit.HOURS);

        updateCheckTimer = new Timer(load_delay, e -> ForkJoinPool.commonPool().execute(this::reloadFilmList));
        updateCheckTimer.setRepeats(true);
        updateCheckTimer.setDelay(load_delay);
    }

    private void reloadFilmList() {
        logger.debug("Automatic FilmList load started.");

        action.performUpdate();

        logger.debug("Automatic FilmList load finished.");
    }

    public void start() {
        logger.debug("AutomaticFilmlistUpdate Started.");
        updateCheckTimer.start();
    }

    @Override
    public void close() {
        if (updateCheckTimer != null) {
            updateCheckTimer.stop();
        }
        logger.debug("AutomaticFilmlistUpdate closed.");
    }

    @FunctionalInterface
    public interface IUpdateAction {
        void performUpdate();
    }
}
