package mediathek.update;

import mediathek.config.Daten;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.io.Closeable;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * Perform one filmlist update automatically every 24 hours if program is running long enough.
 */
public class AutomaticFilmlistUpdate implements Closeable {
    private static final Logger logger = LogManager.getLogger(AutomaticFilmlistUpdate.class);
    private final IUpdateAction action;
    private ScheduledFuture<?> actionFuture;

    public AutomaticFilmlistUpdate(IUpdateAction action) {
        this.action = action;

    }

    private void reloadFilmList() {
        logger.debug("Automatic FilmList load started.");

        action.performUpdate();

        logger.debug("Automatic FilmList load finished.");
    }

    public void start() {
        logger.debug("AutomaticFilmlistUpdate Started.");
        actionFuture = Daten.getInstance().getTimerPool().scheduleWithFixedDelay(() -> SwingUtilities.invokeLater(this::reloadFilmList), 12L, 12L, TimeUnit.HOURS);
    }

    @Override
    public void close() {
        if (actionFuture != null) {
            if (!actionFuture.isDone())
                actionFuture.cancel(false);
        }
        logger.debug("AutomaticFilmlistUpdate closed.");
    }

    @FunctionalInterface
    public interface IUpdateAction {
        void performUpdate();
    }
}
