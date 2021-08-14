package mediathek.windows;

import mediathek.mainwindow.MediathekGui;
import mediathek.tool.notification.INotificationCenter;
import mediathek.tool.notification.WinNotificationCenter;
import mediathek.tool.threads.IndicatorThread;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;

@SuppressWarnings("serial")
public class MediathekGuiWindows extends MediathekGui {
    private final Logger logger = LogManager.getLogger(MediathekGuiWindows.class);
    public MediathekGuiWindows() {
        super();
    }

    @Override
    protected void shutdownComputer() {
        final String strShutdownCommand = "shutdown.exe -s -t 0";

        try {
            logger.info("Shutdown: {}", strShutdownCommand);
            Runtime.getRuntime().exec(strShutdownCommand);
        } catch (IOException ex) {
            logger.error(ex);
        }
    }

    @Override
    protected IndicatorThread createProgressIndicatorThread() {
        return new TaskbarIndicatorThread(this);
    }

    @Override
    protected INotificationCenter getNotificationCenter() {
        return new WinNotificationCenter();
    }
}
