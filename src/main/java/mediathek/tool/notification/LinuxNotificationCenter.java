package mediathek.tool.notification;

import es.blackleg.jlibnotify.JLibnotify;
import es.blackleg.jlibnotify.core.DefaultJLibnotifyLoader;
import es.blackleg.jlibnotify.exception.JLibnotifyInitException;
import es.blackleg.jlibnotify.exception.JLibnotifyLoadException;
import mediathek.config.Config;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Closeable;

public class LinuxNotificationCenter implements INotificationCenter, Closeable {
    private static final Logger logger = LogManager.getLogger();
    private JLibnotify libNotify;
    private boolean nativeSupport;

    public LinuxNotificationCenter() {
        try {
            var loader = new DefaultJLibnotifyLoader();
            libNotify = loader.load();
            libNotify.init("MediathekView");
            nativeSupport = true;

            var serverInfo = libNotify.getServerInfo();
            logger.info(serverInfo);

            if (Config.isDebugModeEnabled()) {
                logger.debug("Server capabilities:");
                var caps = libNotify.getServerCapabilities();
                for (var s : caps)
                    logger.debug("\t {}", s);
            }
        }
        catch (UnsatisfiedLinkError | RuntimeException | JLibnotifyLoadException | JLibnotifyInitException e) {
            nativeSupport = false;
            logger.error("Failed to initialize libNotify",e);
        }
    }

    @Override
    public void displayNotification(NotificationMessage msg) {
        var notification = libNotify.createNotification(msg.getTitle(), msg.message,"dialog-information");
        notification.show();
    }

    public boolean hasNativeSupport() {
        return nativeSupport;
    }

    @Override
    public void close() {
        libNotify.unInit();
    }
}
