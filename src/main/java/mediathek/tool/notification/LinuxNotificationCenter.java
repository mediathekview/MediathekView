package mediathek.tool.notification;

import es.blackleg.jlibnotify.LibNotify;
import es.blackleg.jlibnotify.core.DefaultLibNotifyLoader;
import mediathek.tool.notification.thrift.NotificationMessage;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.Closeable;

public class LinuxNotificationCenter implements INotificationCenter, Closeable {
    private static final Logger logger = LogManager.getLogger();
    private LibNotify libNotify;
    private boolean nativeSupport;

    public LinuxNotificationCenter() {
        try {
            libNotify = DefaultLibNotifyLoader.getInstance().load();
            libNotify.init("MediathekView");
            nativeSupport = true;

            var serverInfo = libNotify.getServerInfo();
            logger.debug("Name: {}", serverInfo.getName());
            logger.debug("Spec Version: {}", serverInfo.getSpecVersion());
            logger.debug("Vendor: {}", serverInfo.getVendor());
            logger.debug("Version: {}", serverInfo.getVersion());

            logger.debug("Server capabilities:");
            var caps = libNotify.getServerCapabilities();
            for (var s : caps)
                logger.debug("\t {}",s);
        }
        catch (UnsatisfiedLinkError | RuntimeException e) {
            nativeSupport = false;
            logger.error("failed to initialize libNotify",e);
        }
    }

    @Override
    public void displayNotification(NotificationMessage msg) {
        var notification = libNotify.createNotification(msg.title, msg.message,"dialog-information");
        libNotify.showNotification(notification);
    }

    public boolean hasNativeSupport() {
        return nativeSupport;
    }

    @Override
    public void close() {
        libNotify.unInit();
    }
}
