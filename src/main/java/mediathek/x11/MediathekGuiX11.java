package mediathek.x11;

import mediathek.config.Konstanten;
import mediathek.config.MVConfig;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.notification.GenericNotificationCenter;
import mediathek.tool.notification.LinuxNotificationCenter;
import mediathek.tool.notification.NullNotificationCenter;
import org.apache.commons.lang3.SystemUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.awt.*;
import java.io.IOException;

public class MediathekGuiX11 extends MediathekGui {
    private static final Logger logger = LogManager.getLogger(MediathekGuiX11.class);

    public MediathekGuiX11() {
        setupX11WindowManagerClassName();
    }

    /**
     * Setup the X11 window manager WM_CLASS hint.
     * Enables e.g. GNOME to determine application name and to enable app specific functionality.
     */
    private void setupX11WindowManagerClassName() {
        try {
            var xToolkit = Toolkit.getDefaultToolkit();
            java.lang.reflect.Field awtAppClassNameField = xToolkit.getClass().getDeclaredField("awtAppClassName");
            awtAppClassNameField.setAccessible(true);
            awtAppClassNameField.set(xToolkit, Konstanten.PROGRAMMNAME);
        } catch (NoSuchFieldException | IllegalAccessException e) {
            logger.warn("Could not set awtAppClassName");
        }
    }

    @Override
    protected void setupNotificationCenter() {
        try {
            var notificationCenter = daten.notificationCenter();
            if (notificationCenter != null) {
                notificationCenter.close();
            }
        } catch (IOException e) {
            logger.error("error closing notification center", e);
        }

        final boolean showNotifications = config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS,true);
        // we need to figure if we have native support available
        var notificationCenter = new LinuxNotificationCenter();
        final boolean hasNativeSupport = notificationCenter.hasNativeSupport();
        config.setProperty(ApplicationConfiguration.APPLICATION_NATIVE_NOTIFICATIONS_SUPPORT, hasNativeSupport);

        //reset if we don´t have native support
        if (!hasNativeSupport) {
           config.setProperty(ApplicationConfiguration.APPLICATION_SHOW_NATIVE_NOTIFICATIONS,false);
        }
        if (!showNotifications) {
            daten.setNotificationCenter(new NullNotificationCenter());
        } else {
            if (config.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NATIVE_NOTIFICATIONS, false))
                daten.setNotificationCenter(notificationCenter);
            else
                daten.setNotificationCenter(new GenericNotificationCenter());
        }
    }

    @Override
    protected void shutdownComputer() {
        String strShutdownCommand;

        if (SystemUtils.IS_OS_LINUX || SystemUtils.IS_OS_FREE_BSD) {
            strShutdownCommand = MVConfig.get(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN); //strShutdownCommand = "shutdown -h now";
            if (strShutdownCommand.isEmpty()) {
                strShutdownCommand = Konstanten.SHUTDOWN_LINUX;
                MVConfig.add(MVConfig.Configs.SYSTEM_LINUX_SHUTDOWN, Konstanten.SHUTDOWN_LINUX);
            }
        } else {
            // unknown operating system
            logger.error("shutdown command is unknown for this operating system");
            return;
        }

        try {
            logger.info("Shutdown: {}", strShutdownCommand);
            Runtime.getRuntime().exec(strShutdownCommand);
        } catch (IOException ex) {
            logger.error(ex);
        }
    }
}
