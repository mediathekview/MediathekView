package mediathek.tool.notification;

import mSearch.tool.ApplicationConfiguration;
import org.apache.commons.lang3.SystemUtils;

public class NotificationFactory {
    private final static boolean bNativeNotifications;

    static {
        //TODO currently notifications native only for macOS
        bNativeNotifications = SystemUtils.IS_OS_MAC_OSX;
    }

    /**
     * Can we show native notifications on this platform?
     *
     * @return true when native notifications are supported, false otherwise
     */
    public static boolean hasNativeNotifications() {
        return bNativeNotifications;
    }

    public static INotificationCenter createNotificationCenter() {
        INotificationCenter notificationCenter;

        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NATIVE_NOTIFICATIONS, false))
            notificationCenter = new NativeNotificationCenter();
        else
            notificationCenter = new GenericNotificationCenter();

        return notificationCenter;
    }
}
