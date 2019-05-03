package mediathek.tool.notification;

import mSearch.tool.ApplicationConfiguration;

public class NotificationFactory {
    private final static boolean bNativeNotifications;

    static {
        bNativeNotifications = false;//SystemUtils.IS_OS_MAC_OSX;
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

        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NATIVE_NOTIFICATIONS, false)
        && hasNativeNotifications())
            notificationCenter = new NativeNotificationCenter();
        else
            notificationCenter = new GenericNotificationCenter();

        return notificationCenter;
    }
}
