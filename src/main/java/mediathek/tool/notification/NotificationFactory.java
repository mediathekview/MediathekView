package mediathek.tool.notification;

import mediathek.tool.ApplicationConfiguration;

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

        // early access to ApplicationConfiguration.
        // at this point of time, ApplicationConfiguration.getConfiguration may be null 
        // (e.g. for the very first start of MV on this user profile, file settings.xml not yet available!)
        // to work around this, use null-safe getter to retrieve boolean config value
        if (ApplicationConfiguration.getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NATIVE_NOTIFICATIONS, false)
        && hasNativeNotifications())
            notificationCenter = new NativeNotificationCenter();
        else
            notificationCenter = new GenericNotificationCenter();

        return notificationCenter;
    }
}
