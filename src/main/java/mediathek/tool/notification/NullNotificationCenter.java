package mediathek.tool.notification;

import mediathek.tool.notification.thrift.NotificationMessage;

/**
 * A notification center which does nothing. Used when notifications are deactivated.
 */
public class NullNotificationCenter implements INotificationCenter {
    @Override
    public void displayNotification(NotificationMessage msg) {
    }

    @Override
    public void close() {
    }
}
