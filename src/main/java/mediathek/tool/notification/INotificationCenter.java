package mediathek.tool.notification;

import mediathek.tool.notification.thrift.NotificationMessage;

/**
 * Interface for the platform specific notification implementation
 */
public interface INotificationCenter {
    void displayNotification(NotificationMessage msg);
}
