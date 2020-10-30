package mediathek.tool.notification;

import mediathek.tool.notification.thrift.NotificationMessage;

import java.io.Closeable;

/**
 * Interface for the platform specific notification implementation
 */
public interface INotificationCenter extends Closeable {
    void displayNotification(NotificationMessage msg);
}
