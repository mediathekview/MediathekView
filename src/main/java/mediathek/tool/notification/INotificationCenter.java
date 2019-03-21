package mediathek.tool.notification;

/**
 * Interface for the platform specific notification implementation
 */
public interface INotificationCenter {
    void displayNotification(NotificationMessage msg);
}
