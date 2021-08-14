package mediathek.tool.notification

import java.io.Closeable

/**
 * Interface for the platform specific notification implementation
 */
interface INotificationCenter : Closeable {
    fun displayNotification(msg: NotificationMessage)
}