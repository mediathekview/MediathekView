package mediathek.tool.notification

import raven.toast.Notifications
import javax.swing.SwingUtilities

/**
 * Implements notification based on controlsfx JavaFX implemention.
 */
class GenericNotificationCenter : INotificationCenter {
    override fun displayNotification(msg: NotificationMessage) {
        SwingUtilities.invokeLater {
            try {
                val ravenNotificationType: Notifications.Type = when (msg.type) {
                    MessageType.INFO -> Notifications.Type.INFO
                    MessageType.ERROR -> Notifications.Type.ERROR
                }

                val message = msg.title + "\n\n" + msg.message
                Notifications.getInstance()
                    .show(ravenNotificationType, Notifications.Location.TOP_RIGHT, message)
            } catch (_: NullPointerException) {
                //the generic version might fail to display a notification with NPE
            }
        }
    }

    override fun close() {}
}