package mediathek.tool.notification

import javafx.application.Platform
import org.controlsfx.control.Notifications

/**
 * Implements notification based on controlsfx JavaFX implemention.
 */
class GenericNotificationCenter : INotificationCenter {
    override fun displayNotification(msg: NotificationMessage) {
        Platform.runLater {
            try {
                val genMsg = Notifications.create()
                genMsg.text(msg.message)
                genMsg.title(msg.title)
                when (msg.type) {
                    MessageType.INFO -> genMsg.showInformation()
                    MessageType.ERROR -> genMsg.showError()
                }
            } catch (ignored: NullPointerException) {
                //the generic version might fail to display a notification with NPE
            }
        }
    }

    override fun close() {}
}