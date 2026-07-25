package mediathek.tool.notification

import mediathek.swing.SwingDispatch
import org.apache.logging.log4j.LogManager
import raven.toast.Notifications
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.SwingUtilities

/**
 * Displays in-application notifications through the Raven Swing toast implementation.
 */
class GenericNotificationCenter internal constructor(
    private val displayOnEdt: (NotificationMessage) -> Unit = ::displayWithRaven,
) : NotificationBackend {
    private val closed = AtomicBoolean()

    override fun publish(notification: NotificationMessage) {
        if (closed.get()) {
            return
        }

        SwingUtilities.invokeLater {
            if (closed.get()) {
                return@invokeLater
            }

            try {
                displayOnEdt(notification)
            } catch (exception: RuntimeException) {
                logger.error("Failed to display in-application notification", exception)
            }
        }
    }

    override fun close() {
        if (!closed.compareAndSet(false, true)) {
            return
        }
        if (!SwingUtilities.isEventDispatchThread()) {
            SwingDispatch.runAndWait("Close generic notification center") {}
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()

        private fun displayWithRaven(notification: NotificationMessage) {
            val ravenNotificationType: Notifications.Type = when (notification.type) {
                MessageType.INFO -> Notifications.Type.INFO
                MessageType.ERROR -> Notifications.Type.ERROR
            }
            val message = notification.title + "\n\n" + notification.message
            Notifications.getInstance().show(ravenNotificationType, Notifications.Location.TOP_RIGHT, message)
        }
    }
}
