package mediathek.tool.notification

import airsquared.JMacNotification.NSUserNotification
import java.io.IOException

class MacNotificationCenter : INotificationCenter {
    override fun displayNotification(msg: NotificationMessage) {
        val notification = NSUserNotification()
        notification.title = msg.title
        notification.informativeText = msg.message
        notification.hasReplyButton = false
        notification.show()
    }

    @Throws(IOException::class)
    override fun close() {
    }
}