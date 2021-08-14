package mediathek.tool.notification

/**
 * A notification center which does nothing. Used when notifications are deactivated.
 */
class NullNotificationCenter : INotificationCenter {
    override fun displayNotification(msg: NotificationMessage) {}
    override fun close() {}
}