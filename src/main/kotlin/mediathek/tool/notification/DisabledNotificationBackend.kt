package mediathek.tool.notification

/**
 * No-op backend used while notifications are disabled or the service is closed.
 */
object DisabledNotificationBackend : NotificationBackend {
    override fun publish(notification: NotificationMessage) {}
    override fun close() {}
}
