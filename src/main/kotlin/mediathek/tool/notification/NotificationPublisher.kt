package mediathek.tool.notification

/**
 * Publishes best-effort application notifications.
 *
 * Implementations must accept calls from any thread and take ownership of the immutable notification value.
 */
fun interface NotificationPublisher {
    fun publish(notification: NotificationMessage)
}
