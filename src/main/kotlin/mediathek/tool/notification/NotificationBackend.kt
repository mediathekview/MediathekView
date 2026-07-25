package mediathek.tool.notification

/**
 * Platform-specific notification delivery backend.
 *
 * [close] is idempotent. Once it returns, this backend must not initiate delivery of queued notifications.
 */
interface NotificationBackend : NotificationPublisher, AutoCloseable
