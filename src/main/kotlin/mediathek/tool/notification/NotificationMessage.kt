package mediathek.tool.notification

data class NotificationMessage(
    val title: String,
    val message: String,
    val type: MessageType,
)
