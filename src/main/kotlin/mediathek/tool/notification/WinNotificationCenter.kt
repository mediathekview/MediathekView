package mediathek.tool.notification

import mediathek.tool.tray.SharedSystemTrayIcon
import java.awt.SystemTray
import java.awt.TrayIcon

class WinNotificationCenter : NotificationBackend {
    private val lifecycleLock = Any()
    private var trayIcon: TrayIcon? = null

    override fun publish(notification: NotificationMessage) {
        synchronized(lifecycleLock) {
            val currentTrayIcon = trayIcon ?: return
            val type = when (notification.type) {
                MessageType.INFO -> TrayIcon.MessageType.INFO
                MessageType.ERROR -> TrayIcon.MessageType.ERROR
            }
            currentTrayIcon.displayMessage(notification.title, notification.message, type)
        }
    }

    override fun close() {
        synchronized(lifecycleLock) {
            trayIcon ?: return
            trayIcon = null
            SharedSystemTrayIcon.release(this)
        }
    }

    init {
        check(SystemTray.isSupported()) { "System Tray is not supported" }
        trayIcon = SharedSystemTrayIcon.acquire(this)
    }
}
