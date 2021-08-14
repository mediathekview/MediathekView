package mediathek.tool.notification

import mediathek.config.Icons
import mediathek.config.Konstanten
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.awt.SystemTray
import java.awt.TrayIcon
import java.io.IOException

class WinNotificationCenter : INotificationCenter {
    private var trayIcon: TrayIcon? = null

    override fun displayNotification(msg: NotificationMessage) {
        if (trayIcon != null) {
            val type = when (msg.type) {
                MessageType.INFO -> TrayIcon.MessageType.INFO
                MessageType.ERROR -> TrayIcon.MessageType.ERROR
            }
            trayIcon!!.displayMessage(msg.title, msg.message, type)
        }
        else
            logger.error("TrayIcon is null, not displaying notification")
    }

    @Throws(IOException::class)
    override fun close() {
        if (trayIcon != null)
            SystemTray.getSystemTray().remove(trayIcon)
    }

    companion object {
        val logger: Logger = LogManager.getLogger()
    }

    init {
        if (!SystemTray.isSupported()) {
            logger.error("System Tray is not supported!")
        } else {
            val tray = SystemTray.getSystemTray()
            trayIcon = TrayIcon(Icons.ICON_TRAY, "MediathekView ${Konstanten.MVVERSION}")
            trayIcon!!.isImageAutoSize = true

            tray.add(trayIcon)
        }
    }
}