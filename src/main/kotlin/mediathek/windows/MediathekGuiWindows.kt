package mediathek.windows

import mediathek.config.Daten
import mediathek.mainwindow.MainWindowDarkModeActionPlacement
import mediathek.mainwindow.MediathekGui
import mediathek.shutdown.WindowsComputerShutdown
import mediathek.tool.notification.GenericNotificationCenter
import mediathek.tool.notification.NotificationBackend
import mediathek.tool.notification.WinNotificationCenter
import org.apache.logging.log4j.LogManager

private val logger = LogManager.getLogger(MediathekGuiWindows::class.java)

private fun createNotificationBackend(): NotificationBackend =
    try {
        WinNotificationCenter()
    } catch (exception: Exception) {
        logger.error("Failed to initialize native Windows notification center", exception)
        GenericNotificationCenter()
    }

class MediathekGuiWindows(daten: Daten) : MediathekGui(
    daten,
    ::createNotificationBackend,
    WindowsComputerShutdown(),
    { frame -> WindowsDownloadProgressIndicator(frame, daten.downloads) },
    MainWindowDarkModeActionPlacement.MENU_BAR,
)
