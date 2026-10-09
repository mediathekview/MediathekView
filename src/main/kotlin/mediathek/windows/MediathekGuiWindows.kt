package mediathek.windows

import mediathek.config.Daten
import mediathek.mainwindow.MainWindowDarkModeActionPlacement
import mediathek.mainwindow.MediathekGui
import mediathek.shutdown.WindowsComputerShutdown
import mediathek.tool.notification.WinNotificationCenter

class MediathekGuiWindows(daten: Daten) : MediathekGui(
    daten,
    ::WinNotificationCenter,
    WindowsComputerShutdown(),
    { frame -> WindowsDownloadProgressIndicator(frame, daten.downloads) },
    MainWindowDarkModeActionPlacement.MENU_BAR,
)
