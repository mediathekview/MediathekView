package mediathek.windows

import mediathek.mainwindow.MainWindowDarkModeActionPlacement
import mediathek.mainwindow.MediathekGui
import mediathek.shutdown.WindowsComputerShutdown
import mediathek.tool.notification.WinNotificationCenter

class MediathekGuiWindows : MediathekGui(
    ::WinNotificationCenter,
    WindowsComputerShutdown(),
    ::WindowsDownloadProgressIndicator,
    MainWindowDarkModeActionPlacement.MENU_BAR,
)
