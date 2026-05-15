package mediathek.windows

import mediathek.mainwindow.MediathekGui
import mediathek.shutdown.WindowsComputerShutdown
import mediathek.tool.notification.WinNotificationCenter

class MediathekGuiWindows : MediathekGui(
    ::WinNotificationCenter,
    WindowsComputerShutdown(),
    ::WindowsDownloadProgressIndicator,
) {
    override fun createDarkModeToggleButton() {
        //not used on Windows 10+, we are creating a menu bar action here
    }

    override fun createMenuBar() {
        super.createMenuBar()
        createDarkModeMenuAction()
    }

}
