package mediathek.windows

import mediathek.mainwindow.MediathekGui
import mediathek.tool.notification.INotificationCenter
import mediathek.tool.notification.WinNotificationCenter
import mediathek.tool.threads.IndicatorThread
import org.apache.logging.log4j.LogManager
import java.io.IOException

class MediathekGuiWindows : MediathekGui() {
    private val logger = LogManager.getLogger()

    override fun createDarkModeToggleButton() {
        //not used on Windows 10+, we are creating a menu bar action here
    }

    override fun createMenuBar() {
        super.createMenuBar()
        createDarkModeMenuAction()
    }
    override fun shutdownComputer() {
        val strShutdownCommand = arrayOf("shutdown.exe", "-s", "-t", "0")
        try {
            logger.info("Windows shutdown binary called.")
            Runtime.getRuntime().exec(strShutdownCommand)
        } catch (ex: IOException) {
            logger.error(ex)
        }
    }

    override fun createProgressIndicatorThread(): IndicatorThread {
        return TaskbarIndicatorThread(this)
    }

    override fun getNotificationCenter(): INotificationCenter {
        return WinNotificationCenter()
    }
}
