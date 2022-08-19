package mediathek.mac

import com.formdev.flatlaf.util.SystemInfo
import mediathek.gui.actions.ShowAboutAction
import mediathek.gui.messages.DownloadFinishedEvent
import mediathek.gui.messages.DownloadStartEvent
import mediathek.gui.messages.InstallTabSwitchListenerEvent
import mediathek.mainwindow.MediathekGui
import mediathek.tool.notification.INotificationCenter
import mediathek.tool.notification.MacNotificationCenter
import mediathek.tool.threads.IndicatorThread
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.awt.Desktop
import java.awt.Taskbar
import java.awt.desktop.QuitEvent
import java.awt.desktop.QuitResponse
import java.io.IOException
import javax.swing.Box

class MediathekGuiMac : MediathekGui() {
    private val powerManager = OsxPowerManager()
    override fun installAdditionalHelpEntries() {
        //unused on macOS
    }

    override fun setupScrollBarWidth() {
        // unused on macOS
    }

    override fun initializeSystemTray() {
        //we don´t use it on macOS
    }

    override fun getNotificationCenter(): INotificationCenter {
        return MacNotificationCenter()
    }

    override fun shutdownComputer() {
        try {
            Runtime.getRuntime().exec("nohup bin/mv_shutdown_helper")
        } catch (e: IOException) {
            logger.error(e)
        }
    }

    override fun installMenuTabSwitchListener() {
        //do not use on OS X as it violates HIG...
    }

    @Handler
    override fun handleInstallTabSwitchListenerEvent(msg: InstallTabSwitchListenerEvent) {
        //do not use on OS X as it violates HIG...
    }

    override fun initMenus() {
        super.initMenus()
        setupUserInterfaceForOsx()
    }

    override fun createProgressIndicatorThread(): IndicatorThread {
        return OsxIndicatorThread()
    }

    override fun handleDownloadStart(msg: DownloadStartEvent) {
        super.handleDownloadStart(msg)
        powerManager.disablePowerManagement()
        setDownloadsBadge(numDownloadsStarted.get())
    }

    override fun handleDownloadFinishedEvent(msg: DownloadFinishedEvent) {
        super.handleDownloadFinishedEvent(msg)
        val numDownloads = numDownloadsStarted.get()
        if (numDownloads == 0) powerManager.enablePowerManagement()
        setDownloadsBadge(numDownloads)
    }

    /**
     * Set the OS X dock icon badge to the number of running downloads.
     *
     * @param numDownloads The number of active downloads.
     */
    private fun setDownloadsBadge(numDownloads: Int) {
        if (Taskbar.isTaskbarSupported()) {
            val taskbar = Taskbar.getTaskbar()
            if (taskbar.isSupported(Taskbar.Feature.ICON_BADGE_NUMBER)) {
                if (numDownloads > 0) taskbar.setIconBadge(numDownloads.toString()) else {
                    taskbar.setIconBadge("")
                }
            }
        }
    }

    /**
     * Setup the UI for OS X
     */
    private fun setupUserInterfaceForOsx() {
        val desktop = Desktop.getDesktop()

        desktop.disableSuddenTermination()
        if (desktop.isSupported(Desktop.Action.APP_QUIT_HANDLER)) {
            desktop.setQuitHandler { _: QuitEvent?, response: QuitResponse ->
                if (!quitApplication()) {
                    response.cancelQuit()
                } else {
                    //should never be reached from quitApplication()
                    response.performQuit()
                }
            }
        }
        if (desktop.isSupported(Desktop.Action.APP_ABOUT)) {
            desktop.setAboutHandler { ShowAboutAction().actionPerformed(null) }
        }

        if (desktop.isSupported(Desktop.Action.APP_PREFERENCES)) {
            desktop.setPreferencesHandler { settingsDialog.isVisible = true }
        }

        getRootPane().putClientProperty("apple.awt.windowTitleVisible", false)
        if (SystemInfo.isMacFullWindowContentSupported) {
            getRootPane().putClientProperty("apple.awt.fullWindowContent", true)
            getRootPane().putClientProperty("apple.awt.transparentTitleBar", true)
            commonToolBar.add(Box.createHorizontalStrut(70), 0)
        }
    }

    companion object {
        val logger: Logger = LogManager.getLogger()
    }
}