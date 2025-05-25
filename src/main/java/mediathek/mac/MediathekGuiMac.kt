package mediathek.mac

import com.formdev.flatlaf.FlatClientProperties
import com.formdev.flatlaf.util.SystemInfo
import mediathek.config.Konstanten
import mediathek.gui.actions.ShowAboutAction
import mediathek.gui.messages.DownloadFinishedEvent
import mediathek.gui.messages.DownloadStartEvent
import mediathek.gui.messages.InstallTabSwitchListenerEvent
import mediathek.gui.messages.ShowSettingsDialogEvent
import mediathek.mainwindow.MediathekGui
import mediathek.tool.GuiFunktionenProgramme
import mediathek.tool.MessageBus
import mediathek.tool.notification.INotificationCenter
import mediathek.tool.notification.MacNotificationCenter
import mediathek.tool.threads.IndicatorThread
import mediathek.tool.timer.TimerPool
import net.engio.mbassy.listener.Handler
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.awt.BorderLayout
import java.awt.Desktop
import java.awt.FlowLayout
import java.awt.Taskbar
import java.awt.desktop.QuitEvent
import java.awt.desktop.QuitResponse
import java.io.IOException
import java.util.*
import java.util.concurrent.TimeUnit
import javax.swing.JOptionPane
import javax.swing.JPanel
import javax.swing.JToolBar
import javax.swing.SwingUtilities
import kotlin.io.path.absolutePathString

class MediathekGuiMac : MediathekGui {
    private val powerManager = OsxPowerManager()

    constructor() : super() {
        TimerPool.timerPool.schedule({checkForCorrectArchitecture()}, 15, TimeUnit.SECONDS)
    }

    /**
     * Check if MV is running "old" intel application on a new Mac with ARM cpu.
     * Issue warning if true as we have a faster alternative.
     */
    private fun checkForCorrectArchitecture() {
        logger.trace("Checking for correct JVM architecture on macOS...")
        val osName = System.getProperty("os.name").lowercase(Locale.getDefault())
        val osArch = System.getProperty("os.arch").lowercase(Locale.getDefault()) // native arch
        val jvmBinaryArch = System.getProperty("os.arch").lowercase(Locale.getDefault())

        val isAppleSilicon = osName.contains("mac") && osArch.contains("aarch64")
        val isJVMIntel = jvmBinaryArch == "x86_64" || jvmBinaryArch == "amd64"

        if (isAppleSilicon && isJVMIntel) {
            logger.warn("⚠️ Running an Intel JVM on Apple Silicon. Consider using a native ARM64 JVM for better performance.")
            SwingUtilities.invokeLater {
                val msg = "<html>Ihr Mac hat eine moderne Apple Silicon CPU.<br/>" +
                        "Sie nutzen jedoch eine MediathekView Version für Intel Prozessoren.<br/><br/>" +
                        "Um die Geschwindigkeit des Programms erheblich zu verbessern laden Sie bitte<br/>" +
                        "die passende <b>MediathekView für Apple Silicon</b> herunter.</html>"
                JOptionPane.showMessageDialog(this,msg, Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
            }
        }
    }

    override fun resetTabPlacement() {
        // do not reset tab placement as it is not necessary...
    }

    override fun addQuitMenuItem() {
        //using native handler instead
    }

    override fun addSettingsMenuItem() {
        //using native handler instead
    }
    override fun setToolBarProperties() {
        //not used on macOS
    }

    override fun configureTabPlacement() {
        // do not configure as it interferes with installToolBar and is not necessary...
    }

    private class MacToolBarPanel(commonToolBar: JToolBar) : JPanel() {
        private class MacFullWindowPlaceHolder : JPanel() {
            init {
                layout = FlowLayout()
                putClientProperty( FlatClientProperties.FULL_WINDOW_CONTENT_BUTTONS_PLACEHOLDER, "mac zeroInFullScreen" )
            }
        }

        init {
            layout = BorderLayout()
            add(MacFullWindowPlaceHolder(), BorderLayout.WEST)
            add(commonToolBar, BorderLayout.CENTER)
        }
    }
    override fun installToolBar() {
        contentPane.add(MacToolBarPanel(commonToolBar), BorderLayout.PAGE_START)
    }

    override fun createFontMenu() {
        //unused on macOS
    }

    override fun addFontMenu() {
        //unused on macOS
    }

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
            val exePath = GuiFunktionenProgramme.findExecutableOnPath("mv_shutdown_helper")
            Runtime.getRuntime().exec(arrayOf("nohup", exePath.absolutePathString()))
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
            desktop.setPreferencesHandler {
                MessageBus.messageBus.publishAsync(ShowSettingsDialogEvent())
            }
        }

        val rootPane = getRootPane()
        rootPane.putClientProperty("apple.awt.windowTitleVisible", false)
        if (SystemInfo.isMacFullWindowContentSupported) {
            rootPane.putClientProperty("apple.awt.fullWindowContent", true)
            rootPane.putClientProperty("apple.awt.transparentTitleBar", true)
        }
    }

    companion object {
        val logger: Logger = LogManager.getLogger()
    }
}