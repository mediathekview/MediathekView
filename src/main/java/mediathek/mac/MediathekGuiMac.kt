/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
import org.apache.commons.lang3.SystemUtils
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.awt.BorderLayout
import java.awt.Desktop
import java.awt.FlowLayout
import java.awt.Taskbar
import java.awt.desktop.QuitEvent
import java.awt.desktop.QuitResponse
import java.lang.foreign.*
import java.nio.file.Path
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
        TimerPool.timerPool.schedule({ checkForCorrectArchitecture() }, 15, TimeUnit.SECONDS)
    }

    @Throws(Throwable::class)
    private fun processorBrand(): String {
        val linker = Linker.nativeLinker()
        val sysctl = linker.defaultLookup().find("sysctlbyname").orElseThrow()
        val sysctlbyname = linker.downcallHandle(
            sysctl, FunctionDescriptor.of(
                ValueLayout.JAVA_INT,  // return type
                ValueLayout.ADDRESS,  // name (const char *)
                ValueLayout.ADDRESS,  // oldp (void *)
                ValueLayout.ADDRESS,  // oldlenp (size_t *)
                ValueLayout.ADDRESS,  // newp (const void *)
                ValueLayout.JAVA_LONG // newlen (size_t)
            )
        )

        val sysctlName = "machdep.cpu.brand_string"

        Arena.ofConfined().use { arena ->
            // Allocate memory for the size output
            val sizePtr = arena.allocate(ValueLayout.JAVA_LONG)
            // First call: get the size of the result buffer
            var res = sysctlbyname.invoke(
                arena.allocateFrom(sysctlName), MemorySegment.NULL,
                sizePtr, MemorySegment.NULL, 0L
            ) as Int
            if (res != 0) {
                throw RuntimeException("sysctlbyname failed to get size")
            }

            val len = sizePtr.get(ValueLayout.JAVA_LONG, 0)
            val buffer = arena.allocate(len)
            // Second call: get the actual value
            res = sysctlbyname.invoke(
                arena.allocateFrom(sysctlName), buffer, sizePtr, MemorySegment.NULL,
                0L
            ) as Int
            if (res != 0) {
                throw RuntimeException("sysctlbyname failed to get value")
            }

            return buffer.getString(0)
        }
    }

    /**
     * Check if MV is running "old" intel application on a new Mac with ARM cpu.
     * Issue warning if true as we have a faster alternative.
     */
    private fun checkForCorrectArchitecture() {
        logger.trace("Checking for correct JVM architecture on macOS...")
        try {
            val jvmBinaryArch = SystemUtils.OS_ARCH.lowercase(Locale.getDefault())
            val isAppleSilicon = processorBrand().lowercase().contains("apple")
            //println("isAppleSilicon: $isAppleSilicon")
            val isJVMIntel = jvmBinaryArch == "x86_64" || jvmBinaryArch == "amd64"
            //println("isJVMIntel: $isJVMIntel")

            if (isAppleSilicon && isJVMIntel) {
                logger.warn("⚠️ Running an Intel JVM on Apple Silicon. Consider using a native ARM64 JVM for better performance.")
                SwingUtilities.invokeLater {
                    val msg = "<html>Ihr Mac hat eine moderne Apple Silicon CPU.<br/>" +
                            "Sie nutzen jedoch eine MediathekView Version für Intel Prozessoren.<br/><br/>" +
                            "Um die Geschwindigkeit des Programms erheblich zu verbessern laden Sie bitte<br/>" +
                            "die passende <b>MediathekView für Apple Silicon</b> herunter.</html>"
                    JOptionPane.showMessageDialog(this, msg, Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
                }
            }
        } catch (e: Throwable) {
            logger.error("Failed to query processor brand", e)
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
                putClientProperty(FlatClientProperties.FULL_WINDOW_CONTENT_BUTTONS_PLACEHOLDER, "mac zeroInFullScreen")
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
        var exePath: Path? = null
        try {
            exePath = GuiFunktionenProgramme.findExecutableOnPath("MVShutdownHelper")
        } catch (_: Exception) {
            //try to find older shutdown binary
            logger.warn("Could not find MVShutdownHelper executable")
            try {
                exePath = GuiFunktionenProgramme.findExecutableOnPath("mv_shutdown_helper")
            }
            catch (_: Exception) {
                logger.error("Could not find old mv_shutdown_helper executable")
            }
        }
        if (exePath != null) {
            Runtime.getRuntime().exec(arrayOf("nohup", exePath.absolutePathString()))
        }
        else {
            logger.error("Could not shutdown mac as executable path is null")
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