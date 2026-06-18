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
import kotlinx.coroutines.*
import kotlinx.coroutines.swing.Swing
import mediathek.config.Konstanten
import mediathek.gui.actions.ShowAboutAction
import mediathek.gui.messages.ShowSettingsDialogEvent
import mediathek.mainwindow.MacMainWindowMenuPolicy
import mediathek.mainwindow.MediathekGui
import mediathek.mainwindow.MainWindowTabPlacementController
import mediathek.mainwindow.MainWindowToolbarInstaller
import mediathek.shutdown.MacComputerShutdown
import mediathek.tool.MessageBus
import mediathek.tool.RuntimeArchitecture
import mediathek.tool.notification.MacNotificationCenter
import org.apache.logging.log4j.LogManager
import org.apache.logging.log4j.Logger
import java.awt.BorderLayout
import java.awt.Container
import java.awt.Desktop
import java.awt.FlowLayout
import java.awt.desktop.QuitEvent
import java.awt.desktop.QuitResponse
import java.lang.foreign.*
import javax.swing.JTabbedPane
import javax.swing.JOptionPane
import javax.swing.JPanel
import javax.swing.JToolBar
import kotlin.time.Duration.Companion.seconds

class MediathekGuiMac : MediathekGui(
    ::MacNotificationCenter,
    MacComputerShutdown(),
    { _ -> MacDownloadProgressIndicator() },
    MacMainWindowToolbarInstaller,
    MainWindowTabPlacementController(false),
    MacMainWindowMenuPolicy,
    false,
) {
    private val architectureCheckScope = CoroutineScope(SupervisorJob() + Dispatchers.IO)

    init {
        architectureCheckScope.launch {
            delay(15.seconds)
            checkForCorrectArchitecture()
        }
    }

    override fun dispose() {
        architectureCheckScope.cancel()
        super.dispose()
    }

    override fun shouldDisableF10MenuShortcut(): Boolean = false

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
    private suspend fun checkForCorrectArchitecture() {
        logger.trace("Checking for correct JVM architecture on macOS...")
        try {
            val isAppleSilicon = processorBrand().lowercase().contains("apple")
            //println("isAppleSilicon: $isAppleSilicon")

            if (isAppleSilicon && RuntimeArchitecture.isIntelOrAmd64Bit) {
                logger.warn("⚠️ Running an Intel JVM on Apple Silicon. Consider using a native ARM64 JVM for better performance.")
                withContext(Dispatchers.Swing) {
                    val msg = "<html>Ihr Mac hat eine moderne Apple Silicon CPU.<br/>" +
                            "Sie nutzen jedoch eine MediathekView Version für Intel Prozessoren.<br/><br/>" +
                            "Um die Geschwindigkeit des Programms erheblich zu verbessern laden Sie bitte<br/>" +
                            "die passende <b>MediathekView für Apple Silicon</b> herunter.</html>"
                    JOptionPane.showMessageDialog(this@MediathekGuiMac, msg, Konstanten.PROGRAMMNAME, JOptionPane.WARNING_MESSAGE)
                }
            }
        } catch (e: Throwable) {
            logger.error("Failed to query processor brand", e)
        }

    }

    override fun setupScrollBarWidth() {
        // unused on macOS
    }

    override fun initializeSystemTray() {
        //we don´t use it on macOS
    }

    override fun initMenus() {
        super.initMenus()
        setupUserInterfaceForOsx()
    }

    /**
     * Setup the UI for OS X
     */
    private fun setupUserInterfaceForOsx() {
        val desktop = Desktop.getDesktop()

        desktop.disableSuddenTermination()
        if (desktop.isSupported(Desktop.Action.APP_QUIT_HANDLER)) {
            desktop.setQuitHandler { _: QuitEvent?, response: QuitResponse ->
                quitApplication()
                response.cancelQuit()
            }
        }
        if (desktop.isSupported(Desktop.Action.APP_ABOUT)) {
            desktop.setAboutHandler { ShowAboutAction(this).actionPerformed(null) }
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

private object MacMainWindowToolbarInstaller : MainWindowToolbarInstaller {
    override fun install(contentPane: Container, tabbedPane: JTabbedPane, commonToolBar: JToolBar) {
        contentPane.add(MacToolBarPanel(commonToolBar), BorderLayout.PAGE_START)
    }
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
