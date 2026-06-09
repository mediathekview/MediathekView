/*
 * Copyright (c) 2026 derreisende77.
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

package mediathek.x11

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.mainwindow.MediathekGui
import mediathek.shutdown.X11ComputerShutdown
import mediathek.tool.notification.GenericNotificationCenter
import mediathek.tool.notification.INotificationCenter
import mediathek.tool.notification.LinuxNotificationCenter
import org.apache.logging.log4j.LogManager
import java.awt.Toolkit

private val logger = LogManager.getLogger(MediathekGuiX11::class.java)

private fun createNotificationCenter(): INotificationCenter {
    val notificationCenter = LinuxNotificationCenter()
    if (notificationCenter.nativeSupport) {
        return notificationCenter
    }

    try {
        notificationCenter.close()
    } catch (e: Exception) {
        logger.error("Failed to close unsupported Linux notification center", e)
    }
    return GenericNotificationCenter()
}

class MediathekGuiX11 : MediathekGui(::createNotificationCenter, X11ComputerShutdown()) {
    init {
        setupX11WindowManagerClassName()
    }

    override fun createDarkModeToggleButton() {
        // we are using a menu item here
    }

    override fun createMenuBar() {
        super.createMenuBar()
        createDarkModeMenuAction()
    }

    override fun setupSystemTray() {
        val useTray = ApplicationConfiguration.getInstance().useTray
        if (!DesktopEnvDetector.trayIconSupported() && useTray) {
            logger.warn("Application tray icon is not supported on this platform, deactivating.")
            ApplicationConfiguration.getInstance().useTray = false
        }
        super.setupSystemTray()
    }

    /**
     * Setup the X11 window manager WM_CLASS hint.
     * Enables e.g. GNOME to determine application name and to enable app specific functionality.
     */
    private fun setupX11WindowManagerClassName() {
        try {
            val xToolkit = Toolkit.getDefaultToolkit()
            val awtAppClassNameField = xToolkit.javaClass.getDeclaredField("awtAppClassName")
            awtAppClassNameField.isAccessible = true
            awtAppClassNameField.set(xToolkit, Konstanten.PROGRAMMNAME)
        } catch (e: Exception) {
            logger.error("Could not set awtAppClassName", e)
        }
    }

}
