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

import mediathek.config.Daten
import mediathek.config.Konstanten
import mediathek.mainwindow.MainWindowDarkModeActionPlacement
import mediathek.mainwindow.MediathekGui
import mediathek.shutdown.X11ComputerShutdown
import mediathek.tool.notification.GenericNotificationCenter
import mediathek.tool.notification.LinuxNotificationCenter
import mediathek.tool.notification.NotificationBackend
import org.apache.logging.log4j.LogManager
import java.awt.Toolkit

private val logger = LogManager.getLogger(MediathekGuiX11::class.java)

private fun createNotificationBackend(): NotificationBackend {
    return try {
        LinuxNotificationCenter.create()
    } catch (exception: LinkageError) {
        logger.error("Failed to initialize native Linux notification center", exception)
        GenericNotificationCenter()
    } catch (exception: Exception) {
        logger.error("Failed to initialize native Linux notification center", exception)
        GenericNotificationCenter()
    }
}

class MediathekGuiX11(daten: Daten) : MediathekGui(
    daten,
    ::createNotificationBackend,
    X11ComputerShutdown(),
    MainWindowDarkModeActionPlacement.MENU_BAR,
    X11MainWindowSystemTrayController,
) {
    init {
        setupX11WindowManagerClassName()
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
