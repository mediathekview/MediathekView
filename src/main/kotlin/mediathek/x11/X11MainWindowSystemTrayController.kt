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

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.MVTray
import mediathek.mainwindow.DefaultMainWindowSystemTrayController
import mediathek.mainwindow.MainWindowSystemTrayController
import mediathek.mainwindow.TrayHost
import org.apache.logging.log4j.LogManager

object X11MainWindowSystemTrayController : MainWindowSystemTrayController {
    private val logger = LogManager.getLogger()

    override fun setup() {
        val useTray = ApplicationConfiguration.getInstance().useTray
        if (!DesktopEnvDetector.trayIconSupported() && useTray) {
            logger.warn("Application tray icon is not supported on this platform, deactivating.")
            ApplicationConfiguration.getInstance().useTray = false
        }
    }

    override fun initialize(owner: TrayHost): MVTray? = DefaultMainWindowSystemTrayController.initialize(owner)
}
