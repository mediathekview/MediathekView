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

package mediathek.mainwindow

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.MVTray
import raven.toast.Notifications
import java.awt.PopupMenu
import java.awt.Taskbar
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.Action
import javax.swing.SwingUtilities

class MainWindowPlatformIntegration(
    private val owner: MediathekGui,
    private val loadFilmListAction: Action,
    private val setupSystemTray: Runnable,
    private val systemTrayController: MainWindowSystemTrayController,
) {
    private var tray: MVTray? = null

    fun setupTaskbarMenuLater() {
        SwingUtilities.invokeLater {
            if (Taskbar.isTaskbarSupported()) {
                setupTaskbarMenu()
            }
        }
    }

    fun setupSystemTray() {
        systemTrayController.setup()
        setupSystemTray.run()
    }

    fun setupSystemTrayLater() {
        SwingUtilities.invokeLater {
            initializeSystemTray()

            owner.addWindowListener(object : WindowAdapter() {
                override fun windowClosing(evt: WindowEvent) {
                    if (tray != null && ApplicationConfiguration.getInstance().useTray) {
                        owner.isVisible = false
                    } else {
                        owner.quitApplication()
                    }
                }
            })
        }
    }

    fun initializeSystemTray() {
        val useTray = ApplicationConfiguration.getInstance().useTray
        if (tray == null && useTray) {
            tray = systemTrayController.initialize(owner)
        } else if (tray != null && !useTray) {
            closeSystemTray()
        }
    }

    fun closeSystemTray() {
        tray?.beenden()
        tray = null
    }

    fun setupRavenNotifications() {
        Notifications.getInstance().setJFrame(owner)
    }

    private fun setupTaskbarMenu() {
        val taskbar = Taskbar.getTaskbar()
        if (taskbar.isSupported(Taskbar.Feature.MENU)) {
            val popupMenu = taskbar.menu ?: PopupMenu()

            popupMenu.addSeparator()
            popupMenu.add(NoIconAwtMenuItem(loadFilmListAction))

            taskbar.menu = popupMenu
        }
    }
}
