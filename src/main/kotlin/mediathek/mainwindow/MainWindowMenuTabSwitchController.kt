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
import mediathek.gui.messages.InstallTabSwitchListenerEvent
import java.awt.Component
import java.util.function.BooleanSupplier
import java.util.function.Supplier
import javax.swing.JMenu
import javax.swing.JTabbedPane
import javax.swing.SwingUtilities

class MainWindowMenuTabSwitchController(
    private val tabbedPane: JTabbedPane,
    private val filmMenu: JMenu,
    private val downloadMenu: JMenu,
    private val filmTab: Supplier<out Component?>,
    private val downloadTab: Supplier<out Component?>,
    private val automaticSwitchingSupported: BooleanSupplier,
) {
    private var filmMenuListener: MenuTabSwitchListener? = null
    private var downloadMenuListener: MenuTabSwitchListener? = null
    private var listenersInstalled = false

    fun initialize() {
        if (!automaticSwitchingSupported.asBoolean) {
            return
        }

        if (filmMenuListener == null) {
            filmMenuListener = MenuTabSwitchListener(tabbedPane, requireNotNull(filmTab.get()))
        }
        if (downloadMenuListener == null) {
            downloadMenuListener = MenuTabSwitchListener(tabbedPane, requireNotNull(downloadTab.get()))
        }

        if (ApplicationConfiguration.getInstance().installTabSwitchListener) {
            installConfiguredMenuTabSwitchListeners()
        }
    }

    fun handleInstallTabSwitchListenerEvent(event: InstallTabSwitchListenerEvent) {
        if (!automaticSwitchingSupported.asBoolean) {
            return
        }

        when (event.event) {
            InstallTabSwitchListenerEvent.INSTALL_TYPE.INSTALL ->
                SwingUtilities.invokeLater(::installConfiguredMenuTabSwitchListeners)

            InstallTabSwitchListenerEvent.INSTALL_TYPE.REMOVE ->
                SwingUtilities.invokeLater(::removeConfiguredMenuTabSwitchListeners)
        }
    }

    private fun installConfiguredMenuTabSwitchListeners() {
        if (listenersInstalled) {
            return
        }

        filmMenu.addMenuListener(requireNotNull(filmMenuListener))
        downloadMenu.addMenuListener(requireNotNull(downloadMenuListener))
        listenersInstalled = true
    }

    private fun removeConfiguredMenuTabSwitchListeners() {
        if (!listenersInstalled) {
            return
        }

        filmMenu.removeMenuListener(requireNotNull(filmMenuListener))
        downloadMenu.removeMenuListener(requireNotNull(downloadMenuListener))
        listenersInstalled = false
    }
}
