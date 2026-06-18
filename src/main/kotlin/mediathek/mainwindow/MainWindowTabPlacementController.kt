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
import mediathek.gui.messages.TabVisualSettingsChangedEvent
import mediathek.tool.MessageBus
import java.awt.BorderLayout
import java.awt.Container
import javax.swing.JTabbedPane
import javax.swing.JToolBar

class MainWindowTabPlacementController(
    private val enabled: Boolean,
) {
    fun resetTabPlacement() {
        if (!enabled) {
            return
        }

        MessageBus.messageBus.publishAsync(TabVisualSettingsChangedEvent())
    }

    fun configureTabPlacement(contentPane: Container, tabbedPane: JTabbedPane, commonToolBar: JToolBar) {
        if (!enabled) {
            return
        }

        if (ApplicationConfiguration.getInstance().tabPositionTop) {
            tabbedPane.tabPlacement = JTabbedPane.TOP
            contentPane.remove(commonToolBar)
            tabbedPane.putClientProperty(TRAILING_COMPONENT_KEY, commonToolBar)
        } else {
            tabbedPane.tabPlacement = JTabbedPane.LEFT
            tabbedPane.putClientProperty(TRAILING_COMPONENT_KEY, null)
            contentPane.add(commonToolBar, BorderLayout.PAGE_START)
        }
    }

    companion object {
        const val TRAILING_COMPONENT_KEY: String = "JTabbedPane.trailingComponent"
        const val TAB_ROTATION_KEY: String = "JTabbedPane.tabRotation"
    }
}
