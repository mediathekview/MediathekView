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

import mediathek.tool.ApplicationConfiguration
import java.awt.event.ActionEvent
import java.util.function.IntConsumer
import javax.swing.AbstractAction
import javax.swing.JComponent
import javax.swing.JTabbedPane

abstract class ToggleOptionalTabAction(
    private val tabbedPane: JTabbedPane,
    private val tabComponent: JComponent,
    actionTitle: String,
    private val tabTitle: String,
    private val visibilityConfigKey: String,
    private val preferredInsertIndex: Int,
) : AbstractAction() {

    init {
        putValue(NAME, actionTitle)
        tabComponent.putClientProperty("JTabbedPane.tabClosable", true)
        tabComponent.putClientProperty("JTabbedPane.tabCloseCallback", IntConsumer { actionPerformed(null) })
    }

    private fun toggleTab() {
        val tabIndex = tabbedPane.indexOfComponent(tabComponent)
        val config = ApplicationConfiguration.getConfiguration()
        if (tabIndex == -1) {
            tabbedPane.insertTab(tabTitle, null, tabComponent, null, clampedInsertIndex())
            config.setProperty(visibilityConfigKey, true)
        } else {
            tabbedPane.remove(tabIndex)
            config.setProperty(visibilityConfigKey, false)
        }
    }

    private fun clampedInsertIndex(): Int = preferredInsertIndex.coerceIn(0, tabbedPane.tabCount)

    final override fun actionPerformed(e: ActionEvent?) {
        toggleTab()
    }
}
