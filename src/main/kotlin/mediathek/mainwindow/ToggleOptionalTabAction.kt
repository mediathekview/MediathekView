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

import java.awt.event.ActionEvent
import javax.swing.AbstractAction
import javax.swing.JTabbedPane

abstract class ToggleOptionalTabAction(
    private val tabbedPane: JTabbedPane,
    private val tab: MainWindowTab,
    actionTitle: String,
    private val visibilityWriter: (Boolean) -> Unit,
    private val preferredInsertIndex: Int,
) : AbstractAction() {

    init {
        putValue(NAME, actionTitle)
    }

    private fun toggleTab() {
        val tabComponent = tab.installedComponent()
        val tabIndex = tabComponent?.let(tabbedPane::indexOfComponent) ?: -1
        if (tabIndex == -1) {
            tabbedPane.insertTab(tab.title, null, tab.initialComponent(), null, clampedInsertIndex())
            visibilityWriter(true)
        } else {
            tabbedPane.remove(tabIndex)
            visibilityWriter(false)
        }
    }

    private fun clampedInsertIndex(): Int = preferredInsertIndex.coerceIn(0, tabbedPane.tabCount)

    final override fun actionPerformed(e: ActionEvent?) {
        toggleTab()
    }
}
