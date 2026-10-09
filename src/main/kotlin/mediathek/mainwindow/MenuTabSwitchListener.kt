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

import java.awt.Component
import javax.swing.JTabbedPane
import javax.swing.event.MenuEvent
import javax.swing.event.MenuListener

internal class MenuTabSwitchListener(
    private val tabbedPane: JTabbedPane,
    private val targetTab: Component,
) : MenuListener {
    override fun menuSelected(e: MenuEvent) {
        setTabIfContain(targetTab)
    }

    override fun menuDeselected(e: MenuEvent) = Unit

    override fun menuCanceled(e: MenuEvent) = Unit

    private fun setTabIfContain(check: Component) {
        for (index in 0 until tabbedPane.tabCount) {
            if (tabbedPane.getComponentAt(index) == check) {
                tabbedPane.selectedIndex = index
                return
            }
        }
    }
}
