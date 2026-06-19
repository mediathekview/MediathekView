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

import mediathek.gui.actions.QuitAction
import mediathek.gui.actions.ShowAboutAction
import javax.swing.Action
import javax.swing.JFrame
import javax.swing.JMenu

interface MainWindowMenuPolicy {
    val supportsFontMenu: Boolean

    fun addSettingsItem(fileMenu: JMenu, settingsAction: Action)

    fun addQuitItem(fileMenu: JMenu, quitApplication: () -> Boolean)

    fun addHelpTail(helpMenu: JMenu, parent: JFrame)
}

object DefaultMainWindowMenuPolicy : MainWindowMenuPolicy {
    override val supportsFontMenu: Boolean = true

    override fun addSettingsItem(fileMenu: JMenu, settingsAction: Action) {
        fileMenu.addSeparator()
        fileMenu.add(settingsAction)
    }

    override fun addQuitItem(fileMenu: JMenu, quitApplication: () -> Boolean) {
        fileMenu.addSeparator()
        fileMenu.add(QuitAction(quitApplication))
    }

    override fun addHelpTail(helpMenu: JMenu, parent: JFrame) {
        helpMenu.addSeparator()
        helpMenu.add(ShowAboutAction(parent))
    }
}

object MacMainWindowMenuPolicy : MainWindowMenuPolicy {
    override val supportsFontMenu: Boolean = false

    override fun addSettingsItem(fileMenu: JMenu, settingsAction: Action) = Unit

    override fun addQuitItem(fileMenu: JMenu, quitApplication: () -> Boolean) = Unit

    override fun addHelpTail(helpMenu: JMenu, parent: JFrame) = Unit
}
