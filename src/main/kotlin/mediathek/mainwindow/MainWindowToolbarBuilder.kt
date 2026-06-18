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
import mediathek.swing.IconOnlyButton
import javax.swing.Action
import javax.swing.JButton
import javax.swing.JToolBar

class MainWindowToolbarBuilder(
    private val toolBar: JToolBar,
    private val loadFilmListAction: Action,
    private val showFilmInformationAction: Action,
    private val toggleBlacklistAction: Action,
    private val editBlacklistAction: Action,
    private val manageAboAction: Action,
    private val settingsAction: Action,
    private val addDarkModeToggleButton: Runnable,
    private val configureToolBar: Runnable,
) {
    fun createCommonToolBar() {
        toolBar.add(IconOnlyButton(loadFilmListAction))
        toolBar.add(IconOnlyButton(showFilmInformationAction))
        createToggleBlacklistButton()
        toolBar.addSeparator()
        toolBar.add(IconOnlyButton(editBlacklistAction))
        toolBar.add(IconOnlyButton(manageAboAction))
        toolBar.addSeparator()
        toolBar.add(IconOnlyButton(settingsAction))
        addDarkModeToggleButton.run()

        configureToolBar.run()
    }

    private fun createToggleBlacklistButton() {
        val useIconWithText = ApplicationConfiguration.getInstance().toolbarBlacklistIconWithText
        if (useIconWithText) {
            toolBar.add(JButton(toggleBlacklistAction))
        } else {
            toolBar.add(IconOnlyButton(toggleBlacklistAction))
        }
    }
}
