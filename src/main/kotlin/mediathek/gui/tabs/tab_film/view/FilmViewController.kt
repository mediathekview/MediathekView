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

package mediathek.gui.tabs.tab_film.view

import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.tabs.tab_film.PsetButtonsPanel
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import org.apache.commons.lang3.SystemUtils
import java.awt.event.KeyEvent
import java.util.function.IntConsumer
import javax.swing.JCheckBoxMenuItem
import javax.swing.JMenu
import javax.swing.JTabbedPane
import javax.swing.KeyStroke

class FilmViewController(private val host: Host) {
    interface Host {
        fun psetButtonsTab(): JTabbedPane
        fun psetButtonsPanel(): PsetButtonsPanel
        fun showButtonsMenuItem(): JCheckBoxMenuItem
        fun showDescriptionMenuItem(): JCheckBoxMenuItem
        fun actions(): FilmUiActions
        fun setDescriptionTabVisible(visible: Boolean)
    }

    fun installViewMenuEntry(menu: JMenu) {
        menu.add(host.showButtonsMenuItem(), 0)
    }

    fun makeButtonsTabVisible(visible: Boolean) {
        val panel = host.psetButtonsPanel()
        if (visible) {
            if (host.psetButtonsTab().indexOfComponent(panel) == -1) {
                host.psetButtonsTab().add(panel, 0)
                host.psetButtonsTab().setTitleAt(0, "Buttons")
            }
        } else {
            if (host.psetButtonsTab().indexOfComponent(panel) != -1) {
                host.psetButtonsTab().remove(panel)
            }
        }
    }

    fun installMenuEntries(menu: JMenu) {
        val actions = host.actions()
        menu.add(actions.playFilm)
        menu.add(actions.saveFilm)
        menu.add(actions.bookmarkAddFilm)
        menu.addSeparator()
        menu.add(actions.markFilmAsSeen)
        menu.add(actions.markFilmAsUnseen)
        menu.addSeparator()
        menu.add(actions.toggleBlacklist)
        menu.add(actions.editBlacklist)
        menu.addSeparator()
        menu.add(host.showDescriptionMenuItem())
    }

    fun setupPsetButtonsTab() {
        val initialVisibility = ApplicationConfiguration.getInstance().buttonsPanelVisible
        setupButtonsMenuItem(initialVisibility)

        val panel = host.psetButtonsPanel()
        panel.putClientProperty("JTabbedPane.tabClosable", true)
        panel.putClientProperty("JTabbedPane.tabCloseCallback", IntConsumer { host.showButtonsMenuItem().doClick() })
        panel.install(host.psetButtonsTab())

        makeButtonsTabVisible(initialVisibility)
    }

    private fun setupButtonsMenuItem(initialVisibility: Boolean) {
        if (!SystemUtils.IS_OS_MAC_OSX) {
            host.showButtonsMenuItem().accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_F11, 0)
        }
        host.showButtonsMenuItem().isSelected = initialVisibility
        host.showButtonsMenuItem().addActionListener {
            val visible = host.showButtonsMenuItem().isSelected
            makeButtonsTabVisible(visible)
            ApplicationConfiguration.getInstance().buttonsPanelVisible = visible
        }
    }

    fun setupShowFilmDescriptionMenuItem() {
        host.showDescriptionMenuItem().accelerator = KeyStroke.getKeyStroke(KeyEvent.VK_F10, 0)
        host.showDescriptionMenuItem().isSelected = ApplicationConfiguration.getInstance().filmDescriptionVisible
        host.showDescriptionMenuItem().addActionListener {
            val visible = host.showDescriptionMenuItem().isSelected
            host.setDescriptionTabVisible(visible)
            ApplicationConfiguration.getInstance().filmDescriptionVisible = visible
        }
    }
}
