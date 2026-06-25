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

import org.apache.logging.log4j.LogManager
import javax.swing.JMenu
import javax.swing.JTabbedPane

class MainWindowTabRegistry(
    private val tabbedPane: JTabbedPane,
) {
    private val tabs = mutableListOf<MainWindowTab>()

    fun register(tab: MainWindowTab) {
        tabs += tab
    }

    fun installVisibleTabs() {
        tabs.filter { it.visible.asBoolean }
            .forEach { tabbedPane.addTab(it.title, it.initialComponent()) }
        materializeSelectedTab()
    }

    fun installSelectedTabMaterializer() {
        tabbedPane.addChangeListener { materializeSelectedTab() }
    }

    fun materializeSelectedTab() {
        val selectedComponent = tabbedPane.selectedComponent ?: return
        val selectedTab = tabs.firstOrNull { it.installedComponent() === selectedComponent } ?: return
        val realComponent = selectedTab.materializeComponent()
        if (realComponent !== selectedComponent) {
            val selectedIndex = tabbedPane.indexOfComponent(selectedComponent)
            if (selectedIndex >= 0) {
                tabbedPane.setComponentAt(selectedIndex, realComponent)
            }
        }
        selectedTab.notifyComponentSelected()
    }

    fun configureIcons(showIcons: Boolean) {
        tabs.forEach { tab ->
            val component = tab.installedComponent() ?: return@forEach
            val index = tabbedPane.indexOfComponent(component)
            if (index >= 0) {
                tabbedPane.setIconAt(index, if (showIcons) tab.icon?.get() else null)
            }
        }
    }

    fun installViewMenuEntries(menu: JMenu) {
        tabs.mapNotNull(MainWindowTab::toggleAction)
            .forEach(menu::add)
    }

    fun disposeTabs() {
        tabs.forEach { tab ->
            try {
                tab.dispose()
            } catch (ex: RuntimeException) {
                logger.error("Could not dispose main window tab: {}", tab.title, ex)
            }
        }
    }

    companion object {
        private val logger = LogManager.getLogger(MainWindowTabRegistry::class.java)
    }
}
