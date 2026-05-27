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

package mediathek.gui.tabs

import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.FilmDescriptionPanel
import mediathek.tool.ApplicationConfiguration
import java.util.*
import java.util.function.IntConsumer
import java.util.function.Supplier
import javax.swing.JCheckBoxMenuItem
import javax.swing.JTabbedPane
import javax.swing.JTable

class DescriptionTabController {
    val tabbedPane: JTabbedPane = JTabbedPane()
    private val descriptionPanel = FilmDescriptionPanel()

    fun install(
        table: JTable,
        closeMenuItem: JCheckBoxMenuItem,
        configKey: String,
        filmSupplier: Supplier<Optional<DatenFilm>>,
    ) {
        descriptionPanel.install(tabbedPane, table, filmSupplier)
        tabbedPane.putClientProperty("JTabbedPane.tabClosable", true)
        tabbedPane.putClientProperty("JTabbedPane.tabCloseCallback", IntConsumer { closeMenuItem.doClick() })
        setVisible(ApplicationConfiguration.getConfiguration().getBoolean(configKey, true))
    }

    fun setVisible(visible: Boolean) {
        if (visible) {
            if (tabbedPane.indexOfComponent(descriptionPanel) == -1) {
                tabbedPane.add(descriptionPanel, 0)
                tabbedPane.setTitleAt(0, "Beschreibung")
            }
        } else if (tabbedPane.indexOfComponent(descriptionPanel) != -1) {
            tabbedPane.remove(descriptionPanel)
        }
    }
}
