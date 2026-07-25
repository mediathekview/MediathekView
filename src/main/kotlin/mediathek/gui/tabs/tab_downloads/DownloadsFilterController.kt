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

package mediathek.gui.tabs.tab_downloads

import mediathek.config.application.ApplicationConfiguration
import javax.swing.DefaultComboBoxModel

class DownloadsFilterController(
    private val toolBar: DownloadsDisplayFilterToolBar,
    private val onFilterChanged: Runnable
) {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    var displayFilter: DisplayFilter = DisplayFilter.all()
        private set
    var viewFilter: ViewFilter = ViewFilter.all()
        private set

    fun install() {
        setupDisplayCategories()
        setupViewCategories()
    }

    private fun setupDisplayCategories() {
        val comboBox = toolBar.displayCategoriesComboBox
        comboBox.model = DefaultComboBoxModel(
            arrayOf(
                DisplayFilter.ALL,
                DisplayFilter.DOWNLOADS_ONLY,
                DisplayFilter.ABOS_ONLY,
            ),
        )
        displayFilter = DisplayFilter.from(applicationConfiguration.getDownloadDisplayFilter(DisplayFilter.ALL))
        comboBox.model.selectedItem = displayFilter.selectedItem()
        comboBox.addActionListener {
            displayFilter = DisplayFilter.from(comboBox.model.selectedItem)
            applicationConfiguration.setDownloadDisplayFilter(displayFilter.selectedItem())
            onFilterChanged.run()
        }
    }

    private fun setupViewCategories() {
        val comboBox = toolBar.viewComboBox
        comboBox.model = DefaultComboBoxModel(
            arrayOf(
                ViewFilter.ALL,
                ViewFilter.NOT_STARTED,
                ViewFilter.STARTED,
                ViewFilter.WAITING,
                ViewFilter.RUN_ONLY,
                ViewFilter.FINISHED_ONLY,
            ),
        )
        viewFilter = ViewFilter.from(applicationConfiguration.getDownloadViewFilter(ViewFilter.ALL))
        comboBox.model.selectedItem = viewFilter.selectedItem()
        comboBox.addActionListener {
            viewFilter = ViewFilter.from(comboBox.model.selectedItem)
            applicationConfiguration.setDownloadViewFilter(viewFilter.selectedItem())
            onFilterChanged.run()
        }
    }
}
