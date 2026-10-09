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

import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JToolBar

class DownloadsDisplayFilterToolBar : JToolBar() {
    val displayCategoriesComboBox = JComboBox<String>()
    val viewComboBox = JComboBox<String>()

    init {
        isFloatable = true
        name = "Anzeige"
        setDownloadsToolBarId("display-filter")

        displayCategoriesComboBox.putClientProperty("JComponent.roundRect", true)
        displayCategoriesComboBox.prototypeDisplayValue = "nur Downloads"
        displayCategoriesComboBox.maximumSize = displayCategoriesComboBox.preferredSize

        viewComboBox.putClientProperty("JComponent.roundRect", true)
        viewComboBox.prototypeDisplayValue = "nur abgeschlossene"
        viewComboBox.maximumSize = viewComboBox.preferredSize

        add(JLabel("Typ:"))
        add(displayCategoriesComboBox)
        addSeparator()
        add(JLabel("Status:"))
        add(viewComboBox)
    }
}
