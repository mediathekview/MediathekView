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

import mediathek.i18n.UiMessages
import java.awt.Component
import javax.swing.DefaultListCellRenderer
import javax.swing.JComboBox
import javax.swing.JLabel
import javax.swing.JList
import javax.swing.JToolBar

class DownloadsDisplayFilterToolBar : JToolBar() {
    val displayCategoriesComboBox = JComboBox<String>()
    val viewComboBox = JComboBox<String>()

    init {
        isFloatable = true
        name = UiMessages.text("downloads.filter.toolbar")
        setDownloadsToolBarId("display-filter")

        configureFilterComboBox(displayCategoriesComboBox, mapOf(
            DisplayFilter.ALL to "downloads.filter.all",
            DisplayFilter.DOWNLOADS_ONLY to "downloads.filter.manual",
            DisplayFilter.ABOS_ONLY to "downloads.filter.subscriptions",
        ))
        configureFilterComboBox(viewComboBox, mapOf(
            ViewFilter.ALL to "downloads.filter.all",
            ViewFilter.NOT_STARTED to "downloads.filter.notStarted",
            ViewFilter.STARTED to "downloads.filter.started",
            ViewFilter.WAITING to "downloads.filter.waiting",
            ViewFilter.RUN_ONLY to "downloads.filter.running",
            ViewFilter.FINISHED_ONLY to "downloads.filter.finished",
        ))

        add(JLabel(UiMessages.text("downloads.filter.type")))
        add(displayCategoriesComboBox)
        addSeparator()
        add(JLabel(UiMessages.text("downloads.filter.status")))
        add(viewComboBox)
    }

    /** Translates displayed options while retaining the legacy values used by persisted filters. */
    private fun configureFilterComboBox(comboBox: JComboBox<String>, labels: Map<String, String>) {
        comboBox.putClientProperty("JComponent.roundRect", true)
        comboBox.renderer = object : DefaultListCellRenderer() {
            override fun getListCellRendererComponent(
                list: JList<*>, value: Any?, index: Int, isSelected: Boolean, cellHasFocus: Boolean,
            ): Component {
                super.getListCellRendererComponent(list, value, index, isSelected, cellHasFocus)
                text = labels[value]?.let(UiMessages::text) ?: value?.toString().orEmpty()
                return this
            }
        }
        val metrics = comboBox.getFontMetrics(comboBox.font)
        comboBox.prototypeDisplayValue = labels.maxBy { metrics.stringWidth(UiMessages.text(it.value)) }.key
        comboBox.maximumSize = comboBox.preferredSize
    }
}
