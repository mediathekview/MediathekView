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

package mediathek.gui.abo

import mediathek.config.Konstanten
import mediathek.config.application.ApplicationConfiguration
import mediathek.gui.messages.FontSizeChangedEvent
import mediathek.tool.MessageBus
import net.engio.mbassy.listener.Handler
import javax.swing.JTable
import javax.swing.SwingUtilities

class AboTable : JTable() {
    private val applicationConfiguration = ApplicationConfiguration.getInstance()
    private var showSenderIcons = applicationConfiguration.aboTableShowSenderIcons
    private var useSmallSenderIcons = applicationConfiguration.aboTableUseSmallSenderIcons

    init {
        autoCreateRowSorter = false
        autoResizeMode = AUTO_RESIZE_OFF
        calculateRowHeight()
        MessageBus.messageBus.subscribe(this)
    }

    fun showSenderIcons(): Boolean = showSenderIcons

    fun setShowSenderIcons(showSenderIcons: Boolean) {
        this.showSenderIcons = showSenderIcons
        calculateRowHeight()
        repaint()
    }

    fun getUseSmallSenderIcons(): Boolean = useSmallSenderIcons

    fun setUseSmallSenderIcons(useSmallSenderIcons: Boolean) {
        this.useSmallSenderIcons = useSmallSenderIcons
        calculateRowHeight()
        repaint()
    }

    fun saveDisplaySettings() {
        applicationConfiguration.aboTableShowSenderIcons = showSenderIcons
        applicationConfiguration.aboTableUseSmallSenderIcons = useSmallSenderIcons
    }

    private fun calculateRowHeight() {
        rowHeight = when {
            !showSenderIcons -> Konstanten.TABLE_DEFAULT_ROW_HEIGHT
            useSmallSenderIcons -> maxOf(Konstanten.TABLE_DEFAULT_ROW_HEIGHT, JTable().rowHeight)
            else -> maxOf(Konstanten.TABLE_DEFAULT_LARGE_ICON_ROW_HEIGHT, JTable().rowHeight)
        }
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleFontSizeChanged(event: FontSizeChangedEvent) {
        SwingUtilities.invokeLater(::calculateRowHeight)
    }
}
