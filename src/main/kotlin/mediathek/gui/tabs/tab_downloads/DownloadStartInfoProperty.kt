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

import mediathek.controller.starter.DownloadServices
import mediathek.controller.starter.DownloadStartInfo
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent
import mediathek.tool.MessageBus.messageBus
import net.engio.mbassy.listener.Handler
import java.beans.PropertyChangeListener
import java.beans.PropertyChangeSupport

class DownloadStartInfoProperty(
    private val downloads: DownloadServices,
) {
    private val pcs = PropertyChangeSupport(this)

    var info: DownloadStartInfo = downloads.startInfo()
        set(value) {
            val oldValue = field
            field = value
            pcs.firePropertyChange("info", oldValue, field)
        }

    init {
        messageBus.subscribe(this)
    }

    @Suppress("UNUSED_PARAMETER")
    @Handler
    private fun handleLeftDisplayUpdate(event: UpdateStatusBarLeftDisplayEvent) {
        info = downloads.startInfo()
    }

    fun addStartInfoChangeListener(listener: PropertyChangeListener) {
        pcs.addPropertyChangeListener(listener)
    }
}
