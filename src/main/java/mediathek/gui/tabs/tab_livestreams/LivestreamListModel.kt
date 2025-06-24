/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.tabs.tab_livestreams

import javax.swing.AbstractListModel

class LivestreamListModel : AbstractListModel<LivestreamEntry>() {

    private val entries = mutableListOf<LivestreamEntry>()

    override fun getSize(): Int = entries.size

    override fun getElementAt(index: Int): LivestreamEntry = entries[index]

    fun setData(newData: List<LivestreamEntry>) {
        entries.clear()
        entries.addAll(newData)
        fireContentsChanged(this, 0, entries.size - 1)
    }

    fun updateEntry(index: Int, updated: LivestreamEntry) {
        entries[index] = updated
        fireContentsChanged(this, index, index)
    }
}
