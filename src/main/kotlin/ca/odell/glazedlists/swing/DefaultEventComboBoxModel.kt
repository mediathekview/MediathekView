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
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.EventList
import javax.swing.ComboBoxModel
import javax.swing.event.ListDataEvent

/** A Swing combo-box model backed by an [EventList]. */
open class DefaultEventComboBoxModel<E>(
    source: EventList<E>,
    disposeSource: Boolean,
) : DefaultEventListModel<E>(source, disposeSource), ComboBoxModel<E> {
    private var selected: Any? = null

    constructor(source: EventList<E>) : this(source, false)

    override fun getSelectedItem(): Any? = selected

    override fun setSelectedItem(selected: Any?) {
        if (this.selected === selected) return

        this.selected = selected
        listDataEvent.setRange(-1, -1)
        listDataEvent.setType(ListDataEvent.CONTENTS_CHANGED)
        fireListDataEvent(listDataEvent)
    }
}
