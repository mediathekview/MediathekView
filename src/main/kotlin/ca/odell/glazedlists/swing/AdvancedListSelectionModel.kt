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
import ca.odell.glazedlists.matchers.Matcher
import javax.swing.ListSelectionModel

/** Defines selection operations beyond the standard Swing [ListSelectionModel]. */
interface AdvancedListSelectionModel<E> : ListSelectionModel {
    /** Selected values; mutations modify the source list. */
    val selected: EventList<E>

    /** Selected values; mutations modify selection state. */
    val togglingSelected: EventList<E>

    /** Deselected values; mutations modify the source list. */
    val deselected: EventList<E>

    /** Deselected values; mutations modify selection state. */
    val togglingDeselected: EventList<E>

    var enabled: Boolean

    fun addValidSelectionMatcher(validSelectionMatcher: Matcher<E>)

    fun removeValidSelectionMatcher(validSelectionMatcher: Matcher<E>)

    fun invertSelection()

    fun dispose()
}
