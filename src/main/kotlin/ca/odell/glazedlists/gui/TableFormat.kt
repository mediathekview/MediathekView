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
package ca.odell.glazedlists.gui

import org.jspecify.annotations.Nullable

/** Defines how values are presented as table columns. */
interface TableFormat<E> {
    /** The number of columns to display. */
    fun getColumnCount(): Int

    /** Gets the title of the specified column. */
    fun getColumnName(column: Int): String

    /** Gets the possibly-null cell value for [baseObject] in [column]. */
    fun getColumnValue(baseObject: E, column: Int): @Nullable Any?
}
