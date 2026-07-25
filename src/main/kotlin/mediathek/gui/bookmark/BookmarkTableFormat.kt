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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.bookmark

import ca.odell.glazedlists.gui.TableFormat

internal object BookmarkTableFormat : TableFormat<BookmarkData> {
    private val columnNames =
        listOf(
            "Gesehen",
            "Sender",
            "Thema",
            "Titel",
            "Dauer",
            "Sendedatum",
            "Verfügbar bis",
            "URL",
            "Notiz",
            "Hash Code",
            "hinzugefügt am",
        )

    override fun getColumnCount(): Int = columnNames.size

    override fun getColumnName(column: Int): String = columnNames[column]

    override fun getColumnValue(baseObject: BookmarkData, column: Int): Any? =
        when (column) {
            0 -> baseObject.seen
            1 -> baseObject.sender
            2 -> baseObject.thema
            3 -> baseObject.title
            4 -> baseObject.dauer
            5 -> baseObject.sendedatum
            6 -> baseObject.availableUntil
            7 -> baseObject.normalQualityUrl
            8 -> baseObject.note
            9 -> baseObject.filmHashCode
            10 -> baseObject.bookmarkAdded
            else -> throw IndexOutOfBoundsException("Column index: $column")
        }
}
