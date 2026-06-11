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

import ca.odell.glazedlists.gui.AdvancedTableFormat
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import mediathek.tool.GermanStringSorter
import java.time.LocalDate

class AboTableFormat(
    private val filmCountProvider: (DatenAbo) -> Int? = { 0 },
) : AdvancedTableFormat<DatenAbo> {
    private val booleanComparator = Comparator<Boolean?> { first, second -> compareValues(first, second) }
    private val intComparator = Comparator<Int?> { first, second -> compareValues(first, second) }
    private val dateComparator = Comparator<LocalDate?> { first, second -> compareValues(first, second) }
    private val stringComparator = Comparator<String?> { first, second ->
        when {
            first == null && second == null -> 0
            first == null -> -1
            second == null -> 1
            else -> GermanStringSorter.compare(first, second)
        }
    }

    override fun getColumnClass(column: Int): Class<*> =
        when (column) {
            DatenAbo.ABO_MINDESTDAUER,
            DatenAbo.ABO_FILM_COUNT,
            -> Int::class.javaObjectType

            DatenAbo.ABO_EINGESCHALTET,
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY,
            -> Boolean::class.javaObjectType

            DatenAbo.ABO_DOWN_DATUM -> LocalDate::class.java
            else -> String::class.java
        }

    override fun getColumnComparator(column: Int): Comparator<*>? =
        when (column) {
            DatenAbo.ABO_MINDESTDAUER,
            DatenAbo.ABO_FILM_COUNT,
            -> intComparator

            DatenAbo.ABO_EINGESCHALTET,
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY,
            -> booleanComparator

            DatenAbo.ABO_DOWN_DATUM -> dateComparator
            else -> stringComparator
        }

    override fun getColumnCount(): Int = DatenAbo.MAX_ELEM

    override fun getColumnName(column: Int): String =
        when (column) {
            DatenAbo.ABO_EINGESCHALTET -> "aktiv"
            DatenAbo.ABO_NAME -> "Name"
            DatenAbo.ABO_SENDER -> "Sender"
            DatenAbo.ABO_THEMA -> "Thema"
            DatenAbo.ABO_TITEL -> "Titel"
            DatenAbo.ABO_THEMA_TITEL -> "Thema-Titel"
            DatenAbo.ABO_IRGENDWO -> "Irgendwo"
            DatenAbo.ABO_MINDESTDAUER -> "Dauer"
            DatenAbo.ABO_MIN -> "min/max"
            DatenAbo.ABO_ZIELPFAD -> "Zielpfad"
            DatenAbo.ABO_DOWN_DATUM -> "letztes Abo"
            DatenAbo.ABO_PSET -> "Programmset"
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY -> "nicht automatisch starten"
            DatenAbo.ABO_FILM_COUNT -> "Filme"
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN NAME: $column")
        }

    override fun getColumnValue(baseObject: DatenAbo, column: Int): Any? =
        when (column) {
            DatenAbo.ABO_EINGESCHALTET -> baseObject.isActive
            DatenAbo.ABO_NAME -> baseObject.name
            DatenAbo.ABO_SENDER -> baseObject.sender
            DatenAbo.ABO_THEMA -> baseObject.thema
            DatenAbo.ABO_TITEL -> baseObject.title
            DatenAbo.ABO_THEMA_TITEL -> baseObject.themaTitel
            DatenAbo.ABO_IRGENDWO -> baseObject.irgendwo
            DatenAbo.ABO_MINDESTDAUER -> baseObject.mindestDauerMinuten
            DatenAbo.ABO_MIN -> if (baseObject.filmLengthState == FilmLengthState.MINIMUM) "min" else "max"
            DatenAbo.ABO_ZIELPFAD -> baseObject.zielpfad
            DatenAbo.ABO_DOWN_DATUM -> baseObject.downDatum
            DatenAbo.ABO_PSET -> baseObject.psetName
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY -> baseObject.isDoNotStartAutomatically
            DatenAbo.ABO_FILM_COUNT -> filmCountProvider(baseObject)
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: $column")
        }
}
