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
import mediathek.i18n.UiMessages
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

    override fun getColumnName(column: Int): String = UiMessages.text(columnDescriptor(column).second)

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
            DatenAbo.ABO_DOWN_DATUM -> baseObject.downloadDate
            DatenAbo.ABO_PSET -> baseObject.psetName
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY -> baseObject.isDoNotStartAutomatically
            DatenAbo.ABO_FILM_COUNT -> filmCountProvider(baseObject)
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: $column")
        }

    companion object {
        /** Historical identifiers used by saved column layouts, independent of translated headings. */
        fun columnIdentifier(column: Int): String = columnDescriptor(column).first

        private fun columnDescriptor(column: Int): Pair<String, String> =
            columns[column] ?: throw IndexOutOfBoundsException("UNKNOWN COLUMN NAME: $column")

        private val columns = mapOf(
            DatenAbo.ABO_EINGESCHALTET to ("aktiv" to "abo.column.eingeschaltet"),
            DatenAbo.ABO_NAME to ("Name" to "abo.column.name"),
            DatenAbo.ABO_SENDER to ("Sender" to "abo.column.sender"),
            DatenAbo.ABO_THEMA to ("Thema" to "abo.column.thema"),
            DatenAbo.ABO_TITEL to ("Titel" to "abo.column.titel"),
            DatenAbo.ABO_THEMA_TITEL to ("Thema-Titel" to "abo.column.thema_titel"),
            DatenAbo.ABO_IRGENDWO to ("Irgendwo" to "abo.column.irgendwo"),
            DatenAbo.ABO_MINDESTDAUER to ("Dauer" to "abo.column.mindestdauer"),
            DatenAbo.ABO_MIN to ("min/max" to "abo.column.min"),
            DatenAbo.ABO_ZIELPFAD to ("Zielpfad" to "abo.column.zielpfad"),
            DatenAbo.ABO_DOWN_DATUM to ("letztes Abo" to "abo.column.down_datum"),
            DatenAbo.ABO_PSET to ("Programmset" to "abo.column.pset"),
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY to ("nicht automatisch starten" to "abo.column.do_not_start_automatically"),
            DatenAbo.ABO_FILM_COUNT to ("Filme" to "abo.column.film_count"),
        )
    }
}
