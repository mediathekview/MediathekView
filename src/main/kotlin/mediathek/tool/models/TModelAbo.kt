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

package mediathek.tool.models

import mediathek.daten.ListeAbo
import mediathek.daten.abo.DatenAbo
import mediathek.daten.abo.FilmLengthState
import java.time.LocalDate
import javax.swing.table.AbstractTableModel

class TModelAbo(private val abos: ListeAbo) : AbstractTableModel() {
    private var visibleAbos: List<DatenAbo> = abos.toList()

    fun setSenderFilter(sender: String?) {
        visibleAbos = if (sender.isNullOrEmpty()) {
            abos.toList()
        } else {
            abos.filter { abo -> sender == abo.sender }
        }
        fireTableDataChanged()
    }

    override fun getColumnClass(columnIndex: Int): Class<*> =
        when (columnIndex) {
            DatenAbo.ABO_NR, DatenAbo.ABO_MINDESTDAUER -> Int::class.javaObjectType
            DatenAbo.ABO_EINGESCHALTET, DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY -> Boolean::class.javaObjectType
            DatenAbo.ABO_DOWN_DATUM -> LocalDate::class.java
            DatenAbo.ABO_REF -> DatenAbo::class.java
            else -> String::class.java
        }

    override fun getRowCount(): Int = visibleAbos.size

    override fun getColumnCount(): Int = DatenAbo.MAX_ELEM

    override fun getColumnName(column: Int): String =
        when (column) {
            DatenAbo.ABO_NR -> "Nr"
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
            DatenAbo.ABO_REF -> ""
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN NAME: $column")
        }

    override fun getValueAt(rowIndex: Int, columnIndex: Int): Any? {
        val abo = visibleAbos[rowIndex]

        return when (columnIndex) {
            DatenAbo.ABO_NR -> abo.nr
            DatenAbo.ABO_EINGESCHALTET -> abo.isActive
            DatenAbo.ABO_NAME -> abo.name
            DatenAbo.ABO_SENDER -> abo.sender
            DatenAbo.ABO_THEMA -> abo.thema
            DatenAbo.ABO_TITEL -> abo.title
            DatenAbo.ABO_THEMA_TITEL -> abo.themaTitel
            DatenAbo.ABO_IRGENDWO -> abo.irgendwo
            DatenAbo.ABO_MINDESTDAUER -> abo.mindestDauerMinuten
            DatenAbo.ABO_MIN -> if (abo.filmLengthState == FilmLengthState.MINIMUM) "min" else "max"
            DatenAbo.ABO_ZIELPFAD -> abo.zielpfad
            DatenAbo.ABO_DOWN_DATUM -> abo.downDatum
            DatenAbo.ABO_PSET -> abo.psetName
            DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY -> abo.isDoNotStartAutomatically
            DatenAbo.ABO_REF -> abo
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: $columnIndex")
        }
    }
}
