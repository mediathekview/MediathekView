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

import mediathek.daten.DatenFilm
import mediathek.tool.datum.DatumFilm
import javax.swing.table.AbstractTableModel

class TModelFilm(capacity: Int = 0) : AbstractTableModel() {
    private val dataList = ArrayList<DatenFilm>(capacity)

    override fun getRowCount(): Int = dataList.size

    override fun getColumnCount(): Int = COLUMN_COUNT

    override fun getColumnClass(columnIndex: Int): Class<*> =
        when (columnIndex) {
            DatenFilm.FILM_NR,
            DatenFilm.FILM_DAUER,
            DatenFilm.FILM_GROESSE,
            -> Int::class.javaObjectType

            DatenFilm.FILM_DATUM -> DatumFilm::class.java

            DatenFilm.FILM_HD,
            DatenFilm.FILM_UT,
            -> Boolean::class.javaObjectType

            DatenFilm.FILM_DATUM_LONG -> Long::class.javaObjectType
            else -> String::class.java
        }

    override fun getColumnName(column: Int): String =
        when (column) {
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN,
            -> ""

            DatenFilm.FILM_NR -> "Nr"
            DatenFilm.FILM_SENDER -> "Sender"
            DatenFilm.FILM_THEMA -> "Thema"
            DatenFilm.FILM_TITEL -> "Titel"
            DatenFilm.FILM_DATUM -> "Datum"
            DatenFilm.FILM_ZEIT -> "Zeit"
            DatenFilm.FILM_DAUER -> "Dauer"
            DatenFilm.FILM_GROESSE -> "Größe [MB]"
            DatenFilm.FILM_HD -> "HQ"
            DatenFilm.FILM_UT -> "UT"
            DatenFilm.FILM_GEO -> "Geo"
            DatenFilm.FILM_URL -> "URL"
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN NAME: $column")
        }

    override fun getValueAt(row: Int, column: Int): Any {
        val film = dataList[row]

        return when (column) {
            DatenFilm.FILM_NR -> film.filmNr
            DatenFilm.FILM_SENDER -> film.sender
            DatenFilm.FILM_THEMA -> film.thema
            DatenFilm.FILM_TITEL -> film.title
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN,
            -> ""

            DatenFilm.FILM_DATUM -> film.datumFilm
            DatenFilm.FILM_ZEIT -> film.sendeZeit
            DatenFilm.FILM_DAUER -> film.filmLength
            DatenFilm.FILM_GROESSE -> film.fileSizeInMegabytes
            DatenFilm.FILM_HD -> film.isHighQuality
            DatenFilm.FILM_UT -> film.hasSubtitle()
            DatenFilm.FILM_GEO -> film.countriesAsString
            DatenFilm.FILM_URL -> film.urlNormalQuality
            DatenFilm.FILM_REF -> film
            else -> throw IndexOutOfBoundsException("UNKNOWN COLUMN VALUE: $column")
        }
    }

    fun addAll(listeFilme: List<DatenFilm>) {
        if (listeFilme.isEmpty()) {
            return
        }
        val oldRowCount = dataList.size
        dataList.addAll(listeFilme)
        fireTableRowsInserted(oldRowCount, dataList.lastIndex)
    }

    private companion object {
        private const val COLUMN_COUNT = 15
    }
}
