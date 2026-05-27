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

package mediathek.gui.dialogEinstellungen.pset

import ca.odell.glazedlists.gui.AdvancedTableFormat
import mediathek.daten.DatenProg
import java.util.Comparator

class ProgramTableFormat : AdvancedTableFormat<DatenProg> {
    private val booleanComparator = Comparator<Boolean?> { first, second -> compareValues(first, second) }
    private val stringComparator = Comparator<String?> { first, second -> compareValues(first, second) }

    override fun getColumnCount(): Int = DatenProg.MAX_ELEM

    override fun getColumnName(column: Int): String = DatenProg.COLUMN_NAMES[column]

    override fun getColumnClass(column: Int): Class<*> =
        when (column) {
            DatenProg.PROGRAMM_RESTART,
            DatenProg.PROGRAMM_DOWNLOADMANAGER,
            -> Boolean::class.javaObjectType

            else -> Any::class.java
        }

    override fun getColumnComparator(column: Int): Comparator<*>? =
        when (column) {
            DatenProg.PROGRAMM_RESTART,
            DatenProg.PROGRAMM_DOWNLOADMANAGER,
            -> booleanComparator

            else -> stringComparator
        }

    override fun getColumnValue(
        baseObject: DatenProg,
        column: Int,
    ): Any? =
        when (column) {
            DatenProg.PROGRAMM_RESTART -> baseObject.isRestart
            DatenProg.PROGRAMM_DOWNLOADMANAGER -> baseObject.isDownloadManager
            else -> baseObject.arr[column]
        }
}
