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
package ca.odell.glazedlists.impl.sort

import ca.odell.glazedlists.GlazedLists
import ca.odell.glazedlists.gui.TableFormat

/** Sorts table rows by the values extracted from a single column. */
internal open class TableColumnComparator<E : Any> : Comparator<E> {
    private val tableFormat: TableFormat<in E>
    private val column: Int
    private val comparator: Comparator<Any?>

    /** Sorts [column] with the natural-order comparator supplied by [GlazedLists]. */
    constructor(tableFormat: TableFormat<in E>, column: Int) :
            this(tableFormat, column, GlazedLists.comparableComparator<Comparable<Any?>>())

    /** Sorts [column] with the supplied column-value [comparator]. */
    @Suppress("UNCHECKED_CAST")
    constructor(tableFormat: TableFormat<in E>, column: Int, comparator: Comparator<*>) {
        this.tableFormat = tableFormat
        this.column = column
        this.comparator = comparator as Comparator<Any?>
    }

    override fun compare(alpha: E, beta: E): Int {
        val alphaField = tableFormat.getColumnValue(alpha, column)
        val betaField = tableFormat.getColumnValue(beta, column)
        try {
            return comparator.compare(alphaField, betaField)
        } catch (exception: ClassCastException) {
            val message = if (comparator === GlazedLists.comparableComparator<Comparable<Any?>>()) {
                "TableComparatorChooser can not sort objects \"$alphaField\", \"$betaField\" that do not implement Comparable."
            } else {
                "TableComparatorChooser can not sort objects \"$alphaField\", \"$betaField\" using the provided Comparator."
            }
            throw IllegalStateException(message, exception)
        }
    }

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (other == null || javaClass != other.javaClass) return false

        other as TableColumnComparator<*>
        return column == other.column &&
                comparator == other.comparator &&
                tableFormat == other.tableFormat
    }

    override fun hashCode(): Int {
        var result = tableFormat.hashCode()
        result = 29 * result + column
        result = 29 * result + comparator.hashCode()
        return result
    }
}
