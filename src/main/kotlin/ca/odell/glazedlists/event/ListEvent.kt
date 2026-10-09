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
package ca.odell.glazedlists.event

import ca.odell.glazedlists.EventList
import java.util.*

abstract class ListEvent<E> protected constructor(sourceList: EventList<E>) : EventObject(sourceList) {
    open var sourceList: EventList<E> = sourceList
        protected set

    abstract val index: Int

    abstract val blockStartIndex: Int

    abstract val blockEndIndex: Int

    abstract val type: Int

    abstract val oldValue: E

    abstract val newValue: E

    abstract val blocksRemaining: Int

    abstract val reorderMap: IntArray

    abstract val isReordering: Boolean

    abstract fun copy(): ListEvent<E>

    abstract fun reset()

    abstract fun next(): Boolean

    abstract fun hasNext(): Boolean

    abstract fun nextBlock(): Boolean

    abstract override fun toString(): String

    companion object {
        const val DELETE = 0
        const val UPDATE = 1
        const val INSERT = 2

        @JvmField
        val UNKNOWN_VALUE: Any = "UNKNOWN VALUE"

        @Suppress("UNCHECKED_CAST")
        fun <E> unknownValue(): E = UNKNOWN_VALUE as E
    }
}
