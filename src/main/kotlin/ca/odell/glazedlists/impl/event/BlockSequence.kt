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
package ca.odell.glazedlists.impl.event

import ca.odell.glazedlists.event.ListEvent

/**
 * Manages a simple sequence of list-event blocks that occur in increasing order.
 *
 * @author <a href="mailto:jesse@swank.ca">Jesse Wilson</a>
 */
open class BlockSequence<E> {
    private var starts = IntArray(INITIAL_CAPACITY)
    private var ends = IntArray(INITIAL_CAPACITY)
    private var types = IntArray(INITIAL_CAPACITY)
    private var blockCount = 0
    private val oldValues = ArrayList<E>()
    private val newValues = ArrayList<E>()

    /**
     * @param startIndex the first updated element, inclusive
     * @param endIndex the last index, exclusive
     */
    open fun update(startIndex: Int, endIndex: Int): Boolean =
        addChange(ListEvent.UPDATE, startIndex, endIndex, ListEvent.unknownValue(), ListEvent.unknownValue())

    /**
     * @param startIndex the first inserted element, inclusive
     * @param endIndex the last index, exclusive
     */
    open fun insert(startIndex: Int, endIndex: Int): Boolean =
        addChange(ListEvent.INSERT, startIndex, endIndex, ListEvent.unknownValue(), ListEvent.unknownValue())

    /**
     * @param startIndex the index of the first element to remove
     * @param endIndex the last index, exclusive
     */
    open fun delete(startIndex: Int, endIndex: Int): Boolean =
        addChange(ListEvent.DELETE, startIndex, endIndex, ListEvent.unknownValue(), ListEvent.unknownValue())

    /**
     * Adds this change to the sequence, or returns `false` without making a change
     * when it violates the increasing-order requirement.
     */
    open fun addChange(type: Int, startIndex: Int, endIndex: Int, oldValue: E, newValue: E): Boolean {
        val lastType: Int
        val lastStartIndex: Int
        val lastEndIndex: Int
        val lastChangedIndex: Int
        val lastOldValue: E
        val lastNewValue: E
        if (blockCount == 0) {
            lastType = -1
            lastStartIndex = -1
            lastEndIndex = 0
            lastChangedIndex = 0
            lastOldValue = ListEvent.unknownValue()
            lastNewValue = ListEvent.unknownValue()
        } else {
            val lastBlock = blockCount - 1
            lastType = types[lastBlock]
            lastStartIndex = starts[lastBlock]
            lastEndIndex = ends[lastBlock]
            lastChangedIndex = if (lastType == ListEvent.DELETE) lastStartIndex else lastEndIndex
            lastOldValue = if (lastType == ListEvent.DELETE) oldValues[lastBlock] else ListEvent.unknownValue()
            lastNewValue = newValues[lastBlock]
        }

        if (startIndex < lastChangedIndex) {
            return false
        }

        if (
            lastChangedIndex == startIndex &&
            lastType == type &&
            oldValue === lastOldValue &&
            newValue === lastNewValue
        ) {
            val newLength = (lastEndIndex - lastStartIndex) + (endIndex - startIndex)
            ends[blockCount - 1] = lastStartIndex + newLength
            return true
        }

        ensureCapacity(blockCount + 1)
        starts[blockCount] = startIndex
        ends[blockCount] = endIndex
        types[blockCount] = type
        oldValues.add(oldValue)
        newValues.add(newValue)
        blockCount++
        return true
    }

    open val isEmpty: Boolean
        get() = blockCount == 0

    open fun reset() {
        blockCount = 0
        oldValues.clear()
        newValues.clear()
    }

    open fun iterator(): Iterator = Iterator()

    override fun toString(): String =
        buildString {
            repeat(blockCount) { block ->
                if (block != 0) append(", ")

                when (types[block]) {
                    ListEvent.INSERT -> append('+')
                    ListEvent.UPDATE -> append('U')
                    ListEvent.DELETE -> append('X')
                }

                val start = starts[block]
                val end = ends[block]
                append(start)
                if (end != start) {
                    append('-')
                    append(end)
                }
            }
        }

    private fun ensureCapacity(requiredCapacity: Int) {
        if (requiredCapacity <= starts.size) return

        val newCapacity = maxOf(requiredCapacity, starts.size * 2)
        starts = starts.copyOf(newCapacity)
        ends = ends.copyOf(newCapacity)
        types = types.copyOf(newCapacity)
    }

    private fun blockCount(): Int = blockCount

    private fun startAt(block: Int): Int = starts[block]

    private fun endAt(block: Int): Int = ends[block]

    private fun typeAt(block: Int): Int = types[block]

    private fun oldValueAt(block: Int): E = oldValues[block]

    private fun newValueAt(block: Int): E = newValues[block]

    /** Iterates through the changes in this sequence. */
    open inner class Iterator {
        private var blockIndex = -1
        private var offset = 0
        private var startIndex = -1
        private var endIndex = -1
        private var currentType = -1

        open fun copy(): Iterator =
            Iterator().also { result ->
                result.blockIndex = blockIndex
                result.offset = offset
                result.startIndex = startIndex
                result.endIndex = endIndex
                result.currentType = currentType
            }

        open val index: Int
            get() =
                when (currentType) {
                    ListEvent.INSERT, ListEvent.UPDATE -> startIndex + offset
                    ListEvent.DELETE -> startIndex
                    else -> throw IllegalStateException()
                }

        open val blockStart: Int
            get() {
                check(startIndex != -1) {
                    "The ListEvent is not currently in a state to return a block start index"
                }
                return startIndex
            }

        open val blockEnd: Int
            get() {
                check(endIndex != -1) {
                    "The ListEvent is not currently in a state to return a block end index"
                }
                return endIndex
            }

        open val type: Int
            get() {
                check(currentType != -1) { "The ListEvent is not currently in a state to return a type" }
                return currentType
            }

        open val oldValue: E
            get() = oldValueAt(blockIndex)

        open val newValue: E
            get() = newValueAt(blockIndex)

        /** Moves to the next changed index, possibly within the same block. */
        open fun next(): Boolean {
            if (offset + 1 < endIndex - startIndex) {
                offset++
                return true
            }

            return moveToNextBlock()
        }

        /** Moves to the next changed block. */
        open fun nextBlock(): Boolean = moveToNextBlock()

        /** @return whether another changed index remains */
        open fun hasNext(): Boolean =
            offset + 1 < endIndex - startIndex || blockIndex + 1 < blockCount()

        /** @return whether another changed block remains */
        open fun hasNextBlock(): Boolean = blockIndex + 1 < blockCount()

        private fun moveToNextBlock(): Boolean {
            if (blockIndex + 1 >= blockCount()) return false

            blockIndex++
            offset = 0
            startIndex = startAt(blockIndex)
            endIndex = endAt(blockIndex)
            currentType = typeAt(blockIndex)
            return true
        }
    }

    private companion object {
        private const val INITIAL_CAPACITY = 10
    }
}
