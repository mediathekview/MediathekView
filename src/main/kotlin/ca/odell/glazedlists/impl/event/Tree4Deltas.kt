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
import ca.odell.glazedlists.impl.adt.barcode2.FourColorTree
import ca.odell.glazedlists.impl.adt.barcode2.FourColorTreeIterator
import ca.odell.glazedlists.impl.adt.barcode2.ListToByteCoder
import java.util.*

/**
 * Manages and describes the differences between two revisions of the same list.
 *
 * The target may accumulate overlapping or out-of-order changes. Source and target
 * indices are tracked independently so the final event can be emitted in target order.
 *
 * @author [Jesse Wilson](mailto:jesse@swank.ca)
 */
class Tree4Deltas<E> {
    private val tree = FourColorTree<DeltaValues<E>>(BYTE_CODER)
    private val deltaValuesByOldValue = IdentityHashMap<E, IdentityHashMap<E, DeltaValues<E>>>()
    private var allowContradictingEvents = false
    private var initialCapacityKnown = false

    fun setAllowContradictingEvents(allowContradictingEvents: Boolean) {
        this.allowContradictingEvents = allowContradictingEvents
    }

    /** Applies an update from [startIndex] inclusive to [endIndex] exclusive. */
    fun targetUpdate(startIndex: Int, endIndex: Int, oldValue: E, newValue: E) {
        if (!initialCapacityKnown) ensureCapacity(endIndex)

        val insertedValues = deltaValues(ListEvent.unknownValue(), newValue)
        for (index in startIndex until endIndex) {
            val overallIndex = tree.convertIndexColor(index, TARGET_INDICES, ALL_INDICES)
            val standingChange = tree[overallIndex, ALL_INDICES]
            if (standingChange.color == INSERT) {
                tree.set(overallIndex, ALL_INDICES, INSERT, insertedValues, 1)
                continue
            }

            val effectiveOldValue =
                if (standingChange.color == UPDATE) standingChange.get().oldValue else oldValue
            val updateValues = deltaValues(effectiveOldValue, newValue)
            tree.set(overallIndex, ALL_INDICES, UPDATE, updateValues, 1)
        }
    }

    /** Inserts values from [startIndex] inclusive to [endIndex] exclusive. */
    fun targetInsert(startIndex: Int, endIndex: Int, newValue: E) {
        if (!initialCapacityKnown) ensureCapacity(endIndex)
        tree.add(
            startIndex,
            TARGET_INDICES,
            INSERT,
            deltaValues(ListEvent.unknownValue(), newValue),
            endIndex - startIndex,
        )
    }

    /** Deletes values from [startIndex] inclusive to [endIndex] exclusive. */
    fun targetDelete(startIndex: Int, endIndex: Int, oldValue: E) {
        if (!initialCapacityKnown) ensureCapacity(endIndex)

        for (index in startIndex until endIndex) {
            if (startIndex > 0 && startIndex > tree.size(TARGET_INDICES)) {
                throw IllegalArgumentException()
            }

            val overallIndex = tree.convertIndexColor(startIndex, TARGET_INDICES, ALL_INDICES)
            val standingChange = tree[overallIndex, ALL_INDICES]
            if (standingChange.color == INSERT) {
                check(allowContradictingEvents) {
                    "Remove $index undoes prior insert at the same index! Consider enabling contradicting events."
                }
                tree.remove(overallIndex, ALL_INDICES, 1)
                continue
            }

            val effectiveOldValue =
                if (standingChange.color == UPDATE) standingChange.get().oldValue else oldValue
            val deleteValues = deltaValues(effectiveOldValue, ListEvent.unknownValue())
            tree.set(overallIndex, ALL_INDICES, DELETE, deleteValues, 1)
        }
    }

    fun reset(size: Int) {
        tree.clear()
        deltaValuesByOldValue.clear()
        initialCapacityKnown = true
        ensureCapacity(size)
    }

    fun addAll(blocks: BlockSequence<E>) {
        val iterator = blocks.iterator()
        while (iterator.nextBlock()) {
            when (iterator.type) {
                ListEvent.INSERT -> targetInsert(iterator.blockStart, iterator.blockEnd, iterator.newValue)
                ListEvent.UPDATE ->
                    targetUpdate(iterator.blockStart, iterator.blockEnd, iterator.oldValue, iterator.newValue)

                ListEvent.DELETE -> targetDelete(iterator.blockStart, iterator.blockEnd, iterator.oldValue)
                else -> throw IllegalStateException()
            }
        }
    }

    val isEmpty: Boolean
        get() = tree.size(CHANGE_INDICES) == 0

    fun iterator(): Iterator<E> = Iterator.create(tree)

    override fun toString(): String = tree.asSequenceOfColors()

    private fun ensureCapacity(size: Int) {
        val currentSize = tree.size(TARGET_INDICES)
        val delta = size - currentSize
        if (delta <= 0) return

        tree.add(
            tree.size(ALL_INDICES),
            ALL_INDICES,
            NO_CHANGE,
            deltaValues(ListEvent.unknownValue(), ListEvent.unknownValue()),
            delta,
        )
    }

    private fun deltaValues(oldValue: E, newValue: E): DeltaValues<E> {
        val valuesByNewValue =
            deltaValuesByOldValue[oldValue]
                ?: IdentityHashMap<E, DeltaValues<E>>().also { deltaValuesByOldValue[oldValue] = it }
        return valuesByNewValue[newValue]
            ?: DeltaValues(oldValue, newValue).also { valuesByNewValue[newValue] = it }
    }

    /** Iterates through the changes in this tree. */
    class Iterator<E> private constructor(
        private val tree: FourColorTree<DeltaValues<E>>,
        private val treeIterator: FourColorTreeIterator<DeltaValues<E>>,
    ) {
        fun copy(): Iterator<E> = Iterator(tree, treeIterator.copy())

        val index: Int
            get() = treeIterator.index(TARGET_INDICES)

        val endIndex: Int
            get() = treeIterator.nodeStartIndex(TARGET_INDICES) + treeIterator.nodeSize(ALL_INDICES)

        val type: Int
            get() =
                when (treeIterator.color()) {
                    INSERT -> ListEvent.INSERT
                    UPDATE -> ListEvent.UPDATE
                    DELETE -> ListEvent.DELETE
                    else -> throw IllegalStateException()
                }

        val oldValue: E
            get() = treeIterator.node().get().oldValue

        val newValue: E
            get() = treeIterator.node().get().newValue

        fun next(): Boolean {
            if (!hasNext()) return false
            treeIterator.next(CHANGE_INDICES)
            return true
        }

        fun nextNode(): Boolean {
            if (!hasNextNode()) return false
            treeIterator.nextNode(CHANGE_INDICES)
            return true
        }

        fun hasNext(): Boolean = treeIterator.hasNext(CHANGE_INDICES)

        fun hasNextNode(): Boolean = treeIterator.hasNextNode(CHANGE_INDICES)

        companion object {
            @JvmSynthetic
            internal fun <E> create(tree: FourColorTree<DeltaValues<E>>): Iterator<E> =
                Iterator(tree, FourColorTreeIterator(tree))
        }
    }

    internal class DeltaValues<E>(val oldValue: E, val newValue: E)

    private companion object {
        private val BYTE_CODER = ListToByteCoder(listOf("+", "U", "X", "_"))
        private val INSERT = BYTE_CODER.colorToByte("+")
        private val UPDATE = BYTE_CODER.colorToByte("U")
        private val DELETE = BYTE_CODER.colorToByte("X")
        private val NO_CHANGE = BYTE_CODER.colorToByte("_")

        private val TARGET_INDICES = BYTE_CODER.colorsToByte(listOf("U", "+", "_"))
        private val ALL_INDICES = BYTE_CODER.colorsToByte(listOf("U", "X", "+", "_"))
        private val CHANGE_INDICES = BYTE_CODER.colorsToByte(listOf("U", "X", "+"))

    }
}
