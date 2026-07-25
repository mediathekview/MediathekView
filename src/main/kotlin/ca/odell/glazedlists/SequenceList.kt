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
package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEvent

/** A read-only sequence that minimally covers the ordered values in a source list. */
@Suppress(
    "INAPPLICABLE_JVM_NAME",
    "UNCHECKED_CAST",
)
class SequenceList<E> private constructor(
    source: SortedList<E>,
    private val sequencer: Sequencer<E>,
    private val comparator: Comparator<in E>,
) : TransformedList<E, E>(source), RandomAccess {
    private val sequence: MutableList<E> = ArrayList()

    constructor(source: EventList<E>, sequencer: Sequencer<E>) :
            this(
                source,
                sequencer,
                GlazedLists.comparableComparator<String>() as Comparator<in E>,
            )

    constructor(
        source: EventList<E>,
        sequencer: Sequencer<E>,
        comparator: Comparator<in E>,
    ) : this(SortedList(source, comparator), sequencer, comparator)

    init {
        updateSequence()
        source.addListEventListener(this)
    }

    override fun isWritable(): Boolean = false

    @get:JvmName("size")
    override val size: Int
        get() = sequence.size

    override fun get(index: Int): E = sequence[index]

    /** Produces the previous sequence value, or [value] when it is already a sequence value. */
    fun getPreviousSequenceValue(value: E): E =
        if (isSequenceValue(value)) value else sequencer.previous(value)

    /** Produces the next sequence value, or [value] when it is already a sequence value. */
    fun getNextSequenceValue(value: E): E =
        if (isSequenceValue(value)) value else sequencer.next(value)

    override fun listChanged(listChanges: ListEvent<E>) {
        updateSequence()
    }

    private fun isSequenceValue(value: E): Boolean {
        val sequencedValue = sequencer.previous(sequencer.next(value))
        return comparator.compare(value, sequencedValue) == 0
    }

    private fun updateSequence() {
        updates.beginEvent()

        val sortedSource = source!!
        if (sortedSource.isEmpty()) {
            while (sequence.isNotEmpty()) {
                updates.elementDeleted(0, sequence.removeAt(0))
            }
        } else {
            if (isEmpty()) {
                val value = sortedSource[0]
                val previousSequenceValue = getPreviousSequenceValue(value)
                val nextSequenceValue = getNextSequenceValue(value)

                sequence.add(0, previousSequenceValue)
                updates.elementInserted(0, previousSequenceValue)
                sequence.add(nextSequenceValue)
                updates.elementInserted(1, nextSequenceValue)
            }

            val firstSourceValue = sortedSource[0]
            while (comparator.compare(firstSourceValue, get(0)) < 0) {
                val element = sequencer.previous(get(0))
                sequence.add(0, element)
                updates.elementInserted(0, element)
            }

            while (comparator.compare(get(1), firstSourceValue) < 0) {
                val oldValue = sequence.removeAt(0)
                updates.elementDeleted(0, oldValue)
            }

            val lastSourceValue = sortedSource[sortedSource.size - 1]
            while (comparator.compare(lastSourceValue, get(size - 1)) > 0) {
                val element = sequencer.next(get(size - 1))
                val index = size
                sequence.add(element)
                updates.elementInserted(index, element)
            }

            while (comparator.compare(get(size - 2), lastSourceValue) > 0) {
                val lastIndex = size - 1
                updates.elementDeleted(lastIndex, sequence.removeAt(lastIndex))
            }
        }

        updates.commitEvent()
    }

    /** Produces adjacent sequence values around an arbitrary value. */
    interface Sequencer<E> {
        fun previous(value: E): E

        fun next(value: E): E
    }
}
