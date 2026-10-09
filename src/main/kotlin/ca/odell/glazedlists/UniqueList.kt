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
import ca.odell.glazedlists.impl.Grouper
import ca.odell.glazedlists.impl.adt.BarcodeIterator
import java.util.*

@Suppress("INAPPLICABLE_JVM_NAME", "REDUNDANT_PROJECTION")
class UniqueList<E> private constructor(
    source: SortedList<E>,
) : TransformedList<E, E>(source) {
    private val grouper = Grouper(source, GrouperClient())

    constructor(source: EventList<E>) : this(source, comparableComparator())

    constructor(source: EventList<E>, comparator: Comparator<in E>?) : this(
        SortedList(source, comparator),
    )

    init {
        source.addListEventListener(this)
    }

    private open inner class GrouperClient : Grouper.Client<E> {
        override fun groupChanged(
            index: Int,
            groupIndex: Int,
            groupChangeType: Int,
            primary: Boolean,
            elementChangeType: Int,
            oldValue: E,
            newValue: E,
            updateNextSeparator: Boolean,
            joinRight: Boolean,
        ) {
            when (groupChangeType) {
                ListEvent.INSERT -> updates.elementInserted(groupIndex, newValue)
                ListEvent.UPDATE -> updates.elementUpdated(groupIndex, oldValue, newValue)
                ListEvent.DELETE -> updates.elementDeleted(groupIndex, oldValue)
                else -> throw IllegalStateException("Unrecognized groupChangeType: $groupChangeType")
            }
        }
    }

    @Suppress("UNCHECKED_CAST")
    fun setComparator(comparator: Comparator<in E>?) {
        val effectiveComparator: Comparator<in E> = comparator ?: comparableComparator()
        (source as SortedList<E>).comparator = effectiveComparator
    }

    @get:JvmName("size")
    override val size: Int
        get() = grouper.barcode.colourSize(Grouper.UNIQUE)

    override fun getSourceIndex(mutationIndex: Int): Int =
        if (mutationIndex == size) source!!.size else grouper.barcode.getIndex(mutationIndex, Grouper.UNIQUE)

    private fun getEndIndex(index: Int): Int =
        if (index == size - 1) source!!.size else getSourceIndex(index + 1)

    @JvmName("remove")
    override fun removeAt(index: Int): E {
        if (index !in indices) {
            throw IndexOutOfBoundsException("Cannot remove at $index on list of size $size")
        }

        updates.beginEvent(true)
        val result = get(index)
        val startIndex = getSourceIndex(index)
        val endIndex = getEndIndex(index)
        source!!.subList(startIndex, endIndex).clear()
        updates.commitEvent()
        return result
    }

    override fun set(index: Int, element: E): E {
        if (index !in indices) {
            throw IndexOutOfBoundsException("Cannot set at $index on list of size $size")
        }

        updates.beginEvent(true)
        val startIndex = getSourceIndex(index) + 1
        val endIndex = getEndIndex(index)
        if (endIndex > startIndex) {
            source!!.subList(startIndex, endIndex).clear()
        }
        val result = super.set(index, element)
        updates.commitEvent()
        return result
    }

    @Suppress("ReplaceJavaStaticMethodWithKotlinAnalog")
    override fun indexOf(element: E): Int {
        val index = Collections.binarySearch(this, element, (source as SortedList<E>).comparator)
        return if (index < 0) -1 else index
    }

    override fun isWritable(): Boolean = true

    override fun listChanged(listChanges: ListEvent<E>) {
        updates.beginEvent(true)

        val sortedSource = source as SortedList<E>
        val sourceComparator = sortedSource.comparator
        if (sourceComparator !== grouper.comparator) {
            check(listChanges.isReordering) { "source comparator changed without reordering!" }

            val reorderingMap = listChanges.reorderMap
            val reverseReorderingMap = IntArray(reorderingMap.size)
            for (r in reorderingMap.indices) {
                reverseReorderingMap[reorderingMap[r]] = r
            }
            val oldGroups: BarcodeIterator = grouper.barcode.iterator()
            while (oldGroups.hasNextBlack()) {
                oldGroups.nextBlack()
                val sourceIndex = oldGroups.index
                updates.elementDeleted(0, sortedSource[reverseReorderingMap[sourceIndex]])
            }
            grouper.barcode.clear()

            grouper.comparator = sourceComparator

            var uniqueIndex = 0
            val newGroups: BarcodeIterator = grouper.barcode.iterator()
            while (newGroups.hasNextBlack()) {
                newGroups.nextBlack()
                val sourceIndex = newGroups.index
                updates.elementInserted(uniqueIndex++, sortedSource[sourceIndex])
            }
        } else {
            grouper.listChanged(listChanges)
        }

        updates.commitEvent()
    }

    fun getCount(index: Int): Int = getEndIndex(index) - getSourceIndex(index)

    fun getCount(value: E): Int {
        val index = indexOf(value)
        return if (index == -1) 0 else getCount(index)
    }

    fun getAll(index: Int): MutableList<E> {
        val startIndex = getSourceIndex(index)
        val endIndex = getEndIndex(index)
        return ArrayList(source!!.subList(startIndex, endIndex))
    }

    fun getAll(value: E): MutableList<E> {
        val index = indexOf(value)
        return if (index == -1) Collections.emptyList() else getAll(index)
    }

    override fun dispose() {
        (source as SortedList<E>).dispose()
        super.dispose()
    }


    companion object {
        fun <E> create(source: EventList<E>): UniqueList<E> where E : Comparable<in E> = UniqueList(source)

        @Suppress("UNCHECKED_CAST")
        private fun <E> comparableComparator(): Comparator<in E> =
            GlazedLists.comparableComparator<Comparable<Any?>>() as Comparator<in E>
    }
}
