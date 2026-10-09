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

import ca.odell.glazedlists.event.ListEventAssembler
import ca.odell.glazedlists.event.ListEventPublisher
import ca.odell.glazedlists.impl.UpgradeDetectingReadWriteLock
import java.util.*
import java.util.concurrent.locks.ReadWriteLock
import java.util.function.Consumer
import java.util.function.Predicate
import java.util.function.UnaryOperator
import java.util.stream.Stream

/** A writable event list backed by an [ArrayList]. */
@Suppress("INAPPLICABLE_JVM_NAME")
class BasicEventList<E> : AbstractEventList<E>, RandomAccess {
    private val data: MutableList<E>

    constructor() : this(UpgradeDetectingReadWriteLock())

    constructor(readWriteLock: ReadWriteLock?) : this(null, readWriteLock)

    constructor(initialCapacity: Int) : this(initialCapacity, null, UpgradeDetectingReadWriteLock())

    constructor(publisher: ListEventPublisher?, readWriteLock: ReadWriteLock?) :
            this(10, publisher, readWriteLock)

    constructor(
        initialCapacity: Int,
        publisher: ListEventPublisher?,
        readWriteLock: ReadWriteLock?,
    ) : super(publisher ?: ListEventAssembler.createListEventPublisher()) {
        data = ArrayList(initialCapacity)
        this.readWriteLock = readWriteLock ?: UpgradeDetectingReadWriteLock()
    }

    override fun add(index: Int, element: E) {
        checkPositionIndex(index)
        updates.beginEvent()
        updates.elementInserted(index, element)
        data.add(index, element)
        updates.commitEvent()
    }

    override fun add(element: E): Boolean {
        updates.beginEvent()
        updates.elementInserted(size, element)
        val result = data.add(element)
        updates.commitEvent()
        return result
    }

    override fun addAll(elements: Collection<E>): Boolean = addAll(size, elements)

    override fun addAll(index: Int, elements: Collection<E>): Boolean {
        checkPositionIndex(index)
        if (elements.isEmpty()) return false

        val inserted = ArrayList(elements)

        updates.beginEvent()
        inserted.forEachIndexed { offset, element ->
            updates.elementInserted(index + offset, element)
        }
        data.addAll(index, inserted)
        updates.commitEvent()
        return true
    }

    @JvmName("remove")
    override fun removeAt(index: Int): E {
        val removed = data[index]
        updates.beginEvent()
        data.removeAt(index)
        updates.elementDeleted(index, removed)
        updates.commitEvent()
        return removed
    }

    override fun remove(element: E): Boolean {
        val index = data.indexOf(element)
        if (index == -1) return false
        removeAt(index)
        return true
    }

    override fun clear() {
        if (isEmpty()) return

        updates.beginEvent()
        data.forEach { updates.elementDeleted(0, it) }
        data.clear()
        updates.commitEvent()
    }

    override fun set(index: Int, element: E): E {
        val previous = data[index]
        updates.beginEvent()
        data[index] = element
        updates.elementUpdated(index, previous, element)
        updates.commitEvent()
        return previous
    }

    override fun get(index: Int): E = data[index]

    @get:JvmName("size")
    override val size: Int
        get() = data.size

    override fun removeIf(filter: Predicate<in E>): Boolean {
        if (isEmpty()) return false

        val removedIndexes = BooleanArray(data.size)
        val removedValues = ArrayList<E>()
        for (index in data.indices) {
            if (filter.test(data[index])) {
                removedIndexes[index] = true
                removedValues += data[index]
            }
        }
        if (removedValues.isEmpty()) return false

        var writeIndex = 0
        for (readIndex in data.indices) {
            if (!removedIndexes[readIndex]) {
                if (writeIndex != readIndex) data[writeIndex] = data[readIndex]
                writeIndex++
            }
        }
        data.subList(writeIndex, data.size).clear()

        updates.beginEvent()
        var removedBefore = 0
        for (originalIndex in removedIndexes.indices) {
            if (removedIndexes[originalIndex]) {
                updates.elementDeleted(originalIndex - removedBefore, removedValues[removedBefore])
                removedBefore++
            }
        }
        updates.commitEvent()
        return true
    }

    override fun replaceAll(operator: UnaryOperator<E>) {
        val replacements = ArrayList<E>(data.size)
        data.forEach { replacements += operator.apply(it) }

        updates.beginEvent()
        for (index in data.indices) {
            val oldValue = data[index]
            val newValue = replacements[index]
            if (oldValue !== newValue) {
                data[index] = newValue
                updates.elementUpdated(index, oldValue, newValue)
            }
        }
        updates.commitEvent()
    }

    override fun forEach(action: Consumer<in E>) {
        data.forEach(action)
    }

    override fun stream(): Stream<E> = data.stream()

    override fun parallelStream(): Stream<E> = data.parallelStream()

    override fun spliterator(): Spliterator<E> = data.spliterator()

    override fun dispose() = Unit

    private fun checkPositionIndex(index: Int) {
        if (index !in 0..data.size) {
            throw IndexOutOfBoundsException("Cannot add at $index on list of size ${data.size}")
        }
    }
}
