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
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.event.ListEventPublisher
import ca.odell.glazedlists.impl.EventListIterator
import ca.odell.glazedlists.impl.SimpleIterator
import ca.odell.glazedlists.impl.SubEventList
import org.jspecify.annotations.NonNull
import java.util.concurrent.locks.ReadWriteLock
import java.util.function.Predicate
import java.util.function.UnaryOperator
import java.lang.reflect.Array as ReflectArray

@Suppress("INAPPLICABLE_JVM_NAME", "UNCHECKED_CAST")
abstract class AbstractEventList<E> protected constructor(
    initialPublisher: ListEventPublisher,
) : EventList<E> {
    /** the read/write lock provides mutual exclusion to access */
    private lateinit var readWriteLockBacking: ReadWriteLock

    override var readWriteLock: ReadWriteLock
        get() = readWriteLockBacking
        @JvmSynthetic
        protected set(value) {
            readWriteLockBacking = value
        }

    /** the publisher manages the distribution of changes */
    override var publisher: ListEventPublisher = initialPublisher
        @JvmSynthetic protected set

    /** the change event and notification system */
    @JvmField
    protected var updates: ListEventAssembler<E> = ListEventAssembler(this, initialPublisher)

    @JvmSynthetic
    internal fun eventAssembler(): ListEventAssembler<E> = updates

    protected constructor() : this(ListEventAssembler.createListEventPublisher())

    override fun addListEventListener(listChangeListener: ListEventListener<in E>) {
        updates.addListEventListener(listChangeListener)
    }

    override fun removeListEventListener(listChangeListener: ListEventListener<in E>) {
        updates.removeListEventListener(listChangeListener)
    }

    @get:JvmName("size")
    abstract override val size: Int

    override fun isEmpty(): Boolean = size == 0

    override fun contains(element: E): Boolean {
        for (value in this) {
            if (element == value) return true
        }
        return false
    }

    @Suppress("CAST_NEVER_SUCCEEDS")
    override fun iterator(): @NonNull MutableIterator<E> = SimpleIterator(this) as MutableIterator<E>

    @Suppress("UseWithIndex")
    open fun toArray(): @NonNull Array<Any?> {
        val array = Array<Any?>(size) { null }
        var index = 0
        for (value in this) {
            array[index] = value
            index++
        }
        return array
    }

    @Suppress("UseWithIndex")
    open fun <T> toArray(array: @NonNull Array<T>): @NonNull Array<T> {
        var target = array
        if (target.size < size) {
            target = ReflectArray.newInstance(target.javaClass.componentType, size) as Array<T>
        } else if (target.size > size) {
            target[size] = null as T
        }

        var index = 0
        for (value in this) {
            target[index] = value as T
            index++
        }
        return target
    }

    override fun add(element: E): Boolean {
        val initialSize = size
        add(size, element)
        return size != initialSize
    }

    override fun remove(element: E): Boolean {
        val index = indexOf(element)
        if (index == -1) return false
        removeAt(index)
        return true
    }

    override fun containsAll(elements: Collection<E>): Boolean {
        for (value in elements) {
            if (!contains(value)) return false
        }
        return true
    }

    override fun addAll(elements: @NonNull Collection<E>): Boolean = addAll(size, elements)

    @Suppress("ConvertTwoComparisonsToRangeCheck")
    override fun addAll(index: Int, elements: @NonNull Collection<E>): Boolean {
        if (index < 0 || index > size) {
            throw IndexOutOfBoundsException("Cannot add at $index on list of size $size")
        }
        if (elements.isEmpty()) return false

        val initialSize = size
        updates.beginEvent(true)
        var insertionIndex = index
        for (value in elements) {
            try {
                add(insertionIndex, value)
            } catch (failure: UnsupportedOperationException) {
                updates.discardEvent()
                throw failure
            }

            if (insertionIndex < size) {
                insertionIndex++
            }
        }
        updates.commitEvent()

        return size != initialSize
    }

    @Suppress("RedundantIf")
    override fun removeAll(elements: @NonNull Collection<E>): Boolean {
        if (isEmpty()) return false
        return removeIf(elements::contains)
    }

    override fun retainAll(elements: @NonNull Collection<E>): Boolean {
        return removeIf(Predicate { value -> !elements.contains(value) })
    }

    override fun clear() {
        if (isEmpty()) return
        removeIf(Predicate { true })
    }

    override fun removeIf(filter: @NonNull Predicate<in E>): Boolean {
        if (isEmpty()) return false

        updates.beginEvent(true)
        var removed = false
        val each = iterator()
        while (each.hasNext()) {
            if (filter.test(each.next())) {
                each.remove()
                removed = true
            }
        }
        updates.commitEvent()
        return removed
    }

    override fun replaceAll(operator: @NonNull UnaryOperator<E>) {
        updates.beginEvent(true)
        for (index in size - 1 downTo 0) {
            val oldValue = get(index)
            val newValue = operator.apply(oldValue)
            if (oldValue !== newValue) {
                try {
                    set(index, newValue)
                } catch (failure: UnsupportedOperationException) {
                    updates.discardEvent()
                    throw failure
                }
                updates.elementUpdated(index, oldValue, newValue)
            }
        }
        updates.commitEvent()
    }

    override fun equals(other: Any?): Boolean {
        if (other === this) return true
        if (other == null) return false
        if (other !is List<*>) return false
        if (other.size != size) return false

        val iterA = iterator()
        val iterB = other.iterator()
        while (iterA.hasNext() && iterB.hasNext()) {
            if (iterA.next() != iterB.next()) return false
        }

        return true
    }

    override fun hashCode(): Int {
        var hashCode = 1
        for (value in this) {
            hashCode = 31 * hashCode + value.hashCode()
        }
        return hashCode
    }

    abstract override fun get(index: Int): E

    override fun set(index: Int, element: E): E {
        throw UnsupportedOperationException("this list does not support set()")
    }

    override fun add(index: Int, element: E) {
        throw UnsupportedOperationException("this list does not support add()")
    }

    @JvmName("remove")
    override fun removeAt(index: Int): E {
        throw UnsupportedOperationException("this list does not support remove()")
    }

    override fun indexOf(element: E): Int {
        var index = 0
        for (value in this) {
            if (element == value) {
                return index
            } else {
                index++
            }
        }
        return -1
    }

    override fun lastIndexOf(element: E): Int {
        for (index in size - 1 downTo 0) {
            if (element == get(index)) return index
        }
        return -1
    }

    override fun listIterator(): @NonNull MutableListIterator<E> = listIterator(0)

    @Suppress("CAST_NEVER_SUCCEEDS")
    override fun listIterator(index: Int): @NonNull MutableListIterator<E> =
        EventListIterator(this, index) as MutableListIterator<E>

    override fun subList(fromIndex: Int, toIndex: Int): @NonNull MutableList<E> =
        SubEventList(this, fromIndex, toIndex, true)

    override fun toString(): String {
        val result = StringBuilder()
        result.append("[")
        val iterator = iterator()
        while (iterator.hasNext()) {
            result.append(iterator.next())
            if (iterator.hasNext()) result.append(", ")
        }
        result.append("]")
        return result.toString()
    }

}
