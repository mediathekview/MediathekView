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
import ca.odell.glazedlists.event.ListEventAssembler
import java.util.function.Predicate

/** A writable, index-preserving view that maps each element of a source [EventList]. */
@Suppress("INAPPLICABLE_JVM_NAME", "UNCHECKED_CAST")
class FunctionList<S, E> : TransformedList<S, E>, RandomAccess {
    private val sourceElements = ArrayList<S>()
    private var needDispose = false
    private val mappedElements: MutableList<E>
    private lateinit var forward: AdvancedFunction<S, E>
    private var reverse: ((E) -> S)? = null

    constructor(source: EventList<S>, forward: (S) -> E) : this(source, forward, null)

    constructor(
        source: EventList<S>,
        forward: (S) -> E,
        reverse: ((E) -> S)?,
    ) : super(source) {
        updateForwardFunction(forward)
        this.reverse = reverse

        mappedElements = ArrayList(source.size)
        source.forEach { mappedElements.add(mapForward(it)) }
        source.addListEventListener(this)
    }

    /** The function used to map source values. */
    var forwardFunction: (S) -> E
        get() {
            val current = forward
            return if (current is AdvancedFunctionAdapter<*, *>) {
                (current as AdvancedFunctionAdapter<S, E>).delegate
            } else {
                current
            }
        }
        set(value) {
            val currentSource = source!!
            val sourceSnapshot = ArrayList<S>(currentSource.size)
            val remapped = ArrayList<E>(currentSource.size)
            currentSource.forEach { sourceValue ->
                sourceSnapshot += sourceValue
                remapped += value(sourceValue)
            }

            updateForwardFunction(value, sourceSnapshot)
            updates.beginEvent(true)
            for (index in remapped.indices) {
                val newValue = remapped[index]
                val oldValue = mappedElements.set(index, newValue)
                updates.elementUpdated(index, oldValue, newValue)
            }
            updates.commitEvent()
        }

    /** The optional function used to write mapped values back to the source. */
    var reverseFunction: ((E) -> S)?
        get() = reverse
        set(value) {
            reverse = value
        }

    private fun mapForward(sourceValue: S): E = forward(sourceValue)

    private fun remapForward(transformedValue: E, sourceValue: S): E =
        forward.reevaluate(sourceValue, transformedValue)

    private fun mapReverse(value: E): S {
        val currentReverse = reverse
            ?: throw IllegalStateException(
                "A reverse mapping function must be specified to support this List operation",
            )
        return currentReverse(value)
    }

    private fun updateForwardFunction(newForward: (S) -> E, sourceSnapshot: Collection<S> = source!!) {
        if (newForward is AdvancedFunction<*, *>) {
            forward = newForward as AdvancedFunction<S, E>
            needDispose = true
            sourceElements.ensureCapacity(sourceSnapshot.size)
            sourceElements.clear()
            sourceElements.addAll(sourceSnapshot)
        } else {
            forward = AdvancedFunctionAdapter(newForward)
            needDispose = false
            sourceElements.clear()
            sourceElements.trimToSize()
        }
    }

    override fun isWritable(): Boolean = true

    override fun listChanged(listChanges: ListEvent<S>) {
        if (listChanges.isReordering) {
            val reorderMap = listChanges.reorderMap
            val originalMappedElements = ArrayList(mappedElements)
            for (index in reorderMap.indices) {
                mappedElements[index] = originalMappedElements[reorderMap[index]]
            }
            if (needDispose) {
                val originalSourceElements = ArrayList(sourceElements)
                for (index in reorderMap.indices) {
                    sourceElements[index] = originalSourceElements[reorderMap[index]]
                }
            }
            updates.beginEvent(true)
            updates.reorder(reorderMap)
            updates.commitEvent()
        } else {
            val currentSource = source!!
            val mappedChanges = ArrayList<MappedChange<E>>()
            try {
                while (listChanges.next()) {
                    val changeIndex = listChanges.index
                    when (listChanges.type) {
                        ListEvent.INSERT -> {
                            val newValue = currentSource[changeIndex]
                            val transformed = mapForward(newValue)
                            if (needDispose) sourceElements.add(changeIndex, newValue)
                            mappedElements.add(changeIndex, transformed)
                            mappedChanges += MappedChange(
                                ListEvent.INSERT,
                                changeIndex,
                                ListEvent.unknownValue(),
                                transformed,
                            )
                        }

                        ListEvent.UPDATE -> {
                            val oldTransformed = mappedElements[changeIndex]
                            val newValue = currentSource[changeIndex]
                            val newTransformed = remapForward(oldTransformed, newValue)
                            if (needDispose) sourceElements[changeIndex] = newValue
                            mappedElements[changeIndex] = newTransformed
                            mappedChanges += MappedChange(
                                ListEvent.UPDATE,
                                changeIndex,
                                oldTransformed,
                                newTransformed,
                            )
                        }

                        ListEvent.DELETE -> {
                            val oldTransformed = mappedElements.removeAt(changeIndex)
                            if (needDispose) {
                                val oldValue = sourceElements.removeAt(changeIndex)
                                forward.dispose(oldValue, oldTransformed)
                            }
                            mappedChanges += MappedChange(
                                ListEvent.DELETE,
                                changeIndex,
                                oldTransformed,
                                ListEvent.unknownValue(),
                            )
                        }
                    }
                }
            } catch (failure: Throwable) {
                recoverFromSource(failure)
                throw failure
            }

            updates.beginEvent(true)
            for ((type, index, oldValue, newValue) in mappedChanges) {
                when (type) {
                    ListEvent.INSERT -> updates.elementInserted(index, newValue)
                    ListEvent.UPDATE -> updates.elementUpdated(index, oldValue, newValue)
                    ListEvent.DELETE -> updates.elementDeleted(index, oldValue)
                }
            }
            updates.commitEvent()
        }
    }

    private fun recoverFromSource(originalFailure: Throwable) {
        try {
            val currentSource = source!!
            val recoveredSourceElements = ArrayList<S>(currentSource.size)
            val recoveredMappedElements = ArrayList<E>(currentSource.size)
            currentSource.forEach { sourceValue ->
                recoveredSourceElements += sourceValue
                recoveredMappedElements += mapForward(sourceValue)
            }

            mappedElements.clear()
            mappedElements.addAll(recoveredMappedElements)
            if (needDispose) {
                sourceElements.clear()
                sourceElements.addAll(recoveredSourceElements)
            }
        } catch (recoveryFailure: Throwable) {
            if (recoveryFailure !== originalFailure) originalFailure.addSuppressed(recoveryFailure)
        }
    }

    override fun get(index: Int): E = mappedElements[index]

    @JvmName("remove")
    override fun removeAt(index: Int): E {
        val removed = get(index)
        source!!.removeAt(index)
        return removed
    }

    override fun set(index: Int, element: E): E {
        val updated = get(index)
        source!![index] = mapReverse(element)
        return updated
    }

    override fun add(index: Int, element: E) {
        if (index !in 0..size) {
            throw IndexOutOfBoundsException("Cannot add at $index on list of size $size")
        }
        source!!.add(index, mapReverse(element))
    }

    override fun addAll(index: Int, elements: Collection<E>): Boolean {
        if (index !in 0..size) {
            throw IndexOutOfBoundsException("Cannot add at $index on list of size $size")
        }
        if (elements.isEmpty()) return false

        val sourceValues = ArrayList<S>(elements.size)
        elements.forEach { sourceValues += mapReverse(it) }
        return source!!.addAll(index, sourceValues)
    }

    override fun removeIf(filter: Predicate<in E>): Boolean {
        val matchedIndexes = ArrayList<Int>()
        for (index in size - 1 downTo 0) {
            if (filter.test(mappedElements[index])) matchedIndexes += index
        }
        if (matchedIndexes.isEmpty()) return false

        val sourceUpdates = sourceUpdates()
        sourceUpdates?.beginEvent(true)
        try {
            for (index in matchedIndexes) {
                removeAt(index)
            }
        } finally {
            sourceUpdates?.commitEvent()
        }
        return true
    }

    private fun sourceUpdates(): ListEventAssembler<*>? {
        val currentSource = source!!
        if (currentSource !is AbstractEventList<*>) return null
        return currentSource.eventAssembler()
    }

    /** A mapping function with update and disposal lifecycle hooks. */
    interface AdvancedFunction<A, B> : (A) -> B {
        fun reevaluate(sourceValue: A, transformedValue: B): B

        fun dispose(sourceValue: A, transformedValue: B)
    }

    private class AdvancedFunctionAdapter<A, B>(val delegate: (A) -> B) : AdvancedFunction<A, B> {
        override fun invoke(sourceValue: A): B = delegate(sourceValue)

        override fun reevaluate(sourceValue: A, transformedValue: B): B = invoke(sourceValue)

        override fun dispose(sourceValue: A, transformedValue: B) = Unit
    }

    private data class MappedChange<E>(
        val type: Int,
        val index: Int,
        val oldValue: E,
        val newValue: E,
    )
}
