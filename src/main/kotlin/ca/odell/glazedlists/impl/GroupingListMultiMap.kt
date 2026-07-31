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
package ca.odell.glazedlists.impl

import ca.odell.glazedlists.*
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import java.lang.reflect.Array as ReflectArray

/** A mutable map from calculated keys to live groups in an observable source list. */
@Suppress(
    "INAPPLICABLE_JVM_NAME",
    "PLATFORM_CLASS_MAPPED_TO_KOTLIN",
    "UNCHECKED_CAST",
)
internal class GroupingListMultiMap<K, V> : DisposableMap<K, List<V>>,
    ListEventListener<List<V>> {
    private val valueList: FunctionList<List<V>, List<V>>
    private val keyList: MutableList<K>
    private var cachedKeySet: KeySet<K, V>? = null
    private val keyFunction: (V) -> K
    private val delegate: MutableMap<K, List<V>>
    private var cachedEntrySet: MutableSet<MutableMap.MutableEntry<K, List<V>>>? = null

    constructor(
        source: EventList<V>,
        keyFunction: (V) -> K,
        keyGrouper: Comparator<in K>,
    ) : super() {
        this.keyFunction = keyFunction
        values = GroupingList(source, FunctionComparator(keyFunction, keyGrouper))
        valueList = FunctionList(values, ValueListFunction(this))
        valueList.addListEventListener(this)

        keyList = BasicEventList(values.size)
        delegate = HashMap(values.size)
        for (value in valueList) {
            val key = key(value)
            keyList.add(key)
            delegate[key] = value
        }
    }

    override fun dispose() {
        valueList.removeListEventListener(this)
        valueList.dispose()
        values.dispose()

        cachedKeySet = null
        cachedEntrySet = null
        keyList.clear()
        delegate.clear()
    }

    @get:JvmName("size")
    override val size: Int
        get() = delegate.size

    override fun isEmpty(): Boolean = delegate.isEmpty()

    override fun containsKey(key: K): Boolean = delegate.containsKey(key)

    override fun containsValue(value: List<V>): Boolean = delegate.containsValue(value)

    override fun get(key: K): MutableList<V>? = delegate[key] as MutableList<V>?

    override fun put(key: K, value: List<V>): MutableList<V>? {
        checkKeyValueAgreement(key, value)

        val removed = remove(key)
        values.add(value)
        return removed
    }

    override fun putAll(from: Map<out K, List<V>>) {
        for ((key, value) in from) {
            checkKeyValueAgreement(key, value)
        }
        for (key in from.keys) {
            remove(key)
        }
        values.addAll(from.values)
    }

    private fun checkKeyValueAgreement(key: K, values: Collection<V>) {
        for (value in values) {
            checkKeyValueAgreement(key, value)
        }
    }

    private fun checkKeyValueAgreement(key: K, value: V) {
        val calculatedKey = key(value)
        require(key == calculatedKey) {
            "The calculated key for the given value ($calculatedKey) does not match the given key ($key)"
        }
    }

    override fun clear() {
        values.clear()
    }

    override fun remove(key: K): MutableList<V>? {
        val index = keyList.indexOf(key)
        return if (index == -1) null else values.removeAt(index)
    }

    override fun remove(key: K, value: List<V>): Boolean {
        val currentValue = get(key)
        if (currentValue != value) {
            return false
        }
        remove(key)
        return true
    }

    @get:JvmName("values")
    override val values: MutableCollection<List<V>>
        field: GroupingList<V>

    @get:JvmName("keySet")
    override val keys: MutableSet<K>
        get() {
            var current = cachedKeySet
            if (current == null) {
                current = KeySet(keyList, values, this)
                cachedKeySet = current
            }
            return current
        }

    @get:JvmName("entrySet")
    override val entries: MutableSet<MutableMap.MutableEntry<K, List<V>>>
        get() {
            var current = cachedEntrySet
            if (current == null) {
                current =
                    EntrySet(keyList, values, delegate, this) as MutableSet<MutableMap.MutableEntry<K, List<V>>>
                cachedEntrySet = current
            }
            return current
        }

    override fun equals(other: Any?): Boolean = delegate == other

    override fun hashCode(): Int = delegate.hashCode()

    override fun listChanged(listChanges: ListEvent<List<V>>) {
        val previousValues = HashMap(delegate)
        while (listChanges.next()) {
            val changeIndex = listChanges.index
            when (listChanges.type) {
                ListEvent.INSERT -> {
                    val inserted = listChanges.sourceList[changeIndex]
                    val key = key(inserted)
                    keyList.add(changeIndex, key)
                    delegate[key] = inserted
                }

                ListEvent.DELETE -> {
                    val deleted = keyList.removeAt(changeIndex)
                    delegate.remove(deleted)
                }

                ListEvent.UPDATE -> {
                    val oldKey = keyList[changeIndex]
                    val updated = listChanges.sourceList[changeIndex]
                    val newKey = key(updated)
                    val oldValue = delegate.remove(oldKey)
                    keyList[changeIndex] = newKey
                    delegate[newKey] = retainLiveValue(oldKey, newKey, oldValue, updated)
                }
            }
        }

        if (keyList.size != valueList.size || delegate.size != valueList.size) {
            rebuildMappings(previousValues)
        }
    }

    private fun retainLiveValue(oldKey: K, newKey: K, oldValue: List<V>?, newValue: List<V>): List<V> {
        if (oldKey != newKey || oldValue !is ValueList<*, *> || newValue !is ValueList<*, *>) {
            return newValue
        }

        val retained = oldValue as ValueList<K, V>
        retained.rebind(newValue as ValueList<K, V>)
        return retained
    }

    private fun rebuildMappings(previousValues: Map<K, List<V>>) {
        val rebuiltKeys = ArrayList<K>(valueList.size)
        val rebuiltDelegate = HashMap<K, List<V>>(valueList.size)
        for (currentValue in valueList) {
            val currentKey = key(currentValue)
            val previousValue = previousValues[currentKey]
            val retainedValue = retainLiveValue(currentKey, currentKey, previousValue, currentValue)
            rebuiltKeys.add(currentKey)
            rebuiltDelegate[currentKey] = retainedValue
        }

        for (index in rebuiltKeys.indices) {
            val expectedKey = rebuiltKeys[index]
            if (index < keyList.size && keyList[index] == expectedKey) continue

            val matchingIndex = keyList.subList(index.coerceAtMost(keyList.size), keyList.size).indexOf(expectedKey)
            if (matchingIndex == -1) {
                keyList.add(index, expectedKey)
            } else {
                repeat(matchingIndex) { keyList.removeAt(index) }
            }
        }
        while (keyList.size > rebuiltKeys.size) {
            keyList.removeAt(keyList.lastIndex)
        }

        delegate.clear()
        delegate.putAll(rebuiltDelegate)
    }

    private fun key(values: List<V>): K {
        if (values.isEmpty()) throw NoSuchElementException()
        return key(values[0])
    }

    private fun key(value: V): K = keyFunction(value)

    @Suppress("PLATFORM_CLASS_MAPPED_TO_KOTLIN")
    private class EntrySet<K, V>(
        private val keyList: MutableList<K>,
        private val groupingList: GroupingList<V>,
        private val delegate: Map<K, List<V>>,
        private val owner: GroupingListMultiMap<K, V>,
    ) : java.util.AbstractSet<Map.Entry<K, List<V>>>() {
        override val size: Int
            get() = keyList.size

        override fun iterator(): MutableIterator<Map.Entry<K, List<V>>> =
            EntrySetIterator(keyList.listIterator(), groupingList, owner)

        override fun contains(element: Map.Entry<K, List<V>>): Boolean = delegate.entries.contains(element)

        override fun remove(element: Map.Entry<K, List<V>>): Boolean {
            if (!contains(element)) return false
            owner.remove(element.key)
            return true
        }

        override fun clear() {
            owner.clear()
        }
    }

    private class EntrySetIterator<K, V>(
        private val keyIterator: MutableListIterator<K>,
        private val groupingList: GroupingList<V>,
        private val owner: GroupingListMultiMap<K, V>,
    ) : MutableIterator<Map.Entry<K, List<V>>> {
        override fun hasNext(): Boolean = keyIterator.hasNext()

        override fun next(): Map.Entry<K, List<V>> {
            val key = keyIterator.next()
            return MultiMapEntry(key, checkNotNull(owner[key]), owner)
        }

        override fun remove() {
            val index = keyIterator.previousIndex()
            if (index == -1) {
                throw IllegalStateException("Cannot remove() without a prior call to next()")
            }
            groupingList.removeAt(index)
        }
    }

    private class MultiMapEntry<K, V>(
        override val key: K,
        initialValue: MutableList<V>,
        private val owner: GroupingListMultiMap<K, V>,
    ) : MutableMap.MutableEntry<K, List<V>> {
        override val value: List<V>
            field: MutableList<V> = initialValue

        override fun setValue(newValue: List<V>): List<V> {
            owner.checkKeyValueAgreement(key, newValue)
            val oldValue = ArrayList(value)
            val replacement = ArrayList(newValue)
            value.addAll(replacement)
            repeat(oldValue.size) { value.removeAt(0) }
            return oldValue
        }

        override fun equals(other: Any?): Boolean =
            other is Map.Entry<*, *> &&
                    key == other.key &&
                    value == other.value

        override fun hashCode(): Int = key.hashCode() xor value.hashCode()

        override fun toString(): String = "$key=$value"
    }

    private class KeySet<K, V>(
        private val keyList: MutableList<K>,
        private val groupingList: GroupingList<V>,
        private val owner: GroupingListMultiMap<K, V>,
    ) : AbstractMutableSet<K>() {
        override val size: Int
            get() = keyList.size

        override fun add(element: K): Boolean = throw UnsupportedOperationException()

        override fun iterator(): MutableIterator<K> = KeySetIterator(keyList.listIterator(), groupingList)

        override fun contains(element: K): Boolean = owner.containsKey(element)

        override fun remove(element: K): Boolean = owner.remove(element) != null

        override fun clear() {
            owner.clear()
        }
    }

    private class KeySetIterator<K, V>(
        private val keyIterator: MutableListIterator<K>,
        private val groupingList: GroupingList<V>,
    ) : MutableIterator<K> {
        override fun hasNext(): Boolean = keyIterator.hasNext()

        override fun next(): K = keyIterator.next()

        override fun remove() {
            val index = keyIterator.previousIndex()
            if (index == -1) {
                throw IllegalStateException("Cannot remove() without a prior call to next()")
            }
            groupingList.removeAt(index)
        }
    }

    private class FunctionComparator<K, V>(
        private val function: (V) -> K,
        private val delegate: Comparator<in K>,
    ) : Comparator<V> {
        override fun compare(left: V, right: V): Int =
            delegate.compare(function(left), function(right))
    }

    private class ValueListFunction<K, V>(
        private val owner: GroupingListMultiMap<K, V>,
    ) : (List<V>) -> List<V> {
        override fun invoke(sourceValue: List<V>): List<V> = ValueList(sourceValue as MutableList<V>, owner)
    }

    @Suppress("PLATFORM_CLASS_MAPPED_TO_KOTLIN")
    private class ValueList<K, V> private constructor(
        private val delegateRef: DelegateRef<V>,
        private val owner: GroupingListMultiMap<K, V>,
        private val delegateView: (MutableList<V>) -> MutableList<V>,
    ) : java.util.AbstractList<V>() {
        constructor(delegate: MutableList<V>, owner: GroupingListMultiMap<K, V>) :
                this(DelegateRef(delegate), owner, { it })

        private val delegate: MutableList<V>
            get() = delegateView(delegateRef.value)

        private val key: K = owner.key(delegate)

        fun rebind(other: ValueList<K, V>) {
            delegateRef.value = other.delegate
        }

        @get:JvmName("size")
        override val size: Int
            get() = delegate.size

        override fun isEmpty(): Boolean = delegate.isEmpty()

        override fun contains(element: V): Boolean = delegate.contains(element)

        override fun iterator(): MutableIterator<V> = delegate.iterator()

        override fun toArray(): Array<Any?> = delegate.toTypedArray()

        @Suppress("UNCHECKED_CAST")
        override fun <T> toArray(array: Array<T>): Array<T> {
            val delegateSize = delegate.size
            val result =
                if (array.size >= delegateSize) {
                    array
                } else {
                    ReflectArray.newInstance(array.javaClass.componentType, delegateSize) as Array<T>
                }
            for (index in 0..<delegateSize) {
                result[index] = delegate[index] as T
            }
            if (result.size > delegateSize) {
                result[delegateSize] = null as T
            }
            return result
        }

        override fun add(element: V): Boolean {
            owner.checkKeyValueAgreement(key, element)
            return delegate.add(element)
        }

        override fun addAll(elements: Collection<V>): Boolean {
            owner.checkKeyValueAgreement(key, elements)
            return delegate.addAll(elements)
        }

        override fun addAll(index: Int, elements: Collection<V>): Boolean {
            owner.checkKeyValueAgreement(key, elements)
            return delegate.addAll(index, elements)
        }

        override fun add(index: Int, element: V) {
            owner.checkKeyValueAgreement(key, element)
            delegate.add(index, element)
        }

        override fun set(index: Int, element: V): V {
            owner.checkKeyValueAgreement(key, element)
            return delegate.set(index, element)
        }

        override fun subList(fromIndex: Int, toIndex: Int): MutableList<V> {
            val parentView = delegateView
            return ValueList(delegateRef, owner) { root ->
                parentView(root).subList(fromIndex, toIndex)
            }
        }

        override fun listIterator(): MutableListIterator<V> = ValueListIterator(delegate.listIterator(), key, owner)

        override fun listIterator(index: Int): MutableListIterator<V> =
            ValueListIterator(delegate.listIterator(index), key, owner)

        override fun remove(element: V): Boolean = delegate.remove(element)

        override fun containsAll(elements: Collection<V>): Boolean = HashSet(delegate).containsAll(elements)

        override fun removeAll(elements: Collection<V>): Boolean = delegate.removeAll(elements)

        override fun retainAll(elements: Collection<V>): Boolean = delegate.retainAll(elements)

        override fun clear() {
            delegate.clear()
        }

        override fun equals(other: Any?): Boolean = delegate == other

        override fun hashCode(): Int = delegate.hashCode()

        override fun get(index: Int): V = delegate[index]

        override fun removeAt(index: Int): V = delegate.removeAt(index)

        override fun indexOf(element: V): Int = delegate.indexOf(element)

        override fun lastIndexOf(element: V): Int = delegate.lastIndexOf(element)

        override fun toString(): String = delegate.toString()
    }

    private class DelegateRef<V>(var value: MutableList<V>)

    private class ValueListIterator<K, V>(
        private val delegate: MutableListIterator<V>,
        private val key: K,
        private val owner: GroupingListMultiMap<K, V>,
    ) : MutableListIterator<V> {
        override fun set(element: V) {
            owner.checkKeyValueAgreement(key, element)
            delegate.set(element)
        }

        override fun add(element: V) {
            owner.checkKeyValueAgreement(key, element)
            delegate.add(element)
        }

        override fun hasNext(): Boolean = delegate.hasNext()

        override fun next(): V = delegate.next()

        override fun hasPrevious(): Boolean = delegate.hasPrevious()

        override fun previous(): V = delegate.previous()

        override fun nextIndex(): Int = delegate.nextIndex()

        override fun remove() {
            delegate.remove()
        }

        override fun previousIndex(): Int = delegate.previousIndex()
    }
}
