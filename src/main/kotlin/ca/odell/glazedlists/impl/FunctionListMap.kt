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
package ca.odell.glazedlists.impl

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.DisposableMap
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener
import java.util.concurrent.atomic.AtomicBoolean
import java.util.function.BiConsumer

/** A mutable map kept in sync with an [EventList] whose values produce unique keys. */
@Suppress("INAPPLICABLE_JVM_NAME", "UNCHECKED_CAST")
internal class FunctionListMap<K, V> : DisposableMap<K, V> {
    private val keyList: MutableList<K>
    private var cachedKeySet: KeySet<K, V>? = null
    private val valueList: EventList<V>
    private var cachedEntrySet: MutableSet<MutableMap.MutableEntry<K, V>>? = null
    private val keyFunction: (V) -> K
    private val delegate: MutableMap<K, V>
    private val eventListener: ListEventListener<V>

    constructor(source: EventList<V>, keyFunction: (V) -> K) {
        valueList = source
        eventListener = ListEventListener(::processListChanges)
        valueList.addListEventListener(eventListener)
        this.keyFunction = keyFunction

        keyList = BasicEventList(source.size)
        delegate = HashMap(source.size)
        for (index in source.indices) {
            elementAdded(index)
        }
    }

    override fun dispose() {
        valueList.removeListEventListener(eventListener)

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

    override fun containsValue(value: V): Boolean = delegate.containsValue(value)

    override fun get(key: K): V? = delegate[key]

    override fun put(key: K, value: V): V? {
        checkKeyValueAgreement(key, value)
        return putNoAgreementCheck(key, value)
    }

    private fun putNoAgreementCheck(key: K, value: V): V? {
        if (!containsKey(key)) {
            valueList.add(value)
            return null
        }

        val toReplace = get(key) as V
        if (!replaceValue(toReplace, value)) {
            throw IllegalStateException(
                "Found key: $key in delegate map but could not find corresponding value in valueList: $toReplace",
            )
        }
        return toReplace
    }

    private fun replaceValue(replaceInstance: V, newValue: V): Boolean {
        if (valueList is RandomAccess) {
            for (index in valueList.size - 1 downTo 0) {
                if (valueList[index] === replaceInstance) {
                    valueList[index] = newValue
                    return true
                }
            }
            return false
        }

        val foundMatch = AtomicBoolean(false)
        valueList.replaceAll { value ->
            if (foundMatch.get()) {
                value
            } else if (value === replaceInstance) {
                foundMatch.set(true)
                newValue
            } else {
                value
            }
        }
        return foundMatch.get()
    }

    override fun putAll(from: Map<out K, V>) {
        from.forEach(BiConsumer(::checkKeyValueAgreement))
        from.forEach(BiConsumer(::putNoAgreementCheck))
    }

    private fun checkKeyValueAgreement(key: K, value: V) {
        val calculatedKey = key(value)
        require(key == calculatedKey) {
            "The calculated key for the given value ($calculatedKey) does not match the given key ($key)"
        }
    }

    override fun clear() {
        valueList.clear()
    }

    override fun remove(key: K): V? {
        if (!containsKey(key)) return null

        val value = get(key) as V
        valueList.removeIf { candidate -> candidate === value }
        return value
    }

    @get:JvmName("values")
    override val values: MutableCollection<V>
        get() = valueList

    @get:JvmName("keySet")
    override val keys: MutableSet<K>
        get() {
            var current = cachedKeySet
            if (current == null) {
                current = KeySet(keyList, valueList, this)
                cachedKeySet = current
            }
            return current
        }

    @get:JvmName("entrySet")
    override val entries: MutableSet<MutableMap.MutableEntry<K, V>>
        get() {
            var current = cachedEntrySet
            if (current == null) {
                current = EntrySet(
                    keyList,
                    valueList,
                    delegate,
                    keyFunction,
                    this
                ) as MutableSet<MutableMap.MutableEntry<K, V>>
                cachedEntrySet = current
            }
            return current
        }

    override fun forEach(action: BiConsumer<in K, in V>) {
        delegate.forEach(action)
    }

    override fun equals(other: Any?): Boolean = delegate == other

    override fun hashCode(): Int = delegate.hashCode()

    @JvmSynthetic
    internal fun processListChanges(listChanges: ListEvent<V>) {
        var offset = 0

        while (listChanges.next()) {
            when (listChanges.type) {
                ListEvent.DELETE -> elementRemoved(listChanges.index + offset)
                ListEvent.UPDATE -> {
                    elementRemoved(listChanges.index + offset)
                    offset--
                }

                ListEvent.INSERT -> offset--
            }
        }

        listChanges.reset()
        while (listChanges.next()) {
            when (listChanges.type) {
                ListEvent.UPDATE, ListEvent.INSERT -> elementAdded(listChanges.index)
            }
        }
    }

    private fun elementAdded(index: Int) {
        val value = valueList[index]
        val key = key(value)
        keyList.add(index, key)
        putInDelegate(key, value)
    }

    private fun elementRemoved(index: Int) {
        val key = keyList.removeAt(index)
        delegate.remove(key)
    }

    private fun putInDelegate(key: K, value: V) {
        if (delegate.containsKey(key)) {
            throw IllegalStateException(
                "Detected duplicate key->value mapping: attempted to put '$key' -> '$value' in the map, " +
                        "but found '$key' -> '${delegate[key]}' already existed.",
            )
        }
        delegate[key] = value
    }

    private fun key(value: V): K = keyFunction(value)

    @Suppress("PLATFORM_CLASS_MAPPED_TO_KOTLIN")
    private class EntrySet<K, V>(
        private val keyList: MutableList<K>,
        private val valueList: EventList<V>,
        private val delegate: Map<K, V>,
        private val keyFunction: (V) -> K,
        private val owner: FunctionListMap<K, V>,
    ) : java.util.AbstractSet<Map.Entry<K, V>>() {
        override val size: Int
            get() = keyList.size

        override fun iterator(): MutableIterator<Map.Entry<K, V>> =
            EntrySetIterator(keyList.listIterator(), valueList, keyFunction, owner)

        override fun contains(element: Map.Entry<K, V>): Boolean =
            delegate.entries.contains(element)

        override fun remove(element: Map.Entry<K, V>): Boolean {
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
        private val valueList: EventList<V>,
        private val keyFunction: (V) -> K,
        private val owner: FunctionListMap<K, V>,
    ) : MutableIterator<Map.Entry<K, V>> {
        override fun hasNext(): Boolean = keyIterator.hasNext()

        override fun next(): Map.Entry<K, V> {
            val key = keyIterator.next()
            return MapEntry(key, owner[key] as V, keyFunction, owner)
        }

        override fun remove() {
            val index = keyIterator.previousIndex()
            if (index == -1) {
                throw IllegalStateException("Cannot remove() without a prior call to next()")
            }
            valueList.removeAt(index)
        }
    }

    private class MapEntry<K, V>(
        override val key: K,
        initialValue: V,
        private val keyFunction: (V) -> K,
        private val owner: FunctionListMap<K, V>,
    ) : MutableMap.MutableEntry<K, V> {
        private var snapshotValue: V = initialValue

        override val value: V
            get() = snapshotValue

        override fun setValue(newValue: V): V {
            val calculatedKey = keyFunction(newValue)
            require(key == calculatedKey) {
                "The calculated key for the given value ($calculatedKey) does not match the given key ($key)"
            }
            val oldValue = owner.put(key, newValue)
            snapshotValue = newValue
            return oldValue as V
        }

        override fun equals(other: Any?): Boolean =
            other is Map.Entry<*, *> &&
                    key == other.key &&
                    value == other.value

        override fun hashCode(): Int = key.hashCode() xor snapshotValue.hashCode()

        override fun toString(): String = "$key=$value"
    }

    private class KeySet<K, V>(
        private val keyList: MutableList<K>,
        private val valueList: EventList<V>,
        private val owner: FunctionListMap<K, V>,
    ) : AbstractMutableSet<K>() {
        override val size: Int
            get() = keyList.size

        override fun add(element: K): Boolean = throw UnsupportedOperationException()

        override fun iterator(): MutableIterator<K> = KeySetIterator(keyList.listIterator(), valueList)

        override fun contains(element: K): Boolean = owner.containsKey(element)

        override fun remove(element: K): Boolean {
            if (!owner.containsKey(element)) return false
            owner.remove(element)
            return true
        }

        override fun clear() {
            owner.clear()
        }
    }

    private class KeySetIterator<K, V>(
        private val keyIterator: MutableListIterator<K>,
        private val valueList: EventList<V>,
    ) : MutableIterator<K> {
        override fun hasNext(): Boolean = keyIterator.hasNext()

        override fun next(): K = keyIterator.next()

        override fun remove() {
            val index = keyIterator.previousIndex()
            if (index == -1) {
                throw IllegalStateException("Cannot remove() without a prior call to next()")
            }
            valueList.removeAt(index)
        }
    }
}
