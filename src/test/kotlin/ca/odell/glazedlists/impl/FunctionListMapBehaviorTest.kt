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

import ca.odell.glazedlists.*
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*

internal class FunctionListMapBehaviorTest {
    @Test
    fun constructionAndSourceChangesKeepLookupAndSourceOrderViewsSynchronized() {
        val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val map = FunctionListMap(source) { it.first() }

        assertEquals(2, map.size)
        assertFalse(map.isEmpty())
        assertEquals("alpha", map['a'])
        assertTrue(map.containsKey('b'))
        assertTrue(map.containsValue("beta"))
        assertEquals(listOf('a', 'b'), map.keys.toList())
        assertEquals(listOf('a', 'b'), map.entries.map { it.key })

        source.add(1, "charlie")
        source[0] = "delta"
        source.removeAt(2)

        assertEquals(mapOf('c' to "charlie", 'd' to "delta"), map)
        assertEquals(listOf('d', 'c'), map.keys.toList())
        assertEquals(listOf("delta", "charlie"), map.values.toList())
    }

    @Test
    fun extensionCreatesTheSameLiveMapContract() {
        val source = BasicEventList<String>().apply { add("alpha") }
        val map = source.synchronizeToMap { it.first() }

        source.add("beta")

        assertEquals(mapOf('a' to "alpha", 'b' to "beta"), map)
        map.dispose()
    }

    @Test
    fun duplicateInsertReportsExactDiagnosticAndLeavesTheJavaPartialState() {
        val original = Value("a", "original")
        val duplicate = Value("a", "duplicate")
        val source = BasicEventList<Value>().apply { add(original) }
        val map = FunctionListMap(source, Value::key)

        val failure = assertThrows(IllegalStateException::class.java) { source.add(duplicate) }

        assertEquals(
            "Detected duplicate key->value mapping: attempted to put 'a' -> '$duplicate' in the map, " +
                    "but found 'a' -> '$original' already existed.",
            failure.message,
        )
        assertEquals(listOf(original, duplicate), source)
        assertEquals(1, map.size)
        assertSame(original, map["a"])
        assertEquals(listOf("a", "a"), map.keys.toList())

        source.removeAt(1)
        assertFalse(map.containsKey("a"))
        assertEquals(listOf(original), source)
        assertEquals(listOf("a"), map.keys.toList())
    }

    @Test
    fun duplicateUpdatePerformsRemovalPassBeforeFailingTheAdditionPass() {
        val a = Value("a", "first")
        val b = Value("b", "second")
        val duplicate = Value("a", "replacement")
        val source = BasicEventList<Value>().apply { addAll(listOf(a, b)) }
        val map = FunctionListMap(source, Value::key)

        val failure = assertThrows(IllegalStateException::class.java) { source[1] = duplicate }

        assertEquals(
            "Detected duplicate key->value mapping: attempted to put 'a' -> '$duplicate' in the map, " +
                    "but found 'a' -> '$a' already existed.",
            failure.message,
        )
        assertEquals(listOf(a, duplicate), source)
        assertEquals(mapOf("a" to a), map)
        assertEquals(listOf("a", "a"), map.keys.toList())
    }

    @Test
    fun duplicateConstructionFailsAfterRegisteringTheListenerAndAddingTheDuplicateKey() {
        val calls = mutableListOf<String>()
        val source = BasicEventList<Value>().apply {
            addAll(listOf(Value("a", "first"), Value("a", "second")))
        }
        val keyFunction: (Value) -> String = { value -> value.key.also { calls += value.label } }

        val failure = assertThrows(IllegalStateException::class.java) { FunctionListMap(source, keyFunction) }

        assertTrue(failure.message!!.startsWith("Detected duplicate key->value mapping:"))
        assertEquals(listOf("first", "second"), calls)
        source.add(Value("c", "third"))
        assertEquals(listOf("first", "second", "third"), calls)
    }

    @Test
    fun putValidatesAgreementAndReplacesByIdentityWithoutMovingTheValue() {
        val equalDistractor = IdentityValue("x", "same")
        val original = IdentityValue("a", "same")
        val source = BasicEventList<IdentityValue>().apply { addAll(listOf(equalDistractor, original)) }
        val map = FunctionListMap(source, IdentityValue::key)
        val replacement = IdentityValue("a", "new")

        val previous = map.put("a", replacement)

        assertSame(original, previous)
        assertSame(equalDistractor, source[0])
        assertSame(replacement, source[1])
        assertSame(replacement, map["a"])

        val failure = assertThrows(IllegalArgumentException::class.java) {
            map.put("wrong", IdentityValue("right", "value"))
        }
        assertEquals(
            "The calculated key for the given value (right) does not match the given key (wrong)",
            failure.message,
        )
        assertFalse(map.containsKey("wrong"))
    }

    @Test
    fun nonRandomAccessReplacementUsesIdentityAndPreservesPosition() {
        val first = IdentityValue("a", "first")
        val second = IdentityValue("b", "second")
        val delegate = BasicEventList<IdentityValue>().apply { addAll(listOf(first, second)) }
        val source = NonAbstractEventList(delegate)
        val map = FunctionListMap(source, IdentityValue::key)
        val replacement = IdentityValue("a", "replacement")

        assertSame(first, map.put("a", replacement))

        assertEquals(listOf(replacement, second), delegate)
        assertSame(replacement, map["a"])
    }

    @Test
    fun putAllPrevalidatesAgreementButRetainsSecondPhasePartialFailures() {
        var calls = 0
        val source = BasicEventList<Value>()
        val keyFunction: (Value) -> String = { value ->
            calls++
            if (calls == 4) throw IllegalStateException("fourth key calculation")
            value.key
        }
        val map = FunctionListMap(source, keyFunction)
        val values = LinkedHashMap<String, Value>().apply {
            put("a", Value("a", "first"))
            put("b", Value("b", "second"))
        }

        val failure = assertThrows(IllegalStateException::class.java) { map.putAll(values) }

        assertEquals("fourth key calculation", failure.message)
        assertEquals(4, calls)
        assertEquals(listOf(Value("a", "first"), Value("b", "second")), source)
        assertEquals(mapOf("a" to Value("a", "first")), map)

        val invalid = LinkedHashMap<String, Value>().apply {
            put("c", Value("c", "valid"))
            put("wrong", Value("d", "invalid"))
        }
        val stableSource = BasicEventList<Value>()
        val stableMap = FunctionListMap(stableSource, Value::key)
        assertThrows(IllegalArgumentException::class.java) { stableMap.putAll(invalid) }
        assertTrue(stableSource.isEmpty())
        assertTrue(stableMap.isEmpty())
    }

    @Test
    fun removeUsesDelegateValueIdentityAndClearWritesThrough() {
        val a = IdentityValue("a", "same")
        val b = IdentityValue("b", "same")
        val source = BasicEventList<IdentityValue>().apply { addAll(listOf(a, b)) }
        val map = FunctionListMap(source, IdentityValue::key)

        assertSame(a, map.remove("a"))
        assertEquals(listOf(b), source)
        assertNull(map.remove("missing"))

        map.clear()
        assertTrue(source.isEmpty())
        assertTrue(map.isEmpty())
    }

    @Test
    fun valuesIsTheExactLiveSourceAndCachedViewsKeepIdentity() {
        val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val map = FunctionListMap(source) { it.first() }

        assertSame(source, map.values)
        val keys = map.keys
        val entries = map.entries
        assertSame(keys, map.keys)
        assertSame(entries, map.entries)

        map.values.add("charlie")
        source.remove("alpha")

        assertEquals(listOf('b', 'c'), keys.toList())
        assertEquals(listOf('b', 'c'), entries.map { it.key })
    }

    @Test
    fun keyAndEntryIteratorsWriteThroughAndRetainExactStateErrors() {
        val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val map = FunctionListMap(source) { it.first() }
        val keyIterator = map.keys.iterator()
        val entryIterator = map.entries.iterator()

        assertEquals(
            "Cannot remove() without a prior call to next()",
            assertThrows(IllegalStateException::class.java) { keyIterator.remove() }.message,
        )
        assertEquals(
            "Cannot remove() without a prior call to next()",
            assertThrows(IllegalStateException::class.java) { entryIterator.remove() }.message,
        )

        assertEquals('a', keyIterator.next())
        keyIterator.remove()
        assertEquals(listOf("beta"), source)
        assertEquals(listOf('b'), map.keys.toList())

        val remainingEntry = entryIterator.next()
        assertEquals('b', remainingEntry.key)
        entryIterator.remove()
        assertTrue(source.isEmpty())
        assertTrue(map.isEmpty())
        assertThrows(IllegalStateException::class.java) { entryIterator.remove() }
    }

    @Test
    fun mapEntryIsASnapshotWhoseSetValueWritesThroughAndReturnsPriorValue() {
        val source = BasicEventList<String>().apply { add("alpha") }
        val map = FunctionListMap(source) { it.first() }
        val entry = map.entries.single()
        val equal = AbstractMap.SimpleEntry('a', "alpha")

        assertEquals(equal, entry)
        assertEquals(equal.hashCode(), entry.hashCode())
        assertEquals("a=alpha", entry.toString())
        assertFalse(entry.equals("not an entry"))

        assertEquals("alpha", entry.setValue("amber"))
        assertEquals("amber", entry.value)
        assertEquals(listOf("amber"), source)

        map.remove('a')
        assertEquals("amber", entry.value)
        assertNull(entry.setValue("azure"))
        assertEquals(listOf("azure"), source)

        val failure = assertThrows(IllegalArgumentException::class.java) { entry.setValue("beta") }
        assertEquals(
            "The calculated key for the given value (b) does not match the given key (a)",
            failure.message,
        )
        assertEquals("azure", entry.value)
    }

    @Test
    fun entrySetAcceptsReadOnlyKotlinEntriesForContainsAndRemoval() {
        val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val map = FunctionListMap(source) { it.first() }
        @Suppress("UNCHECKED_CAST")
        val entries = map.entries as MutableSet<Map.Entry<Char, String>>
        val alpha = ReadOnlyEntry('a', "alpha")
        val beta = ReadOnlyEntry('b', "beta")

        assertTrue(entries.containsAll(listOf(alpha, beta)))
        assertTrue(entries.removeAll(setOf(alpha)))
        assertEquals(listOf("beta"), source)
        assertTrue(entries.remove(beta))
        assertTrue(source.isEmpty())
        assertTrue(map.isEmpty())
    }

    @Test
    fun nullKeysAndValuesWorkInEntriesAndKeyRemoval() {
        val source = BasicEventList<String?>().apply { addAll(listOf(null, "alpha")) }
        val map = FunctionListMap(source) { it?.first() }

        assertTrue(map.containsKey(null))
        assertNull(map[null])
        assertEquals("alpha", map['a'])
        assertNull(map.put(null, null))
        assertEquals(listOf(null, "alpha"), source)

        val nullEntry = map.entries.iterator().next()
        assertNull(nullEntry.key)
        assertNull(nullEntry.value)
        assertEquals(0, nullEntry.hashCode())
        assertEquals("null=null", nullEntry.toString())
        assertNull(nullEntry.setValue(null))
        assertNull(nullEntry.value)

        assertTrue(map.keys.remove(null))
        assertEquals(listOf("alpha"), source)
        assertFalse(map.containsKey(null))
    }

    @Test
    fun entrySetValueSupportsReplacingAValueWithNull() {
        val source = BasicEventList<String?>().apply { add("alpha") }
        val map = FunctionListMap(source) { "key" }
        val entry = map.entries.single()

        assertEquals("alpha", entry.setValue(null))
        assertNull(entry.value)
        assertTrue(map.containsKey("key"))
        assertNull(map["key"])
        assertEquals(listOf<String?>(null), source)
        assertEquals("key".hashCode(), entry.hashCode())

        assertTrue(map.keys.remove("key"))
        assertTrue(source.isEmpty())
    }

    @Test
    fun inheritedSetBulkAndArrayOperationsWriteThrough() {
        val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta", "charlie", "delta")) }
        val map = FunctionListMap(source) { it.first() }
        val keys = map.keys

        assertArrayEquals(arrayOf('a', 'b', 'c', 'd'), keys.toTypedArray())
        assertTrue(keys.removeAll(setOf('a', 'c')))
        assertEquals(listOf("beta", "delta"), source)
        assertTrue(keys.retainAll(setOf('d')))
        assertEquals(listOf("delta"), source)

        map.put('e', "echo")
        map.put('f', "foxtrot")
        assertTrue(map.entries.removeIf { it.key == 'e' })
        assertEquals(listOf("delta", "foxtrot"), source)
        assertTrue(map.keys.removeIf { it == 'f' })
        assertEquals(listOf("delta"), source)
    }

    @Test
    fun transactionUsesTwoPassProcessingAndReorderSynchronizesIterationOrder() {
        val transaction = TransactionList(BasicEventList<String>())
        val map = FunctionListMap(transaction) { it.first() }
        transaction.beginEvent()
        transaction.addAll(listOf("beta", "alpha", "charlie"))
        transaction.commitEvent()

        transaction.beginEvent()
        transaction.add(0, "charlie")
        transaction[1] = "bravo"
        transaction.removeAt(3)
        transaction.commitEvent()

        assertEquals(mapOf('a' to "alpha", 'b' to "bravo", 'c' to "charlie"), map)
        assertEquals(listOf("charlie", "bravo", "alpha"), transaction)
        assertEquals(listOf('c', 'b', 'a'), map.keys.toList())

        val unsorted = BasicEventList<String>().apply { addAll(listOf("beta", "alpha", "charlie")) }
        val sorted = SortedList(unsorted, null)
        val reorderedMap = FunctionListMap(sorted) { it.first() }
        sorted.comparator = Comparator.naturalOrder()

        assertEquals(listOf("alpha", "beta", "charlie"), sorted)
        assertEquals(listOf('a', 'b', 'c'), reorderedMap.keys.toList())
        assertEquals(mapOf('a' to "alpha", 'b' to "beta", 'c' to "charlie"), reorderedMap)
    }

    @Test
    fun forEachEqualsHashCodeAndDisposeRetainDelegateSemantics() {
        val source = BasicEventList<String>().apply { addAll(listOf("alpha", "beta")) }
        val map = FunctionListMap(source) { it.first() }
        val seen = mutableMapOf<Char, String>()
        map.forEach(seen::put)

        assertEquals(mapOf('a' to "alpha", 'b' to "beta"), seen)
        assertEquals(seen, map)
        assertEquals(seen.hashCode(), map.hashCode())

        val oldKeys = map.keys
        val oldEntries = map.entries
        map.dispose()

        assertTrue(map.isEmpty())
        assertTrue(oldKeys.isEmpty())
        assertTrue(oldEntries.isEmpty())
        assertNotSame(oldKeys, map.keys)
        assertNotSame(oldEntries, map.entries)
        assertEquals(listOf("alpha", "beta"), source)

        source.add("charlie")
        assertTrue(map.isEmpty())
        assertEquals(listOf("alpha", "beta", "charlie"), map.values.toList())
    }

    private data class Value(val key: String, val label: String)

    private data class ReadOnlyEntry<K, V>(
        override val key: K,
        override val value: V,
    ) : Map.Entry<K, V>

    private class IdentityValue(val key: String, private val label: String) {
        override fun equals(other: Any?): Boolean = other is IdentityValue && label == other.label
        override fun hashCode(): Int = label.hashCode()
        override fun toString(): String = "$key:$label"
    }

    private class NonAbstractEventList<E>(private val delegate: EventList<E>) : EventList<E> by delegate
}
