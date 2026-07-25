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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.synchronizeToMultiMap
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*

internal class GroupingListMultiMapBehaviorTest {
    @Test
    fun constructorKeyFailureLeavesTheJavaPartialListenerGraphAttached() {
        val source = BasicEventList<String>().apply { add("boom") }
        val calls = mutableListOf<String>()
        val function: (String) -> String = { value ->
            calls += value
            if (value == "boom") throw IllegalStateException("initial key failure")
            value.substring(0, 1)
        }

        assertEquals(
            "initial key failure",
            assertThrows(IllegalStateException::class.java) {
                GroupingListMultiMap(source, function, Comparator.naturalOrder())
            }.message,
        )
        assertEquals(listOf("boom"), calls)

        assertEquals(
            "initial key failure",
            assertThrows(IllegalStateException::class.java) { source.add("after") }.message,
        )
        assertEquals(listOf("boom", "after", "boom"), calls)
    }

    @Test
    fun constructionGroupsByComparatorOrderWithoutReorderingTheOriginalSource() {
        val source = BasicEventList<String>().apply {
            addAll(listOf("plum", "cherry", "pineapple", "banana", "cranberry", "prune"))
        }
        val keyFunction: (String) -> Char = String::first
        val comparatorCalls = mutableListOf<Pair<Char, Char>>()
        val comparator = Comparator<Char> { left, right ->
            comparatorCalls += left to right
            left.compareTo(right)
        }
        val map = GroupingListMultiMap(source, keyFunction, comparator)

        assertEquals(listOf("plum", "cherry", "pineapple", "banana", "cranberry", "prune"), source)
        assertEquals(listOf('b', 'c', 'p'), map.keys.toList())
        assertEquals(listOf("banana"), map['b'])
        assertEquals(listOf("cherry", "cranberry"), map['c'])
        assertEquals(listOf("plum", "pineapple", "prune"), map['p'])
        assertTrue(comparatorCalls.isNotEmpty())
        assertEquals(mapOf('b' to listOf("banana"), 'c' to listOf("cherry", "cranberry"), 'p' to listOf("plum", "pineapple", "prune")), map)
        assertEquals(map.hashCode(), mapOf('b' to listOf("banana"), 'c' to listOf("cherry", "cranberry"), 'p' to listOf("plum", "pineapple", "prune")).hashCode())
    }

    @Test
    fun sourceInsertDeleteAndKeyChangesKeepLiveGroupsSynchronized() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = newMap(source)
        val originalA = map["a"]!!
        val originalB = map["b"]!!

        source.add(1, "a0")
        assertSame(originalA, map["a"])
        assertEquals(listOf("a1", "a0", "a2"), originalA)

        source[2] = "c1"
        assertEquals(listOf("a1", "a0"), map["a"])
        assertSame(originalB, map["b"])
        assertEquals(listOf("c1"), map["c"])
        assertEquals(listOf("a", "b", "c"), map.keys.toList())

        source.remove("b1")
        assertFalse(map.containsKey("b"))
        assertEquals(listOf("a", "c"), map.keys.toList())
    }

    @Test
    fun sameKeySourceUpdateKeepsKeysMappingsAndLiveGroupsSynchronized() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = newMap(source)
        val originalA = map["a"]!!
        val originalB = map["b"]!!

        source[0] = "a9"

        assertEquals(listOf("a9", "a2", "b1"), source)
        assertEquals(2, map.size)
        assertEquals(listOf("a", "b"), map.keys.toList())
        assertSame(originalA, map["a"])
        assertEquals(listOf("a9", "a2"), originalA)
        assertSame(originalB, map["b"])
        assertEquals(listOf("b1"), originalB)
    }

    @Test
    fun retainedSubListFollowsSameKeyGroupRebinding() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = newMap(source)
        val retainedSubList = map["a"]!!.subList(0, 1)

        source[0] = "a9"

        assertEquals(listOf("a9"), retainedSubList)
        retainedSubList[0] = "a8"
        assertEquals(listOf("a8", "a2", "b1"), source)
        assertEquals(listOf("a8", "a2"), map["a"])
    }

    @Test
    fun comparatorDrivenReorderChangesViewOrderButNotMappings() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "b1", "c1")) }
        val sorted = SortedList(source, null)
        val map = GroupingListMultiMap(sorted, { it.substring(0, 1) }, Comparator.naturalOrder())
        val keys = map.keys
        val entries = map.entries

        sorted.comparator = Comparator.reverseOrder()

        assertEquals(listOf("a", "b", "c"), keys.toList())
        assertEquals(listOf("a", "b", "c"), entries.map { it.key })
        assertEquals(listOf("a1"), map["a"])
        assertEquals(listOf("b1"), map["b"])
        assertEquals(listOf("c1"), map["c"])
    }

    @Test
    fun valuesReturnsTheExactLiveGroupingListRatherThanTheValidatedMapValues() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = newMap(source)
        val values = map.values

        assertSame(values, map.values)
        assertNotSame(values.first(), map["a"])
        assertEquals(listOf(listOf("a1", "a2"), listOf("b1")), values.map { it.toList() })

        (values.first() as MutableList<String>).add("c1")
        assertEquals(listOf("a1", "a2", "c1", "b1"), source)
        assertEquals(listOf("a", "b", "c"), map.keys.toList())
        assertEquals(listOf("c1"), map["c"])
    }

    @Test
    fun validatedValueListsPreserveIdentityAndCheckEveryWriteBeforeMutation() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2")) }
        val map = newMap(source)
        val values = map["a"]!!
        val before = values.toList()

        assertSame(values, map["a"])
        assertEquals("The calculated key for the given value (b) does not match the given key (a)", assertThrows(IllegalArgumentException::class.java) { values.add("b1") }.message)
        assertEquals(before, values)
        assertThrows(IllegalArgumentException::class.java) { values.add(99, "b2") }
        assertEquals(before, values)
        assertThrows(IllegalArgumentException::class.java) { values.addAll(99, listOf("a3", "b3")) }
        assertEquals(before, values)
        assertThrows(IllegalArgumentException::class.java) { values[99] = "b4" }
        assertEquals(before, values)

        values.add(1, "a0")
        assertEquals(listOf("a1", "a0", "a2"), source)
        assertEquals("a0", values.set(1, "a9"))
        assertEquals(listOf("a1", "a9", "a2"), source)
        assertTrue(values.addAll(listOf("a3", "a4")))
        assertEquals(listOf("a1", "a9", "a2", "a3", "a4"), values)
    }

    @Test
    fun valueListIteratorValidatesSetAndAddBeforeDelegateStateChecks() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2")) }
        val values = newMap(source)["a"]!!
        val iterator = values.listIterator()

        assertThrows(IllegalArgumentException::class.java) { iterator.set("b1") }
        assertThrows(IllegalStateException::class.java) { iterator.set("a0") }
        assertThrows(IllegalArgumentException::class.java) { iterator.add("b2") }
        assertEquals("a1", iterator.next())
        iterator.set("a9")
        iterator.add("a8")
        assertEquals(listOf("a9", "a8", "a2"), source)
        assertEquals("a8", iterator.previous())
        iterator.remove()
        assertEquals(listOf("a9", "a2"), source)
    }

    @Test
    fun valueListSubListsDeriveTheirOwnKeyAndEmptySubListsFailDuringConstruction() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "a3")) }
        val values = newMap(source)["a"]!!
        val tail = values.subList(1, 3)

        assertEquals(listOf("a2", "a3"), tail)
        tail.add("a4")
        assertEquals(listOf("a1", "a2", "a3", "a4"), source)
        assertThrows(IllegalArgumentException::class.java) { tail.add("b1") }
        assertNull(assertThrows(NoSuchElementException::class.java) { values.subList(1, 1) }.message)
    }

    @Test
    fun valueListDelegatesReadRemovalBulkArrayEqualityAndStringOperations() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "a2", "a3")) }
        val values = newMap(source)["a"]!!

        assertFalse(values.isEmpty())
        assertTrue(values.contains("a2"))
        assertTrue(values.containsAll(listOf("a1", "a2", "a2")))
        assertEquals(1, values.indexOf("a2"))
        assertEquals(2, values.lastIndexOf("a2"))
        assertArrayEquals(arrayOf("a1", "a2", "a2", "a3"), values.toTypedArray())
        assertEquals(listOf("a1", "a2", "a2", "a3"), values)
        assertEquals(listOf("a1", "a2", "a2", "a3").hashCode(), values.hashCode())
        assertEquals("[a1, a2, a2, a3]", values.toString())

        assertTrue(values.remove("a2"))
        assertEquals("a2", values.removeAt(1))
        assertTrue(values.retainAll(listOf("a1")))
        assertEquals(listOf("a1"), source)
        values.clear()
        assertTrue(source.isEmpty())
    }

    @Test
    fun putPrevalidatesThenRemovesBeforeAddingAndReturnsRemovedCopy() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = newMap(source)
        val oldLiveGroup = map["a"]!!

        val removed = map.put("a", mutableListOf("a9", "a8"))

        assertNotSame(oldLiveGroup, removed)
        assertEquals(listOf("a1", "a2"), removed)
        assertEquals(listOf("b1", "a9", "a8"), source)
        assertEquals(listOf("a9", "a8"), map["a"])
        assertNotSame(oldLiveGroup, map["a"])

        val stable = source.toList()
        assertThrows(IllegalArgumentException::class.java) { map.put("wrong", listOf("c1")) }
        assertEquals(stable, source)

        val emptyRemoved = map.put("a", emptyList())
        assertEquals(listOf("a9", "a8"), emptyRemoved)
        assertFalse(map.containsKey("a"))
    }

    @Test
    fun putAllFullyPrevalidatesThenRemovesAllTargetKeysBeforeAdding() {
        val source = BasicEventList<String>().apply { addAll(listOf("a0", "b0", "c0")) }
        val map = newMap(source)
        val invalid = LinkedHashMap<String, List<String>>().apply {
            put("a", listOf("a1"))
            put("wrong", listOf("b1"))
        }

        assertThrows(IllegalArgumentException::class.java) { map.putAll(invalid) }
        assertEquals(listOf("a0", "b0", "c0"), source)

        val replacement = LinkedHashMap<String, List<String>>().apply {
            put("a", listOf("a1", "a2"))
            put("b", emptyList())
        }
        map.putAll(replacement)
        assertEquals(listOf("c0", "a1", "a2"), source)
        assertEquals(setOf("a", "c"), map.keys)
    }

    @Test
    fun putAllRetainsRemovalAndAdditionPartialStateWhenKeyFunctionFailsLater() {
        var b2Calls = 0
        val source = BasicEventList<String>().apply { addAll(listOf("a0", "b0")) }
        val function: (String) -> String = { value ->
            if (value == "b2" && ++b2Calls == 2) throw IllegalStateException("late key failure")
            value.substring(0, 1)
        }
        val map = GroupingListMultiMap(source, function, Comparator.naturalOrder())
        val replacement = LinkedHashMap<String, List<String>>().apply {
            put("a", listOf("a1"))
            put("b", listOf("b2"))
        }

        val failure = assertThrows(IllegalStateException::class.java) { map.putAll(replacement) }
        assertEquals("late key failure", failure.message)
        assertFalse(source.contains("a0"))
        assertFalse(source.contains("b0"))
        assertTrue(source.contains("a1"))
    }

    @Test
    fun removeClearAndCachedViewsWriteThrough() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1", "c1")) }
        val map = newMap(source)
        val keys = map.keys
        val entries = map.entries

        assertSame(keys, map.keys)
        assertSame(entries, map.entries)
        assertNull(map.remove("missing"))
        assertEquals(listOf("a1", "a2"), map.remove("a"))
        assertEquals(listOf("b1", "c1"), source)
        assertTrue(keys.remove("b"))
        assertEquals(listOf("c1"), source)
        entries.clear()
        assertTrue(source.isEmpty())
        assertTrue(map.isEmpty())
    }

    @Test
    fun keyAndEntryIteratorsHaveExactRemoveStateAndRemainConsistentWithEvents() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "b1", "c1")) }
        val map = newMap(source)
        val keyIterator = map.keys.iterator()
        val entryIterator = map.entries.iterator()

        assertEquals("Cannot remove() without a prior call to next()", assertThrows(IllegalStateException::class.java) { keyIterator.remove() }.message)
        assertEquals("Cannot remove() without a prior call to next()", assertThrows(IllegalStateException::class.java) { entryIterator.remove() }.message)
        assertEquals("a", keyIterator.next())
        source.add("d1")
        keyIterator.remove()
        assertFalse(map.containsKey("a"))
        assertThrows(IllegalStateException::class.java) { keyIterator.remove() }

        assertEquals("b", entryIterator.next().key)
        entryIterator.remove()
        assertFalse(map.containsKey("b"))
        assertThrows(IllegalStateException::class.java) { entryIterator.remove() }
    }

    @Test
    fun entrySetUsesObjectTypedMembershipAndInheritedBulkOperations() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "b1", "c1")) }
        val map = newMap(source)
        @Suppress("UNCHECKED_CAST")
        val entries = map.entries as MutableSet<Map.Entry<String, List<String>>>
        val a = ReadOnlyEntry("a", listOf("a1"))
        val b = ReadOnlyEntry("b", listOf("b1"))
        val c = ReadOnlyEntry("c", listOf("c1"))

        assertTrue(entries.contains(a))
        assertFalse(entries.contains(ReadOnlyEntry("a", listOf("wrong"))))
        assertFalse((entries as Set<Any?>).contains(null))
        assertFalse((entries as Set<Any?>).contains("not an entry"))
        assertTrue(entries.containsAll(listOf(a, b, c)))
        assertTrue(entries.removeAll(listOf(a)))
        assertEquals(listOf("b1", "c1"), source)
        assertTrue(entries.retainAll(listOf(c)))
        assertEquals(listOf("c1"), source)
        val array = entries.toTypedArray()
        assertEquals(1, array.size)
        assertTrue(array[0] == c)
    }

    @Test
    fun entryIteratorReturnsWritableSnapshotEntriesWithLegacyReplacementAlgorithm() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = newMap(source)
        val entry = map.entries.first { it.key == "a" }
        val expected = AbstractMap.SimpleEntry("a", listOf("a1", "a2"))

        assertEquals(expected, entry)
        assertEquals(expected.hashCode(), entry.hashCode())
        assertEquals("a=[a1, a2]", entry.toString())
        assertFalse(entry.equals("not an entry"))

        val old = entry.setValue(listOf("a3", "a4"))
        assertEquals(listOf("a1", "a2"), old)
        assertEquals(listOf("a3", "a4"), entry.value)
        assertSame(map["a"], entry.value)
        assertEquals(listOf("a3", "a4", "b1"), source)

        val stable = source.toList()
        assertThrows(IllegalArgumentException::class.java) { entry.setValue(listOf("b2")) }
        assertEquals(stable, source)
    }

    @Test
    fun entrySetValueRetainsReplacementElementsEqualToOldElements() {
        val source = BasicEventList<EqualValue>().apply {
            addAll(listOf(EqualValue("a", "same", 1), EqualValue("a", "same", 2)))
        }
        val map = GroupingListMultiMap(source, EqualValue::key, Comparator.naturalOrder())
        val entry = map.entries.single()
        val replacement = EqualValue("a", "same", 3)

        val old = entry.setValue(listOf(replacement))

        assertEquals(2, old.size)
        assertEquals(1, source.size)
        assertSame(replacement, source.single())
        assertTrue(map.containsKey("a"))
        assertSame(entry.value, map["a"])
        assertEquals(1, entry.value.size)
        assertSame(replacement, entry.value.single())
    }

    @Test
    fun extensionAndMapDefaultsRetainMapSemantics() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = source.synchronizeToMultiMap { it.substring(0, 1) }
        val seen = linkedMapOf<String, List<String>>()

        map.forEach(seen::put)
        assertEquals(map, seen)
        assertEquals(seen, map)
        assertEquals(seen.hashCode(), map.hashCode())
        assertTrue(map.containsKey("a"))
        assertTrue(map.containsValue(listOf("b1")))
        assertNull(map["missing"])
        assertEquals(listOf("fallback"), map.getOrDefault("missing", listOf("fallback")))
    }

    @Test
    fun disposeDetachesListenersClearsCachesAndLeavesOldViewsEmpty() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "b1")) }
        val map = newMap(source)
        val oldKeys = map.keys
        val oldEntries = map.entries
        val oldValues = map.values
        val oldA = map["a"]!!

        map.dispose()

        assertTrue(map.isEmpty())
        assertTrue(oldKeys.isEmpty())
        assertTrue(oldEntries.isEmpty())
        assertNotSame(oldKeys, map.keys)
        assertNotSame(oldEntries, map.entries)
        assertEquals(listOf("a1", "b1"), source)
        assertEquals(listOf(listOf("a1"), listOf("b1")), oldValues.map { it.toList() })
        assertEquals(listOf("a1"), oldA)

        source.add("c1")
        assertTrue(map.isEmpty())
        assertEquals(listOf(listOf("a1"), listOf("b1")), map.values.map { it.toList() })
    }

    private fun newMap(source: BasicEventList<String>): GroupingListMultiMap<String, String> =
        GroupingListMultiMap(source, { it.substring(0, 1) }, Comparator.naturalOrder())

    private data class ReadOnlyEntry<K, V>(
        override val key: K,
        override val value: V,
    ) : Map.Entry<K, V>

    private class EqualValue(
        val key: String,
        private val equality: String,
        private val identity: Int,
    ) {
        override fun equals(other: Any?): Boolean = other is EqualValue && equality == other.equality
        override fun hashCode(): Int = identity
        override fun toString(): String = "$key:$equality:$identity"
    }
}
