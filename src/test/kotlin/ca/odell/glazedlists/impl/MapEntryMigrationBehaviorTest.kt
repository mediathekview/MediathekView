package ca.odell.glazedlists.impl

import ca.odell.glazedlists.BasicEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*

internal class MapEntryMigrationBehaviorTest {
    @Test
    fun functionListMapEntriesKeepStandardMapEntryEquality() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "bb")) }
        val map = FunctionListMap(source, String::length)
        val entry = map.entries.first { it.key == 1 }
        val equalEntry = AbstractMap.SimpleEntry(1, "a")
        val missingNullEntry = AbstractMap.SimpleEntry<Int, String>(99, null)

        assertTrue(entry == equalEntry)
        assertEquals(equalEntry.hashCode(), entry.hashCode())
        assertFalse(entry.equals("not an entry"))
        assertTrue(map.entries.contains(equalEntry))
        assertFalse(map.entries.contains(AbstractMap.SimpleEntry(1, "wrong")))
        assertFalse(map.entries.contains(missingNullEntry))
        assertFalse(map.entries.remove(missingNullEntry))
        assertTrue(map.entries.remove(equalEntry))
        assertEquals(listOf("bb"), source)
    }

    @Test
    fun groupingMultiMapEntriesKeepStandardMapEntryEquality() {
        val source = BasicEventList<String>().apply { addAll(listOf("a1", "a2", "b1")) }
        val map = GroupingListMultiMap(
            source,
            { it.substring(0, 1) },
            Comparator.naturalOrder<String>(),
        )
        val entry = map.entries.first { it.key == "a" }
        val expected = AbstractMap.SimpleEntry("a", listOf("a1", "a2"))
        val missingNullEntry = AbstractMap.SimpleEntry<String, List<String>>("missing", null)

        assertTrue(entry == expected)
        assertEquals(expected.hashCode(), entry.hashCode())
        assertFalse(entry.equals("not an entry"))
        assertTrue(map.entries.contains(expected))
        assertFalse(map.entries.contains(AbstractMap.SimpleEntry("a", listOf("wrong"))))
        assertFalse(map.entries.contains(missingNullEntry))
        assertFalse(map.entries.remove(missingNullEntry))
        assertTrue(map.entries.remove(expected))
        assertEquals(listOf("b1"), source)
    }
}
