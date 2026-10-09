package mediathek.tool

import org.junit.jupiter.api.AfterEach
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.io.File

internal class ReplaceListTest {
    @AfterEach
    fun tearDown() {
        ReplaceList.clear()
    }

    @Test
    fun replaceAppliesEntriesInOrder() {
        ReplaceList.add("a", "b")
        ReplaceList.add("b", "c")

        assertEquals("cc", ReplaceList.replace("aa", false))
    }

    @Test
    fun replaceRemovesEmptySearchEntries() {
        ReplaceList.add("", "_")
        ReplaceList.add("a", "b")

        assertEquals("b", ReplaceList.replace("a", false))
        assertEquals(listOf(ReplaceEntry("a", "b")), ReplaceList.entries())
    }

    @Test
    fun replaceSkipsFileSeparatorForPaths() {
        ReplaceList.add(File.separator, "_")

        assertEquals(File.separator, ReplaceList.replace(File.separator, true))
        assertEquals("_", ReplaceList.replace(File.separator, false))
    }

    @Test
    fun entriesReturnsSnapshotCopies() {
        ReplaceList.add("from", "to")

        val snapshotEntry = ReplaceList.entries().single()
        snapshotEntry.from = "changed"

        assertEquals(ReplaceEntry("from", "to"), ReplaceList.entries().single())
    }

    @Test
    fun checkDetectsReplacementLoopRisk() {
        ReplaceList.add("a", "b")
        ReplaceList.add("b", "c")

        assertTrue(ReplaceList.check())
    }

    @Test
    fun checkReturnsFalseWhenNoEntryFeedsLaterSearchTerm() {
        ReplaceList.add("a", "b")
        ReplaceList.add("x", "y")

        assertFalse(ReplaceList.check())
    }
}
