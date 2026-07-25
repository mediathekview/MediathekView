package mediathek.tool

import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class ReplacementRulesTest {
    @Test
    fun replaceAppliesEntriesInOrder() {
        val rules = ReplacementRules()
        rules.add("a", "b")
        rules.add("b", "c")

        assertEquals("cc", rules.replace("aa", false))
    }

    @Test
    fun addIgnoresEmptySearchEntriesImmediately() {
        val rules = ReplacementRules()
        assertFalse(rules.add("", "_"))
        assertTrue(rules.add("a", "b"))

        assertEquals("b", rules.replace("a", false))
        assertEquals(listOf(ReplaceEntry("a", "b")), rules.entries())
    }

    @Test
    fun addValuesIgnoresEmptySearchEntriesImmediately() {
        val rules = ReplacementRules()

        assertFalse(rules.add(arrayOf("", "_")))

        assertTrue(rules.entries().isEmpty())
    }

    @Test
    fun replaceSkipsBothSlashSeparatorsForPaths() {
        val rules = ReplacementRules()
        rules.add("/", "_")
        rules.add("\\", "_")

        assertEquals("a/b\\c", rules.replace("a/b\\c", true))
        assertEquals("a_b_c", rules.replace("a/b\\c", false))
    }

    @Test
    fun entriesReturnsSnapshotCopies() {
        val rules = ReplacementRules()
        rules.add("from", "to")

        val snapshot = rules.entries()
        rules.setFrom(0, "changed")

        assertEquals(listOf(ReplaceEntry("from", "to")), snapshot)
        assertEquals(listOf(ReplaceEntry("changed", "to")), rules.entries())
    }

    @Test
    fun setFromRemovesEntryWhenSearchValueIsEmpty() {
        val rules = ReplacementRules()
        rules.add("from", "to")

        rules.setFrom(0, "")

        assertTrue(rules.entries().isEmpty())
    }

    @Test
    fun checkDetectsReplacementLoopRisk() {
        val rules = ReplacementRules()
        rules.add("a", "b")
        rules.add("b", "c")

        assertTrue(rules.check())
    }

    @Test
    fun checkReturnsFalseWhenNoEntryFeedsLaterSearchTerm() {
        val rules = ReplacementRules()
        rules.add("a", "b")
        rules.add("x", "y")

        assertFalse(rules.check())
    }

    @Test
    fun moveUpMovesEntryTowardStartAndStopsAtBoundary() {
        val rules = ReplacementRules()
        rules.add("first", "1")
        rules.add("second", "2")

        assertEquals(0, rules.moveUp(1))
        assertEquals(0, rules.moveUp(0))
        assertEquals(listOf("second", "first"), rules.entries().map(ReplaceEntry::from))
    }

    @Test
    fun moveDownMovesEntryTowardEndAndStopsAtBoundary() {
        val rules = ReplacementRules()
        rules.add("first", "1")
        rules.add("second", "2")

        assertEquals(1, rules.moveDown(0))
        assertEquals(1, rules.moveDown(1))
        assertEquals(listOf("second", "first"), rules.entries().map(ReplaceEntry::from))
    }
}
