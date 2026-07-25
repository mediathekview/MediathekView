package ca.odell.glazedlists.event

import ca.odell.glazedlists.BasicEventList
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class Tree4DeltasListEventBehaviorTest {
    @Test
    fun uninitializedEventRetainsCopyAndRawFailureModes() {
        val pending = pendingEvent()
        val copy = pending.event.copy()

        assertEquals("ca.odell.glazedlists.event.Tree4DeltasListEvent", copy.javaClass.name)
        assertSame(pending.source, pending.event.sourceList)
        assertSame(pending.source, copy.sourceList)
        assertEquals("ListEvent: ", pending.event.toString())
        assertEquals("ListEvent: ", copy.toString())

        assertThrows(NullPointerException::class.java) { pending.event.next() }
        assertThrows(NullPointerException::class.java) { pending.event.hasNext() }
        assertThrows(NullPointerException::class.java) { pending.event.nextBlock() }
        assertThrows(NullPointerException::class.java) { pending.event.index }
        assertThrows(NullPointerException::class.java) { pending.event.type }
        assertThrows(NullPointerException::class.java) { pending.event.oldValue }
        assertThrows(NullPointerException::class.java) { pending.event.newValue }
        assertThrows(NullPointerException::class.java) { pending.event.blocksRemaining }

        assertThrows(NullPointerException::class.java) { copy.next() }
        assertThrows(NullPointerException::class.java) { copy.blocksRemaining }
    }

    @Test
    fun linearModeRetainsAccessorFailuresIterationAndResetBehavior() {
        val inserted = Any()
        val pending = pendingEvent {
            it.elementInserted(0, inserted)
        }
        val event = pending.event

        event.reset()

        assertTrue(event.hasNext())
        assertEquals(1, event.blocksRemaining)
        assertThrows(IllegalStateException::class.java) { event.index }
        assertThrows(IllegalStateException::class.java) { event.blockStartIndex }
        assertThrows(IllegalStateException::class.java) { event.blockEndIndex }
        assertThrows(IllegalStateException::class.java) { event.type }
        assertThrows(IndexOutOfBoundsException::class.java) { event.oldValue }
        assertThrows(IndexOutOfBoundsException::class.java) { event.newValue }

        assertTrue(event.next())
        assertEquals(ListEvent.INSERT, event.type)
        assertEquals(0, event.index)
        assertEquals(0, event.blockStartIndex)
        assertEquals(0, event.blockEndIndex)
        assertSame(ListEvent.UNKNOWN_VALUE, event.oldValue)
        assertSame(inserted, event.newValue)
        assertEquals(0, event.blocksRemaining)
        assertEquals(0, event.index)

        assertFalse(event.next())
        assertEquals(ListEvent.INSERT, event.type)
        assertEquals(0, event.index)
        assertEquals(0, event.blockStartIndex)
        assertEquals(0, event.blockEndIndex)
        assertSame(ListEvent.UNKNOWN_VALUE, event.oldValue)
        assertSame(inserted, event.newValue)

        event.reset()
        assertTrue(event.next())
        assertEquals(0, event.index)
    }

    @Test
    fun linearModeUsesIdentityBasedBlockCoalescingAndInterleaving() {
        val repeated = Any()
        val equalButDistinctA = String(charArrayOf('x'))
        val equalButDistinctB = String(charArrayOf('x'))
        val event = pendingEvent {
            it.elementInserted(0, repeated)
            it.elementInserted(1, repeated)
            it.elementInserted(2, equalButDistinctA)
            it.elementInserted(3, equalButDistinctB)
        }.event

        event.reset()

        assertEquals(3, event.blocksRemaining)
        assertTrue(event.next())
        assertEquals(0, event.index)
        assertEquals(0, event.blockStartIndex)
        assertEquals(1, event.blockEndIndex)
        assertSame(repeated, event.newValue)
        assertEquals(2, event.blocksRemaining)

        assertTrue(event.nextBlock())
        assertEquals(2, event.index)
        assertEquals(2, event.blockStartIndex)
        assertEquals(2, event.blockEndIndex)
        assertSame(equalButDistinctA, event.newValue)
        assertEquals(1, event.blocksRemaining)

        assertTrue(event.next())
        assertEquals(3, event.index)
        assertSame(equalButDistinctB, event.newValue)
        assertEquals(0, event.blocksRemaining)
        assertFalse(event.next())
    }

    @Test
    fun linearModeKeepsDeleteIndexAtBlockStartAndSeparatesValuefulUpdates() {
        val deleted = Any()
        val updateValue = Any()
        val event = pendingEvent {
            it.elementDeleted(2, deleted)
            it.elementDeleted(2, deleted)
            it.elementUpdated(4, updateValue, updateValue)
            it.elementUpdated(5, updateValue, updateValue)
        }.event

        event.reset()

        assertTrue(event.next())
        assertEquals(ListEvent.DELETE, event.type)
        assertEquals(2, event.index)
        assertEquals(2, event.blockStartIndex)
        assertEquals(3, event.blockEndIndex)
        assertSame(deleted, event.oldValue)
        assertSame(ListEvent.UNKNOWN_VALUE, event.newValue)

        assertTrue(event.next())
        assertEquals(ListEvent.DELETE, event.type)
        assertEquals(2, event.index)
        assertEquals(2, event.blockStartIndex)
        assertEquals(3, event.blockEndIndex)

        assertTrue(event.nextBlock())
        assertEquals(ListEvent.UPDATE, event.type)
        assertEquals(4, event.index)
        assertEquals(4, event.blockStartIndex)
        assertEquals(4, event.blockEndIndex)
        assertSame(updateValue, event.oldValue)
        assertSame(updateValue, event.newValue)

        assertTrue(event.nextBlock())
        assertEquals(5, event.index)
        assertEquals(5, event.blockStartIndex)
        assertEquals(5, event.blockEndIndex)
        assertFalse(event.nextBlock())
    }

    @Test
    fun treeModeRetainsBeforeMoveFailuresAndInclusiveBounds() {
        val repeated = Any()
        val event = pendingEvent(sourceSize = 2) {
            it.elementUpdated(1, repeated, repeated)
            it.elementUpdated(0, repeated, repeated)
        }.event

        event.reset()

        assertEquals(1, event.blocksRemaining)
        assertThrows(NoSuchElementException::class.java) { event.index }
        assertThrows(NoSuchElementException::class.java) { event.blockStartIndex }
        assertThrows(NoSuchElementException::class.java) { event.blockEndIndex }
        assertThrows(IllegalStateException::class.java) { event.type }
        assertThrows(IllegalStateException::class.java) { event.oldValue }
        assertThrows(IllegalStateException::class.java) { event.newValue }

        assertTrue(event.nextBlock())
        assertEquals(ListEvent.UPDATE, event.type)
        assertEquals(0, event.index)
        assertEquals(0, event.blockStartIndex)
        assertEquals(1, event.blockEndIndex)
        assertSame(repeated, event.oldValue)
        assertSame(repeated, event.newValue)
        assertEquals(0, event.blocksRemaining)
    }

    @Test
    fun treeModeCoalescesRepeatedIdentitySeparatesDistinctEqualsAndPreservesCursorState() {
        val repeated = Any()
        val equalButDistinctOld = String(charArrayOf('y'))
        val equalButDistinctNew = String(charArrayOf('y'))
        val event = pendingEvent(sourceSize = 3) {
            it.elementUpdated(2, repeated, repeated)
            it.elementUpdated(0, repeated, repeated)
            it.elementUpdated(1, repeated, repeated)
            it.elementUpdated(2, equalButDistinctOld, equalButDistinctNew)
        }.event

        event.reset()

        assertEquals(2, event.blocksRemaining)
        assertTrue(event.nextBlock())
        assertEquals(0, event.index)
        assertEquals(0, event.blockStartIndex)
        assertEquals(1, event.blockEndIndex)
        assertSame(repeated, event.oldValue)
        assertSame(repeated, event.newValue)
        assertEquals(1, event.blocksRemaining)
        assertEquals(0, event.index)

        assertTrue(event.next())
        assertEquals(1, event.index)
        assertEquals(1, event.blockStartIndex)
        assertEquals(1, event.blockEndIndex)
        assertEquals(1, event.blocksRemaining)
        assertEquals(1, event.index)

        assertTrue(event.next())
        assertEquals(2, event.index)
        assertEquals(2, event.blockStartIndex)
        assertEquals(2, event.blockEndIndex)
        assertSame(repeated, event.oldValue)
        assertSame(equalButDistinctNew, event.newValue)
        assertEquals(0, event.blocksRemaining)
        assertFalse(event.next())

        assertEquals(2, event.index)
        assertEquals(2, event.blockStartIndex)
        assertEquals(2, event.blockEndIndex)
        assertSame(repeated, event.oldValue)
        assertSame(equalButDistinctNew, event.newValue)
    }

    @Test
    fun copiesRetainUninitializedCursorStateAndIndependentPositionsInBothModes() {
        val linearRepeated = Any()
        val linearEvent = pendingEvent {
            it.elementInserted(0, linearRepeated)
            it.elementInserted(1, linearRepeated)
            it.elementInserted(2, Any())
        }.event
        val linearCopyBeforeReset = linearEvent.copy()

        assertThrows(NullPointerException::class.java) { linearCopyBeforeReset.next() }

        linearEvent.reset()
        val linearCopyBeforeFirst = linearEvent.copy()
        assertTrue(linearEvent.hasNext())
        assertTrue(linearCopyBeforeFirst.hasNext())
        assertEquals(linearEvent.blocksRemaining, linearCopyBeforeFirst.blocksRemaining)
        assertTrue(linearCopyBeforeFirst.next())
        assertEquals(0, linearCopyBeforeFirst.index)
        assertEquals(1, linearCopyBeforeFirst.blocksRemaining)
        assertEquals(2, linearEvent.blocksRemaining)

        assertTrue(linearEvent.next())
        assertEquals(0, linearEvent.index)
        val linearCopyAtCursor = linearEvent.copy()
        assertTrue(linearEvent.next())
        assertEquals(1, linearEvent.index)
        assertEquals(0, linearCopyAtCursor.index)
        assertTrue(linearCopyAtCursor.next())
        assertEquals(1, linearCopyAtCursor.index)

        while (linearEvent.next()) {
            // exhaust
        }
        val linearCopyAfterExhaustion = linearEvent.copy()
        assertFalse(linearCopyAfterExhaustion.next())
        assertEquals(2, linearCopyAfterExhaustion.index)

        val treeRepeated = Any()
        val treeEvent = pendingEvent(sourceSize = 2) {
            it.elementUpdated(1, treeRepeated, treeRepeated)
            it.elementUpdated(0, treeRepeated, treeRepeated)
        }.event
        val treeCopyBeforeReset = treeEvent.copy()

        assertThrows(NullPointerException::class.java) { treeCopyBeforeReset.nextBlock() }

        treeEvent.reset()
        val treeCopyBeforeFirst = treeEvent.copy()
        assertTrue(treeEvent.hasNext())
        assertTrue(treeCopyBeforeFirst.hasNext())
        assertEquals(treeEvent.blocksRemaining, treeCopyBeforeFirst.blocksRemaining)
        assertTrue(treeCopyBeforeFirst.nextBlock())
        assertEquals(0, treeCopyBeforeFirst.index)
        assertEquals(0, treeCopyBeforeFirst.blocksRemaining)
        assertEquals(1, treeEvent.blocksRemaining)

        assertTrue(treeEvent.nextBlock())
        val treeCopyAtCursor = treeEvent.copy()
        assertTrue(treeEvent.next())
        assertEquals(1, treeEvent.index)
        assertEquals(0, treeCopyAtCursor.index)
        assertTrue(treeCopyAtCursor.next())
        assertEquals(1, treeCopyAtCursor.index)

        assertFalse(treeEvent.next())
        val treeCopyAfterExhaustion = treeEvent.copy()
        assertFalse(treeCopyAfterExhaustion.next())
        assertEquals(1, treeCopyAfterExhaustion.index)
        assertSame(treeEvent.sourceList, treeCopyAfterExhaustion.sourceList)
    }

    @Test
    fun reorderEventsExposeTheOriginalArrayAndRetainTheLegacyNonReorderMessage() {
        val reorderMap = intArrayOf(2, 1, 0)
        val reorderEvent = pendingEvent(sourceSize = 3) {
            it.reorder(reorderMap)
        }.event

        reorderEvent.reset()

        assertTrue(reorderEvent.isReordering)
        assertSame(reorderMap, reorderEvent.reorderMap)
        reorderEvent.reorderMap[0] = 9
        assertEquals(9, reorderMap[0])

        reorderEvent.nextBlock()
        reorderEvent.toString()
        reorderEvent.blocksRemaining
        reorderEvent.reset()
        assertSame(reorderMap, reorderEvent.reorderMap)

        val nonReorderEvent = pendingEvent {
            it.elementInserted(0, Any())
        }.event
        nonReorderEvent.reset()

        val failure = assertThrows(IllegalStateException::class.java) {
            nonReorderEvent.reorderMap
        }
        assertEquals("Cannot get reorder map for a non-reordering change", failure.message)
    }

    private fun pendingEvent(
        sourceSize: Int = 0,
        addChanges: (ListEventAssembler<Any>) -> Unit = {},
    ): PendingEvent {
        val source = BasicEventList<Any>()
        repeat(sourceSize) { source += Any() }
        val assembler = ListEventAssembler(source, source.publisher)
        assembler.beginEvent()
        addChanges(assembler)
        return PendingEvent(source, assembler, createTree4DeltasListEvent(assembler, source))
    }

    private data class PendingEvent(
        val source: BasicEventList<Any>,
        val assembler: ListEventAssembler<Any>,
        val event: ListEvent<Any>,
    )
}
