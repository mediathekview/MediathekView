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
import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.event.SequenceDependenciesEventPublisher
import ca.odell.glazedlists.impl.UpgradeDetectingReadWriteLock
import ca.odell.glazedlists.matchers.Matcher
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.atomic.AtomicInteger

internal class ListSelectionBehaviorTest {
    @Test
    fun constructorsDefaultsAndInsertionPoliciesCoverAllFourModes() {
        val defaultSource = eventListOf("a", "b", "c")
        val defaultSelection = ListSelection(defaultSource)
        assertEquals(ListSelection.MULTIPLE_INTERVAL_SELECTION_DEFENSIVE, defaultSelection.selectionMode)
        assertSame(defaultSource, defaultSelection.source)
        assertEquals(emptyList<String>(), defaultSelection.selected)
        assertEquals(listOf("a", "b", "c"), defaultSelection.deselected)

        val initialized = ListSelection(eventListOf("a", "b", "c", "d"), intArrayOf(1, 3))
        assertEquals(listOf("b", "d"), initialized.selected)
        assertEquals(-1, initialized.anchorSelectionIndex)
        assertEquals(-1, initialized.leadSelectionIndex)

        val expectedAfterInsertion =
            mapOf(
                ListSelection.SINGLE_SELECTION to listOf("b"),
                ListSelection.SINGLE_INTERVAL_SELECTION to listOf("inserted", "b", "c"),
                ListSelection.MULTIPLE_INTERVAL_SELECTION to listOf("inserted", "b", "c"),
                ListSelection.MULTIPLE_INTERVAL_SELECTION_DEFENSIVE to listOf("b", "c"),
            )

        for ((mode, expectedSelected) in expectedAfterInsertion) {
            val source = eventListOf("a", "b", "c", "d")
            val selection = ListSelection(source)
            selection.selectionMode = mode
            selection.select(1, 2)

            source.add(1, "inserted")

            assertEquals(expectedSelected, selection.selected.toList(), "mode $mode")
            assertEquals(source.size - expectedSelected.size, selection.deselected.size, "mode $mode")
        }
    }

    @Test
    fun selectionModesApplySingleIntervalAndArbitraryIntervalRules() {
        val single = ListSelection(eventListOf(0, 1, 2, 3, 4, 5, 6))
        single.selectionMode = ListSelection.SINGLE_SELECTION
        single.select(1, 3)
        assertEquals(listOf(1), single.selected)
        single.select(index = 5)
        assertEquals(listOf(5), single.selected)

        val interval = ListSelection(eventListOf(0, 1, 2, 3, 4, 5, 6))
        interval.selectionMode = ListSelection.SINGLE_INTERVAL_SELECTION
        interval.select(1, 2)
        interval.select(5, 6)
        assertEquals(listOf(5, 6), interval.selected)
        interval.select(index = 4)
        assertEquals(listOf(4, 5, 6), interval.selected)
        interval.deselect(5)
        assertEquals(listOf(4), interval.selected)

        for (mode in listOf(ListSelection.MULTIPLE_INTERVAL_SELECTION, ListSelection.MULTIPLE_INTERVAL_SELECTION_DEFENSIVE)) {
            val arbitrary = ListSelection(eventListOf(0, 1, 2, 3, 4))
            arbitrary.selectionMode = mode
            arbitrary.select(index = 1)
            arbitrary.select(index = 3)
            assertEquals(listOf(1, 3), arbitrary.selected, "mode $mode")
        }

        val invalidMode = ListSelection(eventListOf(0, 1, 2))
        invalidMode.selectionMode = 999
        invalidMode.select(index = 1)
        invalidMode.source.add(1, 9)
        assertEquals(999, invalidMode.selectionMode)
        assertEquals(listOf(1), invalidMode.selected)
        assertEquals(listOf(0, 9, 2), invalidMode.deselected)
    }

    @Test
    fun indexAndRangeOperationsSupportReversalOverlapAndReplacement() {
        val selection = ListSelection(eventListOf(0, 1, 2, 3, 4, 5, 6, 7))

        selection.setSelection(5, 3)
        assertSelection(selection, listOf(3, 4, 5), anchor = 5, lead = 3)

        selection.select(2, 4)
        assertSelection(selection, listOf(2, 3, 4, 5), anchor = 2, lead = 4)

        selection.deselect(4, 3)
        assertSelection(selection, listOf(2, 5), anchor = 4, lead = 3)

        selection.setSelection(6)
        assertSelection(selection, listOf(6), anchor = 6, lead = 6)
        assertEquals(6, selection.minSelectionIndex)
        assertEquals(6, selection.maxSelectionIndex)

        selection.setSelection(-1)
        assertEquals(emptyList<Int>(), selection.selected)
        assertEquals(-1, selection.minSelectionIndex)
        assertEquals(-1, selection.maxSelectionIndex)
        assertEquals(6, selection.anchorSelectionIndex, "clearing selection does not clear anchor")
        assertEquals(6, selection.leadSelectionIndex, "clearing selection does not clear lead")
    }

    @Test
    fun sortedIndexArraysAndLegacyOddArraysHaveExactEffects() {
        val selection = ListSelection(eventListOf(0, 1, 2, 3, 4, 5))

        selection.select(intArrayOf(0, 2, 5))
        assertEquals(listOf(0, 2, 5), selection.selected)
        selection.deselect(intArrayOf(2, 5))
        assertEquals(listOf(0), selection.selected)
        selection.setSelection(intArrayOf(1, 3, 4))
        assertEquals(listOf(1, 3, 4), selection.selected)
        selection.setSelection(intArrayOf())
        assertEquals(emptyList<Int>(), selection.selected)

        selection.setSelection(intArrayOf(2, 1))
        assertEquals(listOf(2), selection.selected, "an unsorted suffix is silently ignored")
        selection.setSelection(intArrayOf(1, 1, 2))
        assertEquals(listOf(1), selection.selected, "a duplicate blocks every later index")
        selection.setSelection(intArrayOf(3, 99))
        assertEquals(listOf(3), selection.selected, "an out-of-range suffix is silently ignored")
        selection.setSelection(intArrayOf(-1, 2))
        assertEquals(emptyList<Int>(), selection.selected, "a negative first index blocks all later indices")

        selection.select(intArrayOf(4, 2))
        assertEquals(listOf(4), selection.selected, "select(int[]) has the same sorted-input precondition")
        selection.deselect(intArrayOf(4, 4, 5))
        assertEquals(emptyList<Int>(), selection.selected)
    }

    @Test
    fun valueAndCollectionSelectionUsesFirstEqualValueAndDeduplicatedSourceIndices() {
        val source = eventListOf<String?>("a", "b", "a", null, "c")
        val selection = ListSelection(source)

        assertEquals(0, selection.selectValue("a"))
        assertEquals(-1, selection.selectValue("missing"))
        assertEquals(listOf("a"), selection.selected)

        assertTrue(selection.select(listOf("a", null, "missing", null)))
        assertEquals(listOf("a", null), selection.selected)
        assertFalse(selection.select(listOf("a", null)))
        assertFalse(selection.select(emptyList()))
        assertFalse(selection.isSelected(2), "equal duplicates after the first source match remain deselected")
    }

    @Test
    fun allInvertAndNoOpOperationsHaveExactListenerBehavior() {
        val empty = ListSelection(BasicEventList<String>())
        val emptyRanges = mutableListOf<Range>()
        empty.addSelectionListener { start, end -> emptyRanges += Range(start, end) }

        empty.selectAll()
        empty.deselectAll()
        empty.invertSelection()

        assertEquals(listOf(Range(0, -1)), emptyRanges, "empty inversion still reports an inverted range")

        val selection = ListSelection(eventListOf(0, 1, 2, 3))
        val ranges = mutableListOf<Range>()
        selection.addSelectionListener { start, end -> ranges += Range(start, end) }

        selection.selectAll()
        selection.selectAll()
        selection.deselectAll()
        selection.deselectAll()
        selection.select(index = 1)
        selection.select(index = 1)
        selection.invertSelection()

        assertEquals(
            listOf(Range(0, 3), Range(0, 3), Range(1, 1), Range(0, 3)),
            ranges,
        )
        assertEquals(listOf(0, 2, 3), selection.selected)
        assertEquals(-1, selection.anchorSelectionIndex)
        assertEquals(-1, selection.leadSelectionIndex)
    }

    @Test
    fun anchorLeadMovementReportsLegacyRedrawRangesExactly() {
        val selection = ListSelection(eventListOf(*(0..12).toList().toTypedArray()))
        val ranges = mutableListOf<Range>()
        selection.addSelectionListener { start, end -> ranges += Range(start, end) }

        selection.select(index = 4)
        selection.select(index = 10)
        selection.select(index = 10)

        assertEquals(listOf(Range(4, 4), Range(4, 10)), ranges)
        assertEquals(10, selection.anchorSelectionIndex)
        assertEquals(10, selection.leadSelectionIndex)

        ranges.clear()
        val leadSelection = ListSelection(eventListOf(*(0..12).toList().toTypedArray()))
        leadSelection.addSelectionListener { start, end -> ranges += Range(start, end) }
        leadSelection.setSelection(4)
        ranges.clear()
        leadSelection.leadSelectionIndex = 10
        assertEquals(
            listOf(Range(5, 10)),
            ranges,
            "the Java baseline redraws the new lead rather than the old lead when only the lead moves",
        )

        ranges.clear()
        leadSelection.anchorSelectionIndex = 8
        assertEquals(
            listOf(Range(8, 8)),
            ranges,
            "the Java baseline redraws the new anchor rather than the old anchor when only the anchor moves",
        )
    }

    @Test
    fun selectionListenerDispatchUsesCopyOnWriteSnapshots() {
        val selection = ListSelection(eventListOf("a", "b"))
        val trace = mutableListOf<String>()
        val late = ListSelection.Listener { _, _ -> trace += "late" }
        lateinit var second: ListSelection.Listener
        var changedRegistration = false
        val first = ListSelection.Listener { _, _ ->
            trace += "first"
            if (!changedRegistration) {
                selection.removeSelectionListener(second)
                selection.addSelectionListener(late)
                changedRegistration = true
            }
        }
        second = ListSelection.Listener { _, _ -> trace += "second" }
        selection.addSelectionListener(first)
        selection.addSelectionListener(second)

        selection.select(0)
        selection.select(1)

        assertEquals(listOf("first", "second", "first", "late"), trace)
    }

    @Test
    fun validSelectionMatchersAreConjunctiveAndAddingOneDeselectsInvalidRows() {
        val selection = ListSelection(eventListOf(0, 1, 2, 3, 4, 5))
        selection.selectAll()
        val ranges = mutableListOf<Range>()
        selection.addSelectionListener { start, end -> ranges += Range(start, end) }
        val evens = Matcher<Int> { it % 2 == 0 }
        val greaterThanOne = Matcher<Int> { it > 1 }

        selection.addValidSelectionMatcher(evens)
        assertEquals(listOf(0, 2, 4), selection.selected)
        assertEquals(listOf(Range(1, 1), Range(1, 3), Range(3, 5)), ranges)

        selection.addValidSelectionMatcher(greaterThanOne)
        assertEquals(listOf(2, 4), selection.selected)
        selection.select(0, 5)
        assertEquals(listOf(2, 4), selection.selected)

        selection.removeValidSelectionMatcher(evens)
        selection.select(0, 5)
        assertEquals(listOf(2, 3, 4, 5), selection.selected)
        selection.removeValidSelectionMatcher(greaterThanOne)
        assertEquals(listOf(2, 3, 4, 5), selection.selected, "removing a matcher does not select anything")
    }

    @Test
    fun arraysAllInvertUpdatesAndAdjacentInsertionsBypassSelectionMatchers() {
        val source = eventListOf("allowed", "blocked", "tail")
        val selection = ListSelection(source)
        selection.addValidSelectionMatcher(Matcher { it == "allowed" })

        selection.select(0, 2)
        assertEquals(listOf("allowed"), selection.selected)
        selection.deselectAll()
        selection.select(intArrayOf(1))
        assertEquals(listOf("blocked"), selection.selected)
        selection.deselectAll()
        selection.selectAll()
        assertEquals(source.toList(), selection.selected.toList())
        selection.deselectAll()
        selection.invertSelection()
        assertEquals(source.toList(), selection.selected.toList())

        selection.deselectAll()
        selection.selectionMode = ListSelection.MULTIPLE_INTERVAL_SELECTION
        selection.select(0)
        source.add(0, "inserted-but-blocked")
        assertEquals(listOf("inserted-but-blocked", "allowed"), selection.selected)

        source[0] = "still-blocked"
        assertEquals(listOf("still-blocked", "allowed"), selection.selected, "updates never revalidate selected rows")
    }

    @Test
    fun sourceInsertUpdateAndDeletePublishExactLiveViewEventsAndAdjustIndices() {
        val source = eventListOf("a", "b", "c", "d")
        val selection = ListSelection(source)
        selection.setSelection(1, 3)
        val selectedEvents = EventRecorder(selection.selected)
        val deselectedEvents = EventRecorder(selection.deselected)
        val ranges = mutableListOf<Range>()
        selection.addSelectionListener { start, end -> ranges += Range(start, end) }

        source.add(1, "x")
        assertTrue(selectedEvents.take().isEmpty())
        assertEquals(
            listOf(recordedEvent(change(ListEvent.INSERT, 1, ListEvent.UNKNOWN_VALUE, "x"))),
            deselectedEvents.take(),
        )
        assertEquals(Range(1, 4), ranges.removeFirst())
        assertEquals(2, selection.anchorSelectionIndex)
        assertEquals(4, selection.leadSelectionIndex)

        source[2] = "B"
        assertEquals(listOf(recordedEvent(change(ListEvent.UPDATE, 0, "b", "B"))), selectedEvents.take())
        assertTrue(deselectedEvents.take().isEmpty())
        assertTrue(ranges.isEmpty())

        source.removeAt(1)
        assertTrue(selectedEvents.take().isEmpty())
        assertEquals(
            listOf(recordedEvent(change(ListEvent.DELETE, 1, "x", ListEvent.UNKNOWN_VALUE))),
            deselectedEvents.take(),
        )
        assertEquals(Range(1, 4), ranges.removeFirst())
        assertEquals(1, selection.anchorSelectionIndex)
        assertEquals(3, selection.leadSelectionIndex)

        source.removeAt(3)
        assertEquals(
            listOf(recordedEvent(change(ListEvent.DELETE, 2, "d", ListEvent.UNKNOWN_VALUE))),
            selectedEvents.take(),
        )
        assertTrue(deselectedEvents.take().isEmpty())
        assertEquals(Range(1, 3), ranges.removeFirst())
        assertEquals(1, selection.anchorSelectionIndex)
        assertEquals(-1, selection.leadSelectionIndex)
        assertEquals(listOf("B", "c"), selection.selected)
        assertEquals(listOf("a"), selection.deselected)
    }

    @Test
    fun sourceInsertionIntoSelectedRunAndDeselectedUpdatePublishComplementaryViewEvents() {
        val source = eventListOf("a", "b", "c")
        val selection = ListSelection(source)
        selection.selectionMode = ListSelection.MULTIPLE_INTERVAL_SELECTION
        selection.setSelection(1)
        val selectedEvents = EventRecorder(selection.selected)
        val deselectedEvents = EventRecorder(selection.deselected)
        val ranges = mutableListOf<Range>()
        selection.addSelectionListener { start, end -> ranges += Range(start, end) }

        source.add(1, "x")

        assertEquals(
            listOf(recordedEvent(change(ListEvent.INSERT, 0, ListEvent.UNKNOWN_VALUE, "x"))),
            selectedEvents.take(),
        )
        assertTrue(deselectedEvents.take().isEmpty())
        assertEquals(listOf("x", "b"), selection.selected)
        assertEquals(listOf(Range(1, 2)), ranges)
        assertEquals(2, selection.anchorSelectionIndex)
        assertEquals(2, selection.leadSelectionIndex)

        source[0] = "A"

        assertTrue(selectedEvents.take().isEmpty())
        assertEquals(listOf(recordedEvent(change(ListEvent.UPDATE, 0, "a", "A"))), deselectedEvents.take())
        assertEquals(listOf(Range(1, 2)), ranges, "source updates do not notify selection listeners")
    }

    @Test
    fun sourceReorderPublishesProjectedMapsAndResetsAnchorAndLead() {
        val source = ReorderableEventList(listOf("a", "b", "c", "d"))
        val selection = ListSelection(source)
        selection.setSelection(1, 3)
        selection.deselect(2)
        val selectedEvents = EventRecorder(selection.selected)
        val deselectedEvents = EventRecorder(selection.deselected)
        val ranges = mutableListOf<Range>()
        selection.addSelectionListener { start, end -> ranges += Range(start, end) }

        source.reorder(intArrayOf(3, 2, 1, 0))

        assertEquals(listOf("d", "b"), selection.selected)
        assertEquals(listOf("c", "a"), selection.deselected)
        assertEquals(listOf(RecordedEvent(reorderMap = listOf(1, 0))), selectedEvents.take())
        assertEquals(listOf(RecordedEvent(reorderMap = listOf(1, 0))), deselectedEvents.take())
        assertEquals(listOf(Range(0, 3)), ranges)
        assertEquals(-1, selection.anchorSelectionIndex)
        assertEquals(-1, selection.leadSelectionIndex)
    }

    @Test
    fun viewsAreIdentityCachedAndPublisherDependenciesAreInstalledUntilParentDisposal() {
        val source = eventListOf("a", "b")
        val publisher = source.publisher as SequenceDependenciesEventPublisher
        val selection = ListSelection(source)
        val selected = selection.selected
        val deselected = selection.deselected
        val togglingSelected = selection.togglingSelected
        val togglingDeselected = selection.togglingDeselected

        assertSame(selected, selection.selected)
        assertSame(deselected, selection.deselected)
        assertSame(togglingSelected, selection.togglingSelected)
        assertSame(togglingDeselected, selection.togglingDeselected)
        assertTrue(publisher.getListeners<Any>(source).any { it === selection })
        assertEquals(
            listOf(selected, deselected, togglingSelected, togglingDeselected),
            publisher.getListeners<Any>(selection),
        )

        selected.dispose()
        deselected.dispose()
        togglingSelected.dispose()
        togglingDeselected.dispose()
        assertEquals(4, publisher.getListeners<Any>(selection).size, "disposing any exposed view is a no-op")

        selection.dispose()

        assertFalse(publisher.getListeners<Any>(source).any { it === selection })
        assertEquals(emptyList<Any>(), publisher.getListeners<Any>(selection))
    }

    @Test
    fun selectedAndDeselectedViewsMapWritesToSourceWithoutChangingSelectionPolicy() {
        val source = eventListOf("a", "b", "c", "d", "e")
        val selection = ListSelection(source)
        selection.setSelection(intArrayOf(1, 3))
        val selected = selection.selected
        val deselected = selection.deselected

        assertEquals("d", selected.set(1, "D"))
        assertEquals("c", deselected.set(1, "C"))
        assertEquals(listOf("a", "b", "C", "D", "e"), source)
        assertEquals(listOf("b", "D"), selected)

        assertEquals("b", selected.removeAt(0))
        assertEquals("C", deselected.removeAt(1))
        assertEquals(listOf("a", "D", "e"), source)
        assertEquals(listOf("D"), selected)
        assertEquals(listOf("a", "e"), deselected)

        selected.add(0, "x")
        selected.add(selected.size, "z")
        deselected.add(1, "y")
        assertEquals(listOf("a", "y", "x", "D", "e", "z"), source)
        assertEquals(listOf("D"), selected, "defensive insertion keeps selected-view additions deselected")
        assertEquals(listOf("a", "y", "x", "e", "z"), deselected)

        selected.clear()
        assertEquals(emptyList<String>(), selected)
        assertEquals(source.toList(), deselected.toList())
        selection.select(1, 2)
        deselected.clear()
        assertEquals(listOf("y", "x"), source)
        assertEquals(listOf("y", "x"), selected)
    }

    @Test
    fun togglingViewsMutateSelectionSupportBulkOperationsAndPreserveExactFailures() {
        val source = eventListOf("a", "b", "a", "c")
        val selection = ListSelection(source)
        val selected = selection.togglingSelected
        val deselected = selection.togglingDeselected

        assertTrue(selected.add("a"))
        assertEquals(listOf("a"), selected)
        assertFalse(selection.isSelected(2), "adding an equal duplicate selects source.indexOf() only")
        assertTrue(deselected.remove("b"))
        assertEquals(listOf("a", "b"), selected)
        assertTrue(selected.remove("a"))
        assertEquals(listOf("b"), selected)
        assertFalse(selected.remove("missing"))
        assertFalse(deselected.remove("missing"))
        assertEquals(listOf("a", "b", "a", "c"), source, "toggling never mutates source membership")

        selected.addAll(listOf("a", "c"))
        assertEquals(listOf("a", "b", "c"), selected)
        deselected.addAll(listOf("a", "b"))
        assertEquals(listOf("c"), selected)
        assertTrue(deselected.removeAll(listOf("a", "b")))
        assertEquals(listOf("a", "b", "a", "c"), selected)
        assertTrue(selected.removeAll(listOf("a", "c")))
        assertEquals(listOf("b"), selected)

        val setFailure = assertThrows(UnsupportedOperationException::class.java) { selected[0] = "x" }
        assertEquals("Toggling lists don't support setting items", setFailure.message)
        val missingFailure = assertThrows(IllegalArgumentException::class.java) { selected.add("missing") }
        assertEquals("Added item missing must be in source list", missingFailure.message)
        val lowFailure = assertThrows(IndexOutOfBoundsException::class.java) { selected.removeAt(-1) }
        assertEquals("Cannot remove at -1 on list of size 1", lowFailure.message)
        val highFailure = assertThrows(IndexOutOfBoundsException::class.java) { deselected.removeAt(deselected.size) }
        assertEquals("Cannot remove at ${deselected.size} on list of size ${deselected.size}", highFailure.message)

        selection.deselectAll()
        val partialFailure = assertThrows(IllegalArgumentException::class.java) {
            selected.addAll(listOf("a", "missing", "c"))
        }
        assertEquals("Added item missing must be in source list", partialFailure.message)
        assertEquals(listOf("a"), selected, "bulk toggling commits successful prefixes before failure")
    }

    @Test
    fun matcherFailureLeavesPartialSelectionAndAnUncommittedViewTransaction() {
        val source = eventListOf("ok", "boom", "after")
        val selection = ListSelection(source)
        val selected = selection.selected
        val deselected = selection.deselected
        val selectedEvents = EventRecorder(selected)
        selection.addValidSelectionMatcher(Matcher { value ->
            if (value == "boom") throw IllegalStateException("matcher boom")
            true
        })

        val thrown = assertThrows(IllegalStateException::class.java) { selection.select(0, 2) }

        assertEquals("matcher boom", thrown.message)
        assertEquals(listOf("ok"), selected)
        assertEquals(listOf("boom", "after"), deselected)
        assertTrue(selectedEvents.take().isEmpty(), "the selected insertion remains buffered because commitAll() was skipped")
        assertEquals(0, selection.anchorSelectionIndex)
        assertEquals(2, selection.leadSelectionIndex)
    }

    @Test
    fun boundariesPreserveNoOpsFailureMessagesAndPrevalidationAnchorMutation() {
        val selection = ListSelection(eventListOf(0, 1, 2, 3))
        val ranges = mutableListOf<Range>()
        selection.addSelectionListener { start, end -> ranges += Range(start, end) }

        assertFalse(selection.isSelected(-1))
        assertFalse(selection.isSelected(4))
        selection.select(-1, 99)
        selection.deselect(-1, 99)
        assertTrue(ranges.isEmpty())

        val thrown = assertThrows(IndexOutOfBoundsException::class.java) { selection.select(1, 9) }
        assertEquals("Invalid range for selection: 1-9, list size is 4", thrown.message)
        assertEquals(emptyList<Int>(), selection.selected)
        assertEquals(1, selection.anchorSelectionIndex)
        assertEquals(9, selection.leadSelectionIndex)
        assertTrue(ranges.isEmpty())

        selection.select(index = 2)
        assertEquals(listOf(2), selection.selected)
        assertEquals(listOf(Range(1, 9)), ranges, "the invalid old lead expands the next valid redraw range")

        val deselectFailure = assertThrows(IndexOutOfBoundsException::class.java) { selection.deselect(8) }
        assertEquals("Invalid range for selection: 8-8, list size is 4", deselectFailure.message)
        assertEquals(8, selection.anchorSelectionIndex)
        assertEquals(8, selection.leadSelectionIndex)
    }

    @Test
    fun parentDisposalDetachesAndClearsListenersButLeavesViewsAndDirectSelectionCallsUsable() {
        val source = eventListOf("a", "b", "c")
        val publisher = source.publisher as SequenceDependenciesEventPublisher
        val selection = ListSelection(source)
        selection.select(1)
        val selected = selection.selected
        val deselected = selection.deselected
        selection.togglingSelected
        selection.togglingDeselected
        val selectedEvents = EventRecorder(selected)
        val ranges = AtomicInteger()
        selection.addSelectionListener { _, _ -> ranges.incrementAndGet() }

        selected.dispose()
        source[1] = "B"
        assertEquals(listOf(recordedEvent(change(ListEvent.UPDATE, 0, "b", "B"))), selectedEvents.take())

        assertDoesNotThrow { selection.dispose() }
        assertDoesNotThrow { selection.dispose() }
        assertFalse(publisher.getListeners<Any>(source).any { it === selection })
        assertEquals(emptyList<Any>(), publisher.getListeners<Any>(selection))

        source[1] = "C"
        source.add("d")
        assertTrue(selectedEvents.take().isEmpty())
        assertEquals(listOf("C"), selected.toList(), "the stale barcode still reads current source values at old indices")
        assertEquals(1, selected.size)
        assertEquals(2, deselected.size)
        assertEquals(4, source.size)

        selection.select(0)
        assertEquals(listOf("a", "C"), selected)
        assertEquals(0, ranges.get(), "dispose clears selection listeners")
        assertEquals(listOf(recordedEvent(change(ListEvent.INSERT, 0, ListEvent.UNKNOWN_VALUE, "a"))), selectedEvents.take())
    }

    private fun assertSelection(selection: ListSelection<Int>, expected: List<Int>, anchor: Int, lead: Int) {
        assertEquals(expected, selection.selected.toList())
        assertEquals(selection.source.filterNot(expected::contains), selection.deselected.toList())
        assertEquals(anchor, selection.anchorSelectionIndex)
        assertEquals(lead, selection.leadSelectionIndex)
    }

    private fun <E> eventListOf(vararg values: E): BasicEventList<E> = BasicEventList<E>().apply { addAll(values.toList()) }

    private fun change(type: Int, index: Int, oldValue: Any?, newValue: Any?) = Change(type, index, oldValue, newValue)

    private fun recordedEvent(vararg changes: Change) = RecordedEvent(changes = changes.toList())

    private data class Range(val start: Int, val end: Int)

    private data class Change(
        val type: Int,
        val index: Int,
        val oldValue: Any?,
        val newValue: Any?,
    )

    private data class RecordedEvent(
        val changes: List<Change> = emptyList(),
        val reorderMap: List<Int>? = null,
    )

    private class EventRecorder<E>(source: EventList<E>) : ListEventListener<E> {
        private val events = mutableListOf<RecordedEvent>()

        init {
            source.addListEventListener(this)
        }

        override fun listChanged(listChanges: ListEvent<E>) {
            if (listChanges.isReordering) {
                events += RecordedEvent(reorderMap = listChanges.reorderMap.toList())
                return
            }
            val changes = mutableListOf<Change>()
            while (listChanges.next()) {
                changes += Change(listChanges.type, listChanges.index, listChanges.oldValue, listChanges.newValue)
            }
            events += RecordedEvent(changes)
        }

        fun take(): List<RecordedEvent> = events.toList().also { events.clear() }
    }

    private class ReorderableEventList<E>(elements: List<E>) : AbstractEventList<E>() {
        private var data = elements.toMutableList()

        init {
            readWriteLock = UpgradeDetectingReadWriteLock()
        }

        override val size: Int
            get() = data.size

        override fun get(index: Int): E = data[index]

        override fun dispose() = Unit

        fun reorder(reorderMap: IntArray) {
            val previous = data
            data = reorderMap.mapTo(ArrayList(previous.size), previous::get)
            updates.beginEvent()
            updates.reorder(reorderMap)
            updates.commitEvent()
        }
    }
}
