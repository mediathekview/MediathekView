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
import ca.odell.glazedlists.impl.UpgradeDetectingReadWriteLock
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*

internal class SeparatorListBehaviorTest {
    @Test
    fun constructionCoversEmptyNonemptySortingGroupingAndStableDuplicates() {
        val empty = BasicEventList<Row>()
        val emptySeparated = separated(empty, compareBy(Row::group), 0, Int.MAX_VALUE)
        assertTrue(emptySeparated.isEmpty())

        val twoFirst = Row(2, "two-first")
        val oneFirst = Row(1, "one-first")
        val twoSecond = Row(2, "two-second")
        val oneSecond = Row(1, "one-second")
        val source = BasicEventList<Row>().apply { addAll(listOf(twoFirst, oneFirst, twoSecond, oneSecond)) }
        val list = separated(source, compareBy(Row::group), 1, Int.MAX_VALUE)

        assertEquals(
            listOf(
                "S:one-first:2:2147483647",
                "one-first",
                "one-second",
                "S:two-first:2:2147483647",
                "two-first",
                "two-second"
            ),
            render(list),
        )
        assertSame(oneFirst, separatorAt<Row>(list, 0).first())
        assertSame(twoFirst, separatorAt<Row>(list, 3).first())
        assertEquals(listOf(oneFirst, oneSecond), separatorAt<Row>(list, 0).group)
    }

    @Test
    fun customReverseAndComparatorEquivalenceDefineOrderAndGroups() {
        val source = BasicEventList<String>().apply { addAll(listOf("b2", "A2", "a1", "B1", "c1")) }
        val firstLetterIgnoringCase = Comparator<String> { left, right ->
            left.first().lowercaseChar().compareTo(right.first().lowercaseChar())
        }
        val natural = separated(source, naturalOrder(), 1, Int.MAX_VALUE)
        val equivalent = separated(source, firstLetterIgnoringCase, 1, Int.MAX_VALUE)
        val reverse = separated(source, firstLetterIgnoringCase.reversed(), 1, Int.MAX_VALUE)

        assertEquals(
            listOf(
                "S:A2:1:2147483647",
                "A2",
                "S:B1:1:2147483647",
                "B1",
                "S:a1:1:2147483647",
                "a1",
                "S:b2:1:2147483647",
                "b2",
                "S:c1:1:2147483647",
                "c1"
            ), render(natural)
        )
        assertEquals(
            listOf(
                "S:A2:2:2147483647",
                "A2",
                "a1",
                "S:b2:2:2147483647",
                "b2",
                "B1",
                "S:c1:1:2147483647",
                "c1"
            ), render(equivalent)
        )
        assertEquals(
            listOf(
                "S:c1:1:2147483647",
                "c1",
                "S:b2:2:2147483647",
                "b2",
                "B1",
                "S:A2:2:2147483647",
                "A2",
                "a1"
            ), render(reverse)
        )
    }

    @Test
    fun nullableElementsWorkWithNullFriendlyComparatorAndPreserveStableOrder() {
        val source = BasicEventList<String?>().apply { addAll(listOf("bb", null, "a", null, "cc")) }
        val comparator = compareBy<String?, Int?>(nullsFirst()) { it?.length }
        val list = separated(source, comparator, 1, Int.MAX_VALUE)

        assertEquals(
            listOf(
                "S:null:2:2147483647",
                null,
                null,
                "S:a:1:2147483647",
                "a",
                "S:bb:2:2147483647",
                "bb",
                "cc"
            ), render(list)
        )
        assertEquals(listOf(null, null), separatorAt<String?>(list, 0).group)
    }

    @Test
    fun minimumSizeEdgesControlOnlySeparatorPresence() {
        val source = BasicEventList<String>().apply { addAll(listOf("B", "A", "A", "C", "C", "C")) }

        assertEquals(9, separated(source, naturalOrder(), -1, Int.MAX_VALUE).size)
        assertEquals(9, separated(source, naturalOrder(), 0, Int.MAX_VALUE).size)
        assertEquals(
            listOf("A", "A", "B", "S:C:3:2147483647", "C", "C", "C"),
            render(separated(source, naturalOrder(), 3, Int.MAX_VALUE))
        )
        assertEquals(listOf("A", "A", "B", "C", "C", "C"), render(separated(source, naturalOrder(), Int.MAX_VALUE, 0)))
    }

    @Test
    fun defaultLimitEdgesApplyOnlyToGroupsLargeEnoughForASeparator() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "A", "A", "B")) }

        assertEquals(listOf("S:A:3:-1", "B"), render(separated(source, naturalOrder(), 2, -1)))
        assertEquals(listOf("S:A:3:0", "B"), render(separated(source, naturalOrder(), 2, 0)))
        assertEquals(listOf("S:A:3:2", "A", "A", "B"), render(separated(source, naturalOrder(), 2, 2)))
        assertEquals(
            listOf("S:A:3:2147483647", "A", "A", "A", "B"),
            render(separated(source, naturalOrder(), 2, Int.MAX_VALUE))
        )
    }

    @Test
    fun separatorInterfaceExposesLiveGroupCachedValuesIdentityAndStringForm() {
        val a1 = Row(1, "a1")
        val a2 = Row(1, "a2")
        val source = BasicEventList<Row>().apply { addAll(listOf(a1, a2)) }
        val list = separated(source, compareBy(Row::group), 1, 1)
        val separator = separatorAt<Row>(list, 0)
        val liveGroup = separator.group

        assertEquals(1, separator.limit)
        assertEquals(2, separator.size())
        assertSame(a1, separator.first())
        assertEquals(listOf(a1, a2), liveGroup)
        assertEquals("2 elements starting with \"$a1\"", separator.toString())

        val a0 = Row(1, "a0")
        source.add(0, a0)

        assertSame(separator, list[0])
        assertEquals(3, separator.size())
        assertSame(a0, separator.first())
        assertEquals(listOf(a1, a2), liveGroup, "an already-created sublist retains its original range")
        assertEquals(listOf(a0, a1, a2), separator.group)
    }

    @Test
    fun removedSeparatorBecomesStaleAndIgnoresLimitChanges() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "A", "B")) }
        val list = separated(source, naturalOrder(), 1, Int.MAX_VALUE)
        val stale = separatorAt<String>(list, 0)
        val recorder = EventRecorder(list)

        source[0] = "B"
        source[1] = "B"

        assertEquals(listOf("S:B:3:2147483647", "B", "B", "B"), render(list))
        assertTrue(stale.group.isEmpty())
        assertNull(stale.first())
        assertEquals(0, stale.size())
        assertEquals("0 elements starting with \"null\"", stale.toString())
        val eventCount = recorder.size
        stale.limit = 7
        assertEquals(Int.MAX_VALUE, stale.limit)
        assertEquals(eventCount, recorder.size)
    }

    @Test
    fun collapsingAndExpandingOneGroupPublishesExactUnknownValuedChanges() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "A", "A", "B", "B")) }
        val list = separated(source, naturalOrder(), 1, Int.MAX_VALUE)
        val recorder = EventRecorder(list)
        val a = separatorAt<String>(list, 0)

        a.limit = 0
        assertEquals(listOf("S:A:3:0", "S:B:2:2147483647", "B", "B"), render(list))
        assertEquals(
            listOf(
                change(ListEvent.UPDATE, 0),
                change(ListEvent.DELETE, 1),
                change(ListEvent.DELETE, 1),
                change(ListEvent.DELETE, 1)
            ),
            recorder.last().changes,
        )
        recorder.assertReplayConsistent()

        a.limit = 2
        assertEquals(listOf("S:A:3:2", "A", "A", "S:B:2:2147483647", "B", "B"), render(list))
        assertEquals(
            listOf(change(ListEvent.UPDATE, 0), change(ListEvent.INSERT, 1), change(ListEvent.INSERT, 2)),
            recorder.last().changes
        )
        recorder.assertReplayConsistent()
    }

    @Test
    fun collapsingMultipleGroupsKeepsSeparatorIdentityAndProducesOneEventPerLimitChange() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "A", "B", "B", "C")) }
        val list = separated(source, naturalOrder(), 1, Int.MAX_VALUE)
        val separators = list.filterIsInstance<SeparatorList.Separator<String>>()
        val recorder = EventRecorder(list)

        separators.forEach { it.limit = 0 }

        assertEquals(listOf("S:A:2:0", "S:B:2:0", "S:C:1:0"), render(list))
        assertEquals(3, recorder.size)
        assertSame(separators[0], list[0])
        assertSame(separators[1], list[1])
        assertSame(separators[2], list[2])
        recorder.assertReplayConsistent()
    }

    @Test
    fun sourceAddSetRemoveClearCreateSplitMergeShiftAndRemoveGroupsConsistently() {
        val source = BasicEventList<String>()
        val list = separated(source, naturalOrder(), 1, Int.MAX_VALUE)
        val recorder = EventRecorder(list)

        mutateAndAssert(source, list, recorder, listOf("B", "B")) { source.addAll(listOf("B", "B")) }
        mutateAndAssert(source, list, recorder, listOf("A", "B", "B", "C")) {
            source.add(0, "A")
            source.add("C")
        }
        mutateAndAssert(source, list, recorder, listOf("A", "B", "C", "C")) { source[1] = "C" }
        mutateAndAssert(source, list, recorder, listOf("A", "B", "C")) { source.removeAt(1) }
        mutateAndAssert(source, list, recorder, emptyList()) { source.clear() }
        assertTrue(list.isEmpty())
    }

    @Test
    fun bug500NeighborBoundaryShiftsWorkInBothDirections() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "A", "C", "C", "C")) }
        val list = separated(source, naturalOrder(), 1, Int.MAX_VALUE)
        val recorder = EventRecorder(list)

        source[2] = "A"
        assertEquals(listOf("S:A:3:2147483647", "A", "A", "A", "S:C:2:2147483647", "C", "C"), render(list))
        recorder.assertReplayConsistent()

        source[2] = "C"
        assertEquals(listOf("S:A:2:2147483647", "A", "A", "S:C:3:2147483647", "C", "C", "C"), render(list))
        recorder.assertReplayConsistent()
    }

    @Test
    fun bug499BatchedUpdatesCurrentlyFailAfterTheSourceTransactionCommits() {
        val transaction = TransactionList(BasicEventList<String>())
        val comparator = compareBy<String> { it.first() }
        val list = separated(transaction, comparator, 0, Int.MAX_VALUE)
        val recorder = EventRecorder(list)
        listOf("MSFT", "MSFT", "IBM", "C", "IBM", "C", "C", "IBM", "IBM", "C").forEach(transaction::add)
        val eventCountBeforeFailure = recorder.size

        transaction.beginEvent()
        listOf("MSFT", "MSFT", "MSFT", "C", "C", "IBM", "IBM", "IBM", "C", "MSFT").forEachIndexed { index, value ->
            transaction[index] = value
        }
        val failure = assertThrows(NullPointerException::class.java) { transaction.commitEvent() }

        assertNull(failure.message)
        assertEquals(listOf("MSFT", "MSFT", "MSFT", "C", "C", "IBM", "IBM", "IBM", "C", "MSFT"), transaction.toList())
        assertEquals(eventCountBeforeFailure, recorder.size, "the Grouper failure publishes no additional outer event")
    }

    @Test
    fun bug522UpdateDeleteTransactionRetainsTrailingGroup() {
        val transaction = TransactionList(BasicEventList<String>().apply {
            addAll(listOf("A", "B", "C", "D", "DD", "DDD", "E", "F", "FF"))
        })
        val list = separated(transaction, compareBy<String> { it.first() }, 0, Int.MAX_VALUE)
        val recorder = EventRecorder(list)

        transaction.beginEvent()
        transaction[7] = "F"
        transaction.removeAt(8)
        transaction.commitEvent()

        assertEquals(listOf(1, 1, 1, 3, 1, 1), separatorsOf<String>(list).map { it.size() })
        assertEquals(listOf("A", "B", "C", "D", "E", "F"), separatorsOf<String>(list).map { it.first() })
        recorder.assertReplayConsistent()
    }

    @Test
    fun bug599MixedInsertUpdateTransactionKeepsOneFourElementGroup() {
        val transaction = TransactionList(BasicEventList<String>().apply { addAll(listOf("A", "A")) })
        val list = separated(transaction, String.CASE_INSENSITIVE_ORDER, 1, Int.MAX_VALUE)
        val recorder = EventRecorder(list)

        transaction.beginEvent(true)
        transaction.add(0, "A")
        transaction[1] = "A"
        transaction.add(3, "A")
        transaction.commitEvent()

        assertEquals(listOf("S:A:4:2147483647", "A", "A", "A", "A"), render(list))
        assertEquals(1, recorder.size)
        recorder.assertReplayConsistent()
    }

    @Test
    fun updateDeleteAndSortingRegressionTransactionsKeepAllGroupsConsistent() {
        val transaction = TransactionList(BasicEventList<Element>().apply {
            addAll((1..12).map { id -> Element((id - 1) % 3 + 1, id) })
        })
        val list = separated(transaction, compareBy(Element::group), 0, Int.MAX_VALUE)
        val recorder = EventRecorder(list)

        transaction.beginEvent()
        transaction[3] = Element(1, 4)
        transaction[7] = Element(1, 8)
        transaction.commitEvent()
        assertEquals(listOf(5, 3, 4), separatorsOf<Element>(list).map { it.size() })
        recorder.assertReplayConsistent()

        transaction.beginEvent()
        transaction[10] = Element(1, 11)
        transaction[1] = Element(3, 2)
        transaction.commitEvent()
        assertModelMatches(transaction, list, compareBy(Element::group), 0, Int.MAX_VALUE)
        recorder.assertReplayConsistent()
    }

    @Test
    fun comparatorChangesOnEmptyListAreDeferredAndRepeatedChangesUseLatestComparator() {
        val source = BasicEventList<String>()
        val list = separated(source, naturalOrder(), 0, 0)
        val recorder = EventRecorder(list)

        list.replaceComparator(String.CASE_INSENSITIVE_ORDER)
        list.replaceComparator(compareBy<String> { it.length })
        assertTrue(recorder.isEmpty())

        source.addAll(listOf("aa", "b", "cc"))
        assertEquals(listOf("S:b:1:0", "S:aa:2:0"), render(list))
        recorder.assertReplayConsistent()
    }

    @Test
    fun comparatorChangesOnNonemptyListPublishExactDeleteInsertReplacementEvenWhenRepeated() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "B", "A", "b")) }
        val list = separated(source, String.CASE_INSENSITIVE_ORDER, 1, Int.MAX_VALUE)
        val recorder = EventRecorder(list)

        list.replaceComparator(naturalOrder<String>())
        assertEquals(
            listOf(
                "S:A:1:2147483647",
                "A",
                "S:B:1:2147483647",
                "B",
                "S:a:1:2147483647",
                "a",
                "S:b:1:2147483647",
                "b"
            ), render(list)
        )
        assertEquals(
            (0..5).map { change(ListEvent.DELETE, 0) } + (0..7).map { change(ListEvent.INSERT, it) },
            recorder.last().changes
        )
        recorder.assertReplayConsistent()

        list.replaceComparator(String.CASE_INSENSITIVE_ORDER)
        assertEquals(listOf("S:a:2:2147483647", "a", "A", "S:B:2:2147483647", "B", "b"), render(list))
        recorder.assertReplayConsistent()
    }

    @Test
    fun comparatorChangesWithLimitsRebuildVisibilityAndDiscardPerGroupOverrides() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "a", "B", "b", "C")) }
        val list = separated(source, String.CASE_INSENSITIVE_ORDER, 0, 1)
        val originalA = separatorAt<String>(list, 0)
        originalA.limit = 0

        list.replaceComparator(naturalOrder<String>())

        assertEquals(
            listOf("S:A:1:1", "A", "S:B:1:1", "B", "S:C:1:1", "C", "S:a:1:1", "a", "S:b:1:1", "b"),
            render(list)
        )
        assertEquals(listOf("A"), originalA.group)
        assertEquals("A", originalA.first())
        assertEquals(
            2,
            originalA.size(),
            "cached size is not refreshed when comparator rebuilding abandons the old tree"
        )
        assertEquals(0, originalA.limit)
        assertFalse(list.any { it === originalA })
    }

    @Test
    fun comparatorFailureLeavesThePreviousOrderingUsable() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B")) }
        val list = separated(source, naturalOrder(), 0, Int.MAX_VALUE)
        val recorder = EventRecorder(list)
        val before = render(list)
        val failure = Comparator<String> { _, _ -> throw IllegalStateException("broken comparator") }

        val exception = assertThrows(IllegalStateException::class.java) { list.replaceComparator(failure) }

        assertEquals("broken comparator", exception.message)
        assertTrue(recorder.isEmpty())
        assertEquals(before, render(list))

        source.add("C")

        assertEquals(listOf("S:A:1:2147483647", "A", "S:B:1:2147483647", "B", "S:C:1:2147483647", "C"), render(list))
        assertEquals(1, recorder.size)
        recorder.assertReplayConsistent()
    }

    @Test
    fun comparatorFailureDuringGrouperRebuildRestoresCollapsedStateAndAllowsReuse() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "A", "B", "C")) }
        val list = separated(source, naturalOrder(), 0, 1)
        separatorAt<String>(list, 0).limit = 0
        val recorder = EventRecorder(list)
        val before = render(list)
        var throwOnFirstDescendingAdjacency = true
        val lateFailure = Comparator<String> { left, right ->
            if (throwOnFirstDescendingAdjacency && left < right) {
                throwOnFirstDescendingAdjacency = false
                throw IllegalStateException("late comparator failure")
            }
            right.compareTo(left)
        }

        val exception = assertThrows(IllegalStateException::class.java) { list.replaceComparator(lateFailure) }

        assertEquals("late comparator failure", exception.message)
        assertTrue(recorder.isEmpty())
        assertEquals(before, render(list))

        source.add("D")
        assertEquals(
            listOf("S:A:2:0", "S:B:1:1", "B", "S:C:1:1", "C", "S:D:1:1", "D"),
            render(list),
        )
        recorder.assertReplayConsistent()

        list.replaceComparator(lateFailure)
        source.add("E")

        assertEquals(
            listOf(
                "S:E:1:1", "E", "S:D:1:1", "D", "S:C:1:1", "C", "S:B:1:1", "B", "S:A:2:1", "A",
            ),
            render(list),
        )
        recorder.assertReplayConsistent()
    }

    @Test
    fun fullyVisibleWithinGroupSourceReorderForwardsExactReorderMap() {
        val one = Row(1, "one")
        val two = Row(1, "two")
        val three = Row(1, "three")
        val source = ReorderableEventList(listOf(one, two, three))
        val list = separated(source, compareBy(Row::group), 0, Int.MAX_VALUE)
        val separator = separatorAt<Row>(list, 0)
        val recorder = EventRecorder(list)

        source.reorder(intArrayOf(2, 0, 1))

        assertEquals(listOf("S:three:3:2147483647", "three", "one", "two"), render(list))
        assertSame(separator, list[0])
        assertEquals(listOf(0, 3, 1, 2), recorder.last().reorderMap)
        recorder.assertReplayConsistent()
    }

    @Test
    fun finiteLimitSourceReorderFallsBackToDeleteInsertChanges() {
        val source = ReorderableEventList(listOf(Row(1, "one"), Row(1, "two"), Row(1, "three")))
        val list = separated(source, compareBy(Row::group), 0, 2)
        val recorder = EventRecorder(list)

        source.reorder(intArrayOf(2, 0, 1))

        assertEquals(listOf("S:three:3:2", "three", "one"), render(list))
        assertNull(recorder.last().reorderMap)
        assertEquals(
            listOf(
                change(ListEvent.DELETE, 0),
                change(ListEvent.DELETE, 0),
                change(ListEvent.DELETE, 0),
                change(ListEvent.INSERT, 0),
                change(ListEvent.INSERT, 1),
                change(ListEvent.INSERT, 2),
            ),
            recorder.last().changes,
        )
        recorder.assertReplayConsistent()
    }

    @Test
    fun crossGroupSourceReorderIsSuppressedByTheSortedLayer() {
        val source = ReorderableEventList(listOf(Row(1, "one"), Row(2, "two")))
        val list = separated(source, compareBy(Row::group), 0, Int.MAX_VALUE)
        val recorder = EventRecorder(list)

        source.reorder(intArrayOf(1, 0))
        assertEquals(listOf("two", "one"), source.map(Row::id))
        assertEquals(listOf("S:one:1:2147483647", "one", "S:two:1:2147483647", "two"), render(list))
        assertTrue(recorder.isEmpty())
    }

    @Test
    fun writableElementMappingAndInheritedIteratorAndSublistWritesReachOriginalSource() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "B", "C")) }
        val list = separated(source, naturalOrder(), 1, Int.MAX_VALUE)

        assertEquals("A", list.set(1, "D"))
        assertEquals(listOf("B", "C", "D"), source.sorted())

        list.add(1, "A")
        assertTrue(source.contains("A"))

        val iterator = list.listIterator()
        while (iterator.hasNext()) {
            if (iterator.next() == "B") {
                iterator.remove()
                break
            }
        }
        assertFalse(source.contains("B"))

        val cIndex = list.indexOf("C")
        list.subList(cIndex, cIndex + 1).clear()
        assertFalse(source.contains("C"))
        assertModelMatches(source, list, naturalOrder(), 1, Int.MAX_VALUE)
    }

    @Test
    fun writesOnSeparatorsAndBoundsUseExactBaselineMessages() {
        val source = BasicEventList<String>().apply { addAll(listOf("A", "A")) }
        val list = separated(source, naturalOrder(), 1, Int.MAX_VALUE)

        listOf<(SeparatorList<Any?>) -> Unit>(
            { it.add(0, "X") },
            { it[0] = "X" },
            { it.removeAt(0) },
        ).forEach { operation ->
            val exception = assertThrows(IllegalArgumentException::class.java) { operation(list) }
            assertEquals("No source index exists for the separator located at index 0", exception.message)
        }

        assertEquals(
            "Cannot get at -1 on list of size 3",
            assertThrows(IndexOutOfBoundsException::class.java) { list[-1] }.message
        )
        assertEquals(
            "Cannot add at 4 on list of size 3",
            assertThrows(IndexOutOfBoundsException::class.java) { list.add(4, "X") }.message
        )
        assertEquals(
            "Cannot set at 3 on list of size 3",
            assertThrows(IndexOutOfBoundsException::class.java) { list[3] = "X" }.message
        )
        assertEquals(
            "Cannot remove at 3 on list of size 3",
            assertThrows(IndexOutOfBoundsException::class.java) { list.removeAt(3) }.message
        )
    }

    @Test
    fun disposalDetachesInOrderIsIdempotentAndLeavesAReadableStaleSnapshot() {
        val source = ListenerTrackingList(listOf("B", "A"))
        val list = separated(source, naturalOrder(), 0, Int.MAX_VALUE)
        val before = render(list)
        var outerEvents = 0
        list.addListEventListener { outerEvents++ }
        assertEquals(1, source.listenerCount)

        list.dispose()
        assertEquals(0, source.listenerCount)
        assertEquals(0, source.disposeCalls)
        assertDoesNotThrow { list.dispose() }
        assertEquals(-1, source.listenerCount, "dispose repeats the original-source detach on every call")

        source.add("C")
        assertEquals(0, outerEvents)
        assertEquals(before, render(list))
    }

    @Test
    fun transformedSourcesAreInjectedAndSortedRatherThanExposingTheOriginalSource() {
        val original = BasicEventList<String>().apply { addAll(listOf("B", "A")) }
        val list = separated(original, naturalOrder(), 0, Int.MAX_VALUE)
        val sourceField = TransformedList::class.java.getDeclaredField("source").apply { trySetAccessible() }
        val injected = sourceField.get(list)
        val sorted = sourceField.get(injected)

        assertEquals("ca.odell.glazedlists.SeparatorList${'$'}SeparatorInjectorList", injected.javaClass.name)
        assertTrue(sorted is SortedList<*>)
        assertSame(original, sourceField.get(sorted))
        assertFalse(SeparatorList::class.java.methods.any { it.name == "getSource" })
    }

    @Test
    fun deterministicRandomMutationsMaintainModelSeparatorIdentityAndEventReplay() {
        val random = Random(0x5EEDL)
        val source = BasicEventList<Tagged>()
        val comparator = compareBy(Tagged::group)
        val list = separated(source, comparator, 1, 2)
        val recorder = EventRecorder(list)
        var nextId = 0

        repeat(60) {
            when {
                source.isEmpty() || random.nextInt(4) == 0 -> {
                    source.add(random.nextInt(source.size + 1), Tagged(random.nextInt(4), nextId++))
                }

                random.nextInt(3) == 0 -> source.removeAt(random.nextInt(source.size))
                else -> source[random.nextInt(source.size)] = Tagged(random.nextInt(4), nextId++)
            }
            assertModelMatches(source, list, comparator, 1, 2)
            recorder.assertReplayConsistent()
            separatorsOf<Tagged>(list).forEach { separator ->
                assertEquals(separator.group.size, separator.size())
                assertSame(separator.group.first(), separator.first())
            }
        }
    }

    private fun <E> mutateAndAssert(
        source: EventList<E>,
        list: SeparatorList<Any?>,
        recorder: EventRecorder,
        expectedSorted: List<E>,
        mutation: () -> Unit,
    ) {
        mutation()
        assertEquals(expectedSorted, source.sortedWith(naturalOrderComparator()))
        assertModelMatches(source, list, naturalOrderComparator(), 1, Int.MAX_VALUE)
        recorder.assertReplayConsistent()
    }

    private fun <E> assertModelMatches(
        source: List<E>,
        list: SeparatorList<Any?>,
        comparator: Comparator<in E>,
        minimumSize: Int,
        limit: Int,
    ) {
        val sorted = source.withIndex().sortedWith { left, right ->
            val compared = comparator.compare(left.value, right.value)
            if (compared != 0) compared else left.index.compareTo(right.index)
        }.map { it.value }
        val expected = mutableListOf<Any?>()
        var index = 0
        while (index < sorted.size) {
            var end = index + 1
            while (end < sorted.size && comparator.compare(sorted[index], sorted[end]) == 0) end++
            val group = sorted.subList(index, end)
            if (group.size >= minimumSize) {
                expected += "S:${display(group.first())}:${group.size}:$limit"
                expected.addAll(group.take(limit.coerceAtLeast(0)))
            } else {
                expected.addAll(group)
            }
            index = end
        }
        assertEquals(expected.map(::display), render(list))
    }

    private fun <E> naturalOrderComparator(): Comparator<E> = Comparator { left, right ->
        @Suppress("UNCHECKED_CAST")
        (left as Comparable<Any?>).compareTo(right)
    }

    @Suppress("UNCHECKED_CAST")
    private fun <E> SeparatorList<Any?>.replaceComparator(comparator: Comparator<E>) {
        setComparator(comparator as Comparator<Any?>)
    }

    @Suppress("UNCHECKED_CAST")
    private fun <E> separated(
        source: EventList<E>,
        comparator: Comparator<in E>,
        minimumSize: Int,
        limit: Int,
    ): SeparatorList<Any?> = SeparatorList(source, comparator, minimumSize, limit) as SeparatorList<Any?>

    @Suppress("UNCHECKED_CAST")
    private fun <E> separatorAt(list: SeparatorList<Any?>, index: Int): SeparatorList.Separator<E> =
        list[index] as SeparatorList.Separator<E>

    @Suppress("UNCHECKED_CAST")
    private fun <E> separatorsOf(list: SeparatorList<Any?>): List<SeparatorList.Separator<E>> =
        list.filterIsInstance<SeparatorList.Separator<*>>().map { it as SeparatorList.Separator<E> }

    private fun render(list: List<Any?>): List<Any?> = list.map { value ->
        if (value is SeparatorList.Separator<*>) {
            "S:${display(value.first())}:${value.size()}:${value.limit}"
        } else {
            display(value)
        }
    }

    private fun display(value: Any?): Any? = when (value) {
        is Row -> value.id
        is Tagged -> "${value.group}:${value.id}"
        else -> value
    }

    private fun change(type: Int, index: Int) = Change(type, index, "UNKNOWN", "UNKNOWN")

    private data class Row(val group: Int, val id: String) {
        override fun toString(): String = id
    }

    private data class Tagged(val group: Int, val id: Int)

    private data class Element(val group: Int, val id: Int) {
        override fun equals(other: Any?): Boolean = other is Element && id == other.id
        override fun hashCode(): Int = id
    }

    private data class Change(val type: Int, val index: Int, val oldValue: Any?, val newValue: Any?)

    private data class RecordedEvent(
        val beforeSize: Int,
        val afterSize: Int,
        val reorderMap: List<Int>? = null,
        val changes: List<Change> = emptyList(),
    )

    private class EventRecorder(private val source: EventList<Any?>) : ListEventListener<Any?> {
        private val events = mutableListOf<RecordedEvent>()
        private var shadowSize = source.size

        init {
            source.addListEventListener(this)
        }

        val size: Int get() = events.size

        override fun listChanged(listChanges: ListEvent<Any?>) {
            val beforeSize = shadowSize
            val changes = mutableListOf<Change>()
            val reorder = if (listChanges.isReordering) listChanges.reorderMap.toList() else null
            if (reorder == null) {
                while (listChanges.next()) {
                    changes += Change(
                        listChanges.type,
                        listChanges.index,
                        eventValue(listChanges.oldValue),
                        eventValue(listChanges.newValue),
                    )
                }
            }
            shadowSize = source.size
            events += RecordedEvent(beforeSize, shadowSize, reorder, changes)
        }

        fun last(): RecordedEvent = events.last()
        fun isEmpty(): Boolean = events.isEmpty()

        fun assertReplayConsistent() {
            events.forEach { event ->
                if (event.reorderMap != null) {
                    assertEquals(event.beforeSize, event.reorderMap.size)
                    assertEquals((0 until event.beforeSize).toSet(), event.reorderMap.toSet())
                    assertEquals(event.beforeSize, event.afterSize)
                } else {
                    var replayedSize = event.beforeSize
                    event.changes.forEach { change ->
                        when (change.type) {
                            ListEvent.INSERT -> {
                                assertTrue(change.index in 0..replayedSize)
                                replayedSize++
                            }

                            ListEvent.DELETE -> {
                                assertTrue(change.index in 0 until replayedSize)
                                replayedSize--
                            }

                            ListEvent.UPDATE -> assertTrue(change.index in 0 until replayedSize)
                            else -> throw AssertionError("unexpected event type ${change.type}")
                        }
                    }
                    assertEquals(event.afterSize, replayedSize, event.toString())
                }
            }
            assertEquals(source.size, shadowSize)
        }

        private fun eventValue(value: Any?): Any? = if (value === ListEvent.UNKNOWN_VALUE) "UNKNOWN" else value
    }

    private class ReorderableEventList<E>(elements: List<E>) : AbstractEventList<E>() {
        private var data = elements.toMutableList()

        init {
            readWriteLock = UpgradeDetectingReadWriteLock()
        }

        override val size: Int get() = data.size
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

    private class ListenerTrackingList<E>(initial: List<E>) : AbstractEventList<E>() {
        private val data = initial.toMutableList()
        var listenerCount = 0
            private set
        var disposeCalls = 0
            private set

        init {
            readWriteLock = UpgradeDetectingReadWriteLock()
        }

        override val size: Int get() = data.size
        override fun get(index: Int): E = data[index]

        override fun addListEventListener(listChangeListener: ListEventListener<in E>) {
            listenerCount++
            super.addListEventListener(listChangeListener)
        }

        override fun removeListEventListener(listChangeListener: ListEventListener<in E>) {
            listenerCount--
            super.removeListEventListener(listChangeListener)
        }

        override fun add(index: Int, element: E) {
            updates.beginEvent()
            data.add(index, element)
            updates.elementInserted(index, element)
            updates.commitEvent()
        }

        override fun dispose() {
            disposeCalls++
        }
    }
}
