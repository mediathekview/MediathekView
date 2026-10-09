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
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.replaceAll
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test

internal class DiffBehaviorTest {
    @Test
    fun emptyAndEqualListsAreNoOpsWhenUpdatesAreDisabled() {
        val empty = BasicEventList<String>()
        val emptyEvents = captureEvents(empty) { Diff.replaceAll(empty, emptyList(), false) }
        assertEquals(emptyList<EventStep>(), emptyEvents)

        val first = Item(1, "first")
        val second = Item(2, "second")
        val target = BasicEventList<Item>().apply { addAll(listOf(first, second)) }
        val events = captureEvents(target) { Diff.replaceAll(target, listOf(first, second), false) }

        assertEquals(emptyList<EventStep>(), events)
        assertSame(first, target[0])
        assertSame(second, target[1])
    }

    @Test
    fun updatesControlWhetherComparatorEqualValuesAreReplacedInOrder() {
        val originalFirst = Item(1, "original-first")
        val originalSecond = Item(2, "original-second")
        val replacementFirst = Item(1, "replacement-first")
        val replacementSecond = Item(2, "replacement-second")
        val comparator = compareBy<Item> { it.key }

        val preservingTarget = BasicEventList<Item>().apply { addAll(listOf(originalFirst, originalSecond)) }
        assertEquals(
            emptyList<EventStep>(),
            captureEvents(preservingTarget) {
                Diff.replaceAll(preservingTarget, listOf(replacementFirst, replacementSecond), false, comparator)
            },
        )
        assertSame(originalFirst, preservingTarget[0])
        assertSame(originalSecond, preservingTarget[1])

        val updatingTarget = BasicEventList<Item>().apply { addAll(listOf(originalFirst, originalSecond)) }
        assertEquals(
            listOf(
                EventStep(ListEvent.UPDATE, 0, originalFirst, replacementFirst),
                EventStep(ListEvent.UPDATE, 1, originalSecond, replacementSecond),
            ),
            captureEvents(updatingTarget) {
                Diff.replaceAll(updatingTarget, listOf(replacementFirst, replacementSecond), true, comparator)
            },
        )
        assertSame(replacementFirst, updatingTarget[0])
        assertSame(replacementSecond, updatingTarget[1])
    }

    @Test
    fun disjointListsDeleteFromTheFrontBeforeInsertingTheSource() {
        val target = BasicEventList<String>().apply { addAll(listOf("A", "B")) }

        val events = captureEvents(target) { Diff.replaceAll(target, listOf("X", "Y"), false) }

        assertEquals(listOf("X", "Y"), target.toList())
        assertEquals(
            listOf(
                EventStep(ListEvent.DELETE, 0, "A", ListEvent.UNKNOWN_VALUE),
                EventStep(ListEvent.DELETE, 0, "B", ListEvent.UNKNOWN_VALUE),
                EventStep(ListEvent.INSERT, 0, ListEvent.UNKNOWN_VALUE, "X"),
                EventStep(ListEvent.INSERT, 1, ListEvent.UNKNOWN_VALUE, "Y"),
            ),
            events,
        )
    }

    @Test
    fun isolatedInsertAndDeleteUseTheCurrentTargetIndex() {
        val insertionTarget = BasicEventList<String>().apply { addAll(listOf("A", "C")) }
        assertEquals(
            listOf(EventStep(ListEvent.INSERT, 1, ListEvent.UNKNOWN_VALUE, "B")),
            captureEvents(insertionTarget) { Diff.replaceAll(insertionTarget, listOf("A", "B", "C"), false) },
        )

        val deletionTarget = BasicEventList<String>().apply { addAll(listOf("A", "B", "C")) }
        assertEquals(
            listOf(EventStep(ListEvent.DELETE, 1, "B", ListEvent.UNKNOWN_VALUE)),
            captureEvents(deletionTarget) { Diff.replaceAll(deletionTarget, listOf("A", "C"), false) },
        )
    }

    @Test
    fun duplicateMatchingIsStableForTheCurrentMyersTieBreak() {
        val target = BasicEventList<String>().apply { addAll(listOf("A", "B", "A", "C")) }

        val events = captureEvents(target) { Diff.replaceAll(target, listOf("A", "A", "B", "C"), false) }

        assertEquals(listOf("A", "A", "B", "C"), target.toList())
        assertEquals(
            listOf(
                EventStep(ListEvent.DELETE, 1, "B", ListEvent.UNKNOWN_VALUE),
                EventStep(ListEvent.INSERT, 2, ListEvent.UNKNOWN_VALUE, "B"),
            ),
            events,
        )
    }

    @Test
    fun comparatorIsUsedForEqualityOnlyAndDoesNotRequireSortOrdering() {
        val left = Item(2, "left")
        val middle = Item(1, "middle")
        val right = Item(3, "right")
        val replacementLeft = Item(2, "replacement-left")
        val replacementMiddle = Item(1, "replacement-middle")
        val replacementRight = Item(3, "replacement-right")
        val comparedPairs = mutableListOf<Pair<Int, Int>>()
        val equalityOnlyComparator = Comparator<Item> { alpha, beta ->
            comparedPairs += alpha.key to beta.key
            if (alpha.key == beta.key) 0 else 7
        }
        val target = BasicEventList<Item>().apply { addAll(listOf(left, middle, right)) }

        Diff.replaceAll(
            target,
            listOf(replacementLeft, replacementMiddle, replacementRight),
            true,
            equalityOnlyComparator,
        )

        assertEquals(listOf(replacementLeft, replacementMiddle, replacementRight), target.toList())
        assertEquals(listOf(2 to 2, 1 to 1, 3 to 3), comparedPairs)
    }

    @Test
    fun equalsComparatorSupportsNullsAndThePublicExtensionRetainsTheSamePath() {
        val target = BasicEventList<String?>().apply { addAll(listOf(null, "A")) }

        val events = captureEvents(target) {
            target.replaceAll(listOf(null, "B"), false)
        }

        assertEquals(listOf(null, "B"), target.toList())
        assertEquals(
            listOf(
                EventStep(ListEvent.DELETE, 1, "A", ListEvent.UNKNOWN_VALUE),
                EventStep(ListEvent.INSERT, 1, ListEvent.UNKNOWN_VALUE, "B"),
            ),
            events,
        )
    }

    private fun <E> captureEvents(target: EventList<E>, action: () -> Unit): List<EventStep> {
        val events = mutableListOf<EventStep>()
        target.addListEventListener { event ->
            while (event.next()) {
                events += EventStep(event.type, event.index, event.oldValue, event.newValue)
            }
        }
        action()
        return events
    }

    private data class EventStep(
        val type: Int,
        val index: Int,
        val oldValue: Any?,
        val newValue: Any?,
    )

    private data class Item(val key: Int, val label: String)
}
