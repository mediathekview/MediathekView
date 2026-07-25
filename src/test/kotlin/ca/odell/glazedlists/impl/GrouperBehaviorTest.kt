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
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.SortedList
import ca.odell.glazedlists.TransactionList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.impl.adt.Barcode
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class GrouperBehaviorTest {
    @Test
    fun initialBarcodeConstantsGettersAndComparatorIdentityArePreserved() {
        val source = BasicEventList<String>().apply { addAll(listOf("b", "a", "a", "c")) }
        val comparator = naturalOrder<String>()
        val fixture = Fixture(source, comparator)
        val initialBarcode = fixture.grouper.barcode

        assertSame(Barcode.BLACK, Grouper.UNIQUE)
        assertSame(Barcode.WHITE, Grouper.DUPLICATE)
        assertSame(comparator, fixture.grouper.comparator)
        assertSame(fixture.client, fixture.grouper.client)
        assertEquals("UDUU", fixture.barcodePattern())

        fixture.grouper.comparator = comparator
        assertSame(initialBarcode, fixture.grouper.barcode)

        val equivalentComparator = Comparator<String> { left, right -> left.compareTo(right) }
        fixture.grouper.comparator = equivalentComparator
        assertSame(equivalentComparator, fixture.grouper.comparator)
        assertNotSame(initialBarcode, fixture.grouper.barcode)
        assertEquals("UDUU", fixture.barcodePattern())
        assertEquals(emptyList<Callback<String>>(), fixture.callbacks)
    }

    @Test
    fun uniqueAndDuplicateInsertionsPreserveCallbackOrderAndBarcode() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "a", "b")) }
        val fixture = Fixture(source)

        source.add("a")
        source.add("d")

        assertEquals("UDDUU", fixture.barcodePattern())
        assertEquals(
            listOf(
                callback(2, 0, ListEvent.UPDATE, true, ListEvent.INSERT, "a", "a"),
                callback(4, 2, ListEvent.INSERT, true, ListEvent.INSERT, unknown(), "d"),
            ),
            fixture.callbacks,
        )
    }

    @Test
    fun deletingFirstGroupElementThenWholeGroupPreservesPromotionCallbacks() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "a", "b")) }
        val fixture = Fixture(source)

        source.removeAt(0)
        source.removeAt(0)

        assertEquals("U", fixture.barcodePattern())
        assertEquals(
            listOf(
                callback(0, 0, ListEvent.UPDATE, true, ListEvent.DELETE, "a", "a", joinRight = true),
                callback(0, 0, ListEvent.DELETE, true, ListEvent.DELETE, "a", unknown(), joinRight = true),
            ),
            fixture.callbacks,
        )
    }

    @Test
    fun updatesSplitAndJoinGroupsWithExactPrimaryCallbacks() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "a", "c")) }
        val fixture = Fixture(source)

        source[1] = "b"
        assertEquals("UUU", fixture.barcodePattern())
        source[1] = "a"

        assertEquals("UDU", fixture.barcodePattern())
        assertEquals(
            listOf(
                callback(1, 0, ListEvent.UPDATE, false, ListEvent.UPDATE, "a", "a"),
                callback(1, 1, ListEvent.INSERT, true, ListEvent.UPDATE, unknown(), "b"),
                callback(1, 0, ListEvent.UPDATE, true, ListEvent.UPDATE, "a", "a"),
                callback(1, 1, ListEvent.DELETE, false, ListEvent.UPDATE, "b", unknown()),
            ),
            fixture.callbacks,
        )
    }

    @Test
    fun boundaryUpdatesPreserveBug500SeparatorFlags() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "a", "c", "c", "c")) }
        val fixture = Fixture(source)

        source[2] = "a"
        assertEquals("UDDU D".replace(" ", ""), fixture.barcodePattern())
        source[2] = "c"

        assertEquals("UDUDD", fixture.barcodePattern())
        assertEquals(
            listOf(
                callback(2, 0, ListEvent.UPDATE, true, ListEvent.UPDATE, "a", "a", updateNextSeparator = true),
                callback(2, 1, ListEvent.UPDATE, false, ListEvent.UPDATE, "c", "c"),
                callback(
                    2,
                    0,
                    ListEvent.UPDATE,
                    false,
                    ListEvent.UPDATE,
                    "a",
                    "a",
                    updateNextSeparator = true,
                    joinRight = true
                ),
                callback(2, 1, ListEvent.UPDATE, true, ListEvent.UPDATE, "c", "c", joinRight = true),
            ),
            fixture.callbacks,
        )
    }

    @Test
    fun glazedLists599BatchPreservesTwoPassStateAndCallbacks() {
        val base = BasicEventList<String>().apply { addAll(listOf("A", "A")) }
        val transaction = TransactionList(base)
        val fixture = Fixture(transaction, String.CASE_INSENSITIVE_ORDER)

        transaction.withTransaction {
            add(0, "A")
            this[1] = "A"
            add(3, "A")
        }

        assertEquals("UDDD", fixture.barcodePattern())
        assertEquals(
            listOf(
                callback(0, 0, ListEvent.UPDATE, true, ListEvent.INSERT, "A", "A"),
                callback(1, 0, ListEvent.UPDATE, true, ListEvent.UPDATE, "A", "A"),
                callback(3, 0, ListEvent.UPDATE, true, ListEvent.INSERT, "A", "A"),
            ),
            fixture.callbacks,
        )
    }

    @Test
    fun overriddenGettersDoNotRedirectInternalStateMachineAccess() {
        val source = BasicEventList<String>().apply { add("a") }
        val sortedList = SortedList(source, naturalOrder())
        val callbacks = mutableListOf<Int>()
        val grouper =
            object : Grouper<String>(
                sortedList,
                Client { _, groupIndex, _, _, _, _, _, _, _ -> callbacks += groupIndex },
            ) {
                override val client: Client<String>
                    get() = error("internal code called the overridable client getter")

                override var comparator: Comparator<in String>?
                    get() = error("internal code called the overridable comparator getter")
                    set(value) {
                        super.comparator = value
                    }

                override val barcode: Barcode
                    get() = error("internal code called the overridable barcode getter")

                fun actualBarcodePattern(): String {
                    val actualBarcode = super.barcode
                    return buildString {
                        repeat(actualBarcode.size()) { index ->
                            append(if (actualBarcode[index] === UNIQUE) 'U' else 'D')
                        }
                    }
                }
            }
        sortedList.addListEventListener(grouper::listChanged)

        source.add("a")

        assertEquals("UD", grouper.actualBarcodePattern())
        assertEquals(listOf(0), callbacks)
    }

    @Test
    fun failedComparatorRebuildLeavesNewPartiallyPopulatedBarcodeInstalled() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "a", "b")) }
        val fixture = Fixture(source)
        val originalBarcode = fixture.grouper.barcode
        var comparisonCount = 0
        val failingComparator = Comparator<String> { left, right ->
            if (++comparisonCount == 2) error("comparison failed")
            left.compareTo(right)
        }

        assertThrows(IllegalStateException::class.java) {
            fixture.grouper.comparator = failingComparator
        }

        assertSame(failingComparator, fixture.grouper.comparator)
        assertNotSame(originalBarcode, fixture.grouper.barcode)
        assertEquals("UD", fixture.barcodePattern())
    }

    private class Fixture<E>(
        source: EventList<E>,
        comparator: Comparator<in E>,
    ) {
        val sortedList = SortedList(source, comparator)
        val callbacks = mutableListOf<Callback<E>>()
        val client =
            Grouper.Client<E> { index, groupIndex, groupChangeType, primary, elementChangeType, oldValue, newValue, updateNextSeparator, joinRight ->
                callbacks += Callback(
                    index,
                    groupIndex,
                    groupChangeType,
                    primary,
                    elementChangeType,
                    oldValue,
                    newValue,
                    updateNextSeparator,
                    joinRight,
                )
            }
        val grouper = Grouper(sortedList, client)

        constructor(source: EventList<E>) : this(source, naturalOrderComparator())

        init {
            sortedList.addListEventListener(grouper::listChanged)
        }

        fun barcodePattern(): String = buildString {
            repeat(grouper.barcode.size()) { index ->
                append(if (grouper.barcode[index] === Grouper.UNIQUE) 'U' else 'D')
            }
        }
    }

    private data class Callback<E>(
        val index: Int,
        val groupIndex: Int,
        val groupChangeType: Int,
        val primary: Boolean,
        val elementChangeType: Int,
        val oldValue: E,
        val newValue: E,
        val updateNextSeparator: Boolean,
        val joinRight: Boolean,
    )

    private companion object {
        fun <E> callback(
            index: Int,
            groupIndex: Int,
            groupChangeType: Int,
            primary: Boolean,
            elementChangeType: Int,
            oldValue: E,
            newValue: E,
            updateNextSeparator: Boolean = false,
            joinRight: Boolean = false,
        ) = Callback(
            index,
            groupIndex,
            groupChangeType,
            primary,
            elementChangeType,
            oldValue,
            newValue,
            updateNextSeparator,
            joinRight,
        )

        fun <E> unknown(): E = ListEvent.unknownValue()

        @Suppress("UNCHECKED_CAST")
        fun <E> naturalOrderComparator(): Comparator<in E> = naturalOrder<Comparable<Any?>>() as Comparator<in E>
    }
}
