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
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class GlazedListsImplBehaviorTest {
    @Test
    fun replaceAllUsesNaturalOrderingForInsertionsAndDeletions() {
        val target = BasicEventList<Int>().apply { addAll(listOf(1, 3, 5)) }

        GlazedListsImpl.replaceAll(target, listOf(1, 2, 5, 7), true, null)

        assertEquals(listOf(1, 2, 5, 7), target.toList())
    }

    @Test
    fun replaceAllControlsWhetherComparatorEqualValuesAreReplaced() {
        val original = Item(1, "original")
        val removed = Item(3, "removed")
        val replacement = Item(1, "replacement")
        val inserted = Item(2, "inserted")
        val comparator = compareBy<Item> { it.key }

        val preservingTarget = BasicEventList<Item>().apply { addAll(listOf(original, removed)) }
        GlazedListsImpl.replaceAll(preservingTarget, listOf(replacement, inserted), false, comparator)
        assertSame(original, preservingTarget[0])
        assertSame(inserted, preservingTarget[1])

        val updatingTarget = BasicEventList<Item>().apply { addAll(listOf(original, removed)) }
        GlazedListsImpl.replaceAll(updatingTarget, listOf(replacement, inserted), true, comparator)
        assertSame(replacement, updatingTarget[0])
        assertSame(inserted, updatingTarget[1])
    }

    @Test
    fun comparatorAndIdentityFactoriesPreserveTheirContracts() {
        val firstComparator = GlazedListsImpl.equalsComparator<String?>()
        val secondComparator = GlazedListsImpl.equalsComparator<String?>()
        val value = Any()

        assertEquals(0, firstComparator.compare(null, null))
        assertEquals(0, firstComparator.compare("same", "same"))
        assertEquals(1, firstComparator.compare("left", "right"))
        assertNotSame(firstComparator, secondComparator)
        assertSame(value, GlazedListsImpl.identityFunction<Any>()(value))
    }

    private data class Item(val key: Int, val label: String)
}
