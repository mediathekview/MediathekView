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
import ca.odell.glazedlists.GlazedLists
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test

internal class SimpleFactoryLeavesBehaviorTest {
    @Test
    fun listCollectionModelReturnsTheParent() {
        val model = ListCollectionListModel<String>()
        val parent = mutableListOf("first", "second")

        assertSame(parent, model.getChildren(parent))
        assertSame(parent, GlazedLists.listCollectionListModel<String>().getChildren(parent))
    }

    @Test
    fun simpleFunctionListMapsCurrentValuesAndForwardsSourceEvents() {
        val source = BasicEventList<String>().apply { addAll(listOf("a", "bbbb")) }
        val mapped = SimpleFunctionList(source, String::length)
        var events = 0
        mapped.addListEventListener { events++ }

        assertEquals(listOf(1, 4), mapped.toList())

        source[0] = "ccc"
        source.add("xx")
        source.removeAt(1)

        assertEquals(listOf(3, 2), mapped.toList())
        assertEquals(3, events)
        assertThrows(IllegalStateException::class.java) { mapped.add(0, 10) }
    }

    @Test
    fun disposingSimpleFunctionListStopsForwardingEvents() {
        val source = BasicEventList<String>().apply { add("a") }
        val mapped = SimpleFunctionList(source, String::length)
        var events = 0
        mapped.addListEventListener { events++ }

        mapped.dispose()
        source.add("bb")

        assertEquals(0, events)
    }
}
