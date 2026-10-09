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
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.event.ListEvent
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertSame
import org.junit.jupiter.api.Test
import javax.swing.event.ListDataEvent
import javax.swing.event.TableModelEvent
import javax.swing.table.DefaultTableModel

internal class MutableSwingEventTest {
    @Test
    fun listEventStartsWithTheLegacyDefaultState() {
        val source = Any()
        val event = MutableListDataEvent(source)

        assertSame(source, event.source)
        assertEquals(ListDataEvent.CONTENTS_CHANGED, event.type)
        assertEquals(0, event.index0)
        assertEquals(0, event.index1)
        assertEquals("0[0,0]", event.toString())
    }

    @Test
    fun listEventCanRewriteItsTypeAndInclusiveRange() {
        val event = MutableListDataEvent(Any())

        event.setRange(3, 7)
        event.setType(ListDataEvent.INTERVAL_REMOVED)

        assertEquals(ListDataEvent.INTERVAL_REMOVED, event.type)
        assertEquals(3, event.index0)
        assertEquals(7, event.index1)
        assertEquals("2[3,7]", event.toString())
    }

    @Test
    fun tableEventStartsWithTheSwingDefaultState() {
        val source = DefaultTableModel()
        val event = MutableTableModelEvent(source)

        assertSame(source, event.source)
        assertEquals(TableModelEvent.UPDATE, event.type)
        assertEquals(0, event.firstRow)
        assertEquals(Int.MAX_VALUE, event.lastRow)
        assertEquals(TableModelEvent.ALL_COLUMNS, event.column)
    }

    @Test
    fun tableEventMapsEveryListChangeType() {
        val event = MutableTableModelEvent(DefaultTableModel())

        listOf(
            ListEvent.INSERT to TableModelEvent.INSERT,
            ListEvent.DELETE to TableModelEvent.DELETE,
            ListEvent.UPDATE to TableModelEvent.UPDATE,
        ).forEachIndexed { index, (listType, tableType) ->
            event.setValues(index, index + 2, listType)

            assertEquals(index, event.firstRow)
            assertEquals(index + 2, event.lastRow)
            assertEquals(tableType, event.type)
            assertEquals(TableModelEvent.ALL_COLUMNS, event.column)
        }
    }

    @Test
    fun tableEventCanRepresentStructureAndWholeDataChanges() {
        val event = MutableTableModelEvent(DefaultTableModel())

        event.setStructureChanged()
        assertEquals(TableModelEvent.HEADER_ROW, event.firstRow)
        assertEquals(TableModelEvent.HEADER_ROW, event.lastRow)
        assertEquals(TableModelEvent.ALL_COLUMNS, event.column)
        assertEquals(TableModelEvent.UPDATE, event.type)

        event.setAllDataChanged()
        assertEquals(0, event.firstRow)
        assertEquals(Int.MAX_VALUE, event.lastRow)
        assertEquals(TableModelEvent.ALL_COLUMNS, event.column)
        assertEquals(TableModelEvent.UPDATE, event.type)
    }

    @Test
    fun unknownListChangeTypeLeavesTheCurrentTableEventTypeUnchanged() {
        val event = MutableTableModelEvent(DefaultTableModel())
        event.setType(TableModelEvent.INSERT)

        event.setValues(4, 6, Int.MIN_VALUE)

        assertEquals(4, event.firstRow)
        assertEquals(6, event.lastRow)
        assertEquals(TableModelEvent.INSERT, event.type)
        assertEquals(TableModelEvent.ALL_COLUMNS, event.column)
    }
}
