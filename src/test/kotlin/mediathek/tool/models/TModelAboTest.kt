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

package mediathek.tool.models

import mediathek.daten.ListeAbo
import mediathek.daten.abo.DatenAbo
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import javax.swing.RowSorter
import javax.swing.SortOrder
import javax.swing.table.TableRowSorter

class TModelAboTest {
    @Test
    fun sorterUsesAboReferenceValuesForTextColumns() {
        val abos = ListeAbo()
        abos.add(createAbo("ZDF", "Zebra"))
        abos.add(createAbo("ARD", "Alpha"))
        val model = TModelAbo(abos)

        val sorter = TableRowSorter(model)
        sorter.sortKeys = listOf(RowSorter.SortKey(DatenAbo.ABO_NAME, SortOrder.ASCENDING))

        assertEquals("Alpha", model.getValueAt(sorter.convertRowIndexToModel(0), DatenAbo.ABO_NAME))
        assertEquals("Zebra", model.getValueAt(sorter.convertRowIndexToModel(1), DatenAbo.ABO_NAME))
    }

    @Test
    fun senderFilterUsesAboListDirectly() {
        val abos = ListeAbo()
        abos.add(createAbo("ZDF", "Zebra"))
        abos.add(createAbo("ARD", "Alpha"))
        val model = TModelAbo(abos)

        model.setSenderFilter("ARD")

        assertEquals(1, model.rowCount)
        assertEquals("Alpha", model.getValueAt(0, DatenAbo.ABO_NAME))
    }

    private fun createAbo(sender: String, name: String): DatenAbo =
        DatenAbo().apply {
            this.sender = sender
            this.name = name
        }
}
