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

package mediathek.gui.abo

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.ListeAbo
import mediathek.daten.abo.DatenAbo
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean
import javax.swing.SwingUtilities

class AboTableFormatTest {
    @Test
    fun exposesAboColumnsForGlazedListsTableModel() {
        val abo = createAbo("ZDF", "Heute Journal").apply {
            mindestDauerMinuten = 15
            isDoNotStartAutomatically = true
        }
        val format = AboTableFormat { 42 }

        assertEquals(DatenAbo.MAX_ELEM, format.getColumnCount())
        assertEquals("Name", format.getColumnName(DatenAbo.ABO_NAME))
        assertEquals(Int::class.javaObjectType, format.getColumnClass(DatenAbo.ABO_MINDESTDAUER))
        assertEquals(Int::class.javaObjectType, format.getColumnClass(DatenAbo.ABO_FILM_COUNT))
        assertEquals(Boolean::class.javaObjectType, format.getColumnClass(DatenAbo.ABO_DO_NOT_START_AUTOMATICALLY))
        assertEquals("Heute Journal", format.getColumnValue(abo, DatenAbo.ABO_NAME))
        assertEquals(15, format.getColumnValue(abo, DatenAbo.ABO_MINDESTDAUER))
        assertEquals("Filme", format.getColumnName(DatenAbo.ABO_FILM_COUNT))
        assertEquals(42, format.getColumnValue(abo, DatenAbo.ABO_FILM_COUNT))
    }

    @Test
    fun exposesComparatorsForSortableColumnsOnly() {
        val format = AboTableFormat()

        assertNotNull(format.getColumnComparator(DatenAbo.ABO_NAME))
        assertNotNull(format.getColumnComparator(DatenAbo.ABO_MINDESTDAUER))
        assertNotNull(format.getColumnComparator(DatenAbo.ABO_FILM_COUNT))
        assertNotNull(format.getColumnComparator(DatenAbo.ABO_EINGESCHALTET))
    }

    @Test
    fun filmCountCanBeBlankWhileComputationIsRunning() {
        val format = AboTableFormat { null }

        assertEquals(Int::class.javaObjectType, format.getColumnClass(DatenAbo.ABO_FILM_COUNT))
        assertNull(format.getColumnValue(DatenAbo(), DatenAbo.ABO_FILM_COUNT))
    }

    @Test
    fun bindingFiltersRowsByExactSenderAndMatchesAllWhenCleared() {
        val abos = ListeAbo().apply {
            addAboWithoutNotification(createAbo("ZDF", "Zebra"))
            addAboWithoutNotification(createAbo("ARD", "Alpha"))
        }

        SwingUtilities.invokeAndWait {
            val table = AboTable()
            val binding = AboTableBinding(table, abos)
            try {
                assertEquals(2, table.rowCount)

                binding.setSenderFilter("ARD")
                assertEquals(1, table.rowCount)
                assertEquals("Alpha", table.getValueAt(0, DatenAbo.ABO_NAME))
                assertEquals("ARD", binding.aboAtViewRow(0)?.sender)

                binding.setSenderFilter("ard")
                assertEquals(0, table.rowCount)

                binding.setSenderFilter("ZDF")
                assertEquals(1, table.rowCount)
                assertEquals("ZDF", binding.aboAtViewRow(0)?.sender)

                binding.setSenderFilter(null)
                assertEquals(setOf("ARD", "ZDF"), table.senders(binding))

                binding.setSenderFilter("")
                assertEquals(setOf("ARD", "ZDF"), table.senders(binding))
            } finally {
                binding.dispose()
            }
        }
    }

    @Test
    fun clearingSenderFilterKeepsOnlyTheSelectedAboSelected() {
        val selectedAbo = createAbo("3Sat", "Iss besser!")
        val abos = ListeAbo().apply {
            addAboWithoutNotification(createAbo("ARD", "Alpha"))
            addAboWithoutNotification(selectedAbo)
            addAboWithoutNotification(createAbo("ZDF", "Zulu"))
        }

        SwingUtilities.invokeAndWait {
            val table = AboTable()
            val binding = AboTableBinding(table, abos)
            try {
                binding.clearSorting()
                binding.setSenderFilter("3Sat")
                table.selectionModel.setSelectionInterval(0, 0)

                binding.setSenderFilter(null)

                assertEquals(1, binding.selectedAboCount)
                assertSame(selectedAbo, binding.selectedAbos.single())
                assertSame(selectedAbo, binding.aboAtViewRow(table.selectedRow))
            } finally {
                binding.dispose()
            }
        }
    }

    private fun AboTable.senders(binding: AboTableBinding): Set<String> =
        (0 until rowCount).mapNotNull(binding::aboAtViewRow).mapTo(mutableSetOf(), DatenAbo::sender)

    @Test
    fun aboTableKeepsSortingOutOfSwingRowSorter() {
        SwingUtilities.invokeAndWait {
            val table = AboTable()
            val binding = AboTableBinding(table, ListeAbo())
            try {
                assertFalse(table.autoCreateRowSorter)
                assertNull(table.rowSorter)
                assertEquals(DatenAbo.MAX_ELEM, table.model.columnCount)
            } finally {
                binding.dispose()
            }
        }
    }

    @Test
    fun bindingKeepsTableModelWhenRowsAreUpdated() {
        val abo = createAbo("ZDF", "Heute Journal")
        val abos = ListeAbo().apply { addAboWithoutNotification(abo) }

        SwingUtilities.invokeAndWait {
            val table = AboTable()
            val binding = AboTableBinding(table, abos)
            try {
                val originalModel = table.model

                abos.fireAbosChanged(listOf(abo))

                assertSame(originalModel, table.model)
            } finally {
                binding.dispose()
            }
        }
    }

    @Test
    fun backgroundUpdatesAdjustSelectionOnTheEdt() {
        val abos = ListeAbo().apply {
            addAboWithoutNotification(createAbo("ARD", "Bravo"))
            addAboWithoutNotification(createAbo("ZDF", "Charlie"))
        }
        lateinit var binding: AboTableBinding
        lateinit var table: AboTable
        val selectionChanged = CountDownLatch(1)
        val selectionChangedOnEdt = AtomicBoolean()

        SwingUtilities.invokeAndWait {
            table = AboTable()
            binding = AboTableBinding(table, abos)
            table.selectionModel.setSelectionInterval(1, 1)
            table.selectionModel.addListSelectionListener { event ->
                if (!event.valueIsAdjusting) {
                    selectionChangedOnEdt.set(SwingUtilities.isEventDispatchThread())
                    selectionChanged.countDown()
                }
            }
        }

        try {
            Thread.ofVirtual().start {
                abos.addAbo(createAbo("ARTE", "Alpha"))
            }.join()

            assertTrue(selectionChanged.await(5, TimeUnit.SECONDS))
            assertTrue(selectionChangedOnEdt.get())
            SwingUtilities.invokeAndWait {
                assertEquals(1, binding.selectedAboCount)
                assertSame(binding.selectedAbos.single(), binding.aboAtViewRow(table.selectedRow))
            }
        } finally {
            SwingUtilities.invokeAndWait(binding::dispose)
        }
    }

    @Test
    fun bindingDisposeIsIdempotent() {
        SwingUtilities.invokeAndWait {
            val binding = AboTableBinding(AboTable(), ListeAbo())

            binding.dispose()
            binding.dispose()
        }
    }

    @Test
    fun columnSettingsHideLegacyDetailColumnsByDefault() {
        val config = ApplicationConfiguration.getInstance()
        val originalSettings = try {
            config.getTableColumnSettings("abo")
        } catch (_: NoSuchElementException) {
            ""
        }
        config.setTableColumnSettings("abo", "")

        try {
            SwingUtilities.invokeAndWait {
                val table = AboTable()
                val binding = AboTableBinding(table, ListeAbo())
                try {
                    AboTableColumnSettings(table, binding::clearSorting).load()

                    assertThrows(IllegalArgumentException::class.java) {
                        table.convertColumnIndexToView(DatenAbo.ABO_ZIELPFAD).takeIf { it >= 0 }
                            ?: throw IllegalArgumentException()
                    }
                } finally {
                    binding.dispose()
                }
            }
        } finally {
            config.setTableColumnSettings("abo", originalSettings)
        }
    }

    private fun createAbo(sender: String, name: String): DatenAbo =
        DatenAbo().apply {
            this.sender = sender
            this.name = name
        }
}
