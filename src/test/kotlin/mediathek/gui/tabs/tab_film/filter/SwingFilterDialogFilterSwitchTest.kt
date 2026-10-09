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

package mediathek.gui.tabs.tab_film.filter

import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.assumeUiAvailable
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.createDialogSetup
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.lockCurrentFilterCheckBox
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.onEdt
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.showNewOnlyCheckBox
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.triggerAddNewFilter
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.triggerCloneCurrentFilter
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.triggerDeleteCurrentFilter
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.triggerRenameCurrentFilter
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.triggerResetCurrentFilter
import mediathek.gui.tabs.tab_film.filter.SwingFilterDialogTestFixture.zeitraumSpinnerValue
import mediathek.tool.FilterDTO
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import kotlin.time.Duration.Companion.milliseconds
import kotlin.time.measureTime

internal class SwingFilterDialogFilterSwitchTest {

    @Test
    fun `combo box switch does not wait for theme catalog scan on EDT`() {
        assumeUiAvailable()

        val scanStarted = CountDownLatch(1)
        val releaseScan = CountDownLatch(1)
        val setup = createDialogSetup {
            scanStarted.countDown()
            releaseScan.await(5, TimeUnit.SECONDS)
            emptyList()
        }

        try {
            assertTrue(scanStarted.await(2, TimeUnit.SECONDS))

            val elapsed = measureTime {
                onEdt {
                    setup.comboBox.selectedItem = setup.secondFilter
                }
            }

            assertTrue(elapsed < 500.milliseconds, "filter switch blocked the EDT for $elapsed")
            assertEquals(setup.secondFilter, setup.controller.currentFilter())
        } finally {
            releaseScan.countDown()
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `combo box switch applies selected filter and requests table reload`() {
        assumeUiAvailable()

        val setup = createDialogSetup()
        val secondFilter = setup.secondFilter

        try {
            onEdt {
                setup.comboBox.selectedItem = secondFilter
            }
            onEdt {}

            assertEquals(secondFilter, setup.controller.currentFilter())
            assertEquals(true, setup.controller.state().showNewOnly)
            assertEquals(1, setup.reloadRequester.tableReloadRequests)
            assertEquals(0, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `combo box switch to zeitraum filter requests zeitraum reload`() {
        assumeUiAvailable()

        val setup = createDialogSetup()
        val zeitraumFilter = setup.zeitraumFilter

        try {
            onEdt {
                setup.comboBox.selectedItem = zeitraumFilter
            }
            onEdt {}

            assertEquals(zeitraumFilter, setup.controller.currentFilter())
            assertEquals("7", setup.controller.state().zeitraum)
            assertEquals(0, setup.reloadRequester.tableReloadRequests)
            assertEquals(1, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `adding a filter and selecting it updates dialog state without duplicate reload`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            val tableReloadsBeforeAdd = setup.reloadRequester.tableReloadRequests
            val addedFilter = when (val result = setup.controller.addFilter("Added Filter")) {
                is FilmFilterController.AddFilterResult.Added -> result.filter
                FilmFilterController.AddFilterResult.NameAlreadyExists -> error("filter name unexpectedly existed")
            }

            onEdt {
                setup.model.selectedItem = addedFilter
            }
            onEdt {}

            assertEquals(addedFilter, setup.controller.currentFilter())
            assertEquals(addedFilter, setup.comboBox.selectedItem)
            assertEquals(tableReloadsBeforeAdd, setup.reloadRequester.tableReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `adding a filter from a zeitraum-limited selection resets zeitraum to unlimited`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            onEdt {
                setup.comboBox.selectedItem = setup.zeitraumFilter
            }
            onEdt {}

            onEdt {
                triggerAddNewFilter(setup, "Fresh Filter")
            }
            onEdt {}

            assertEquals("Fresh Filter", setup.controller.currentFilter().name)
            assertEquals(ZeitraumSpinner.INFINITE_TEXT, setup.controller.state().zeitraum)
            assertEquals(ZeitraumSpinner.INFINITE_VALUE, zeitraumSpinnerValue(setup.dialog))
            assertEquals(0, setup.reloadRequester.tableReloadRequests)
            assertEquals(2, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `cloning current filter copies state and selects cloned filter`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            onEdt {
                setup.comboBox.selectedItem = setup.zeitraumFilter
            }
            onEdt {}

            onEdt {
                triggerCloneCurrentFilter(setup.dialog)
            }
            onEdt {}

            assertEquals("Filter 3 Kopie", setup.controller.currentFilter().name)
            assertEquals("7", setup.controller.state().zeitraum)
            assertEquals(setup.controller.currentFilter(), setup.comboBox.selectedItem)
            assertEquals(1, setup.reloadRequester.tableReloadRequests)
            assertEquals(1, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `deleting current filter updates combo box selection`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            onEdt {
                setup.comboBox.selectedItem = setup.secondFilter
            }
            onEdt {}

            val tableReloadsBeforeDelete = setup.reloadRequester.tableReloadRequests
            val zeitraumReloadsBeforeDelete = setup.reloadRequester.zeitraumReloadRequests

            onEdt {
                triggerDeleteCurrentFilter(setup.dialog)
            }
            onEdt {}

            assertEquals("Filter 1", setup.controller.currentFilter().name)
            assertEquals(setup.controller.currentFilter(), setup.comboBox.selectedItem)
            assertFalse(setup.controller.availableFilters().any { it == setup.secondFilter })
            assertEquals(tableReloadsBeforeDelete + 1, setup.reloadRequester.tableReloadRequests)
            assertEquals(zeitraumReloadsBeforeDelete, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `renaming current filter updates selected combo box entry`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            val renameResult = setup.controller.renameCurrentFilter("Renamed Filter 1")
            onEdt {}

            assertEquals(FilmFilterController.RenameFilterResult.Renamed, renameResult)
            assertEquals("Renamed Filter 1", setup.controller.currentFilter().name)
            assertEquals("Renamed Filter 1", (setup.comboBox.selectedItem as FilterDTO).name)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `rename current filter action updates selected combo box entry`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            onEdt {
                triggerRenameCurrentFilter(setup, "Renamed Via Action")
            }
            onEdt {}

            assertEquals("Renamed Via Action", setup.controller.currentFilter().name)
            assertEquals("Renamed Via Action", (setup.comboBox.selectedItem as FilterDTO).name)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `reset current filter action resets selected filter state and keeps selection`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            onEdt {
                setup.comboBox.selectedItem = setup.secondFilter
            }
            onEdt {}

            assertEquals(true, setup.controller.state().showNewOnly)
            val tableReloadsBeforeReset = setup.reloadRequester.tableReloadRequests
            val zeitraumReloadsBeforeReset = setup.reloadRequester.zeitraumReloadRequests

            onEdt {
                triggerResetCurrentFilter(setup.dialog)
            }
            onEdt {}

            assertEquals(setup.secondFilter, setup.controller.currentFilter())
            assertEquals(setup.secondFilter, setup.comboBox.selectedItem)
            assertEquals(false, setup.controller.state().showNewOnly)
            assertEquals(tableReloadsBeforeReset + 1, setup.reloadRequester.tableReloadRequests)
            assertEquals(zeitraumReloadsBeforeReset, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `reset current filter action restores saved values after a live checkbox edit`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            assertEquals(false, setup.controller.state().showNewOnly)

            onEdt {
                showNewOnlyCheckBox(setup.dialog).doClick()
            }
            onEdt {}

            assertEquals(true, setup.controller.state().showNewOnly)
            val tableReloadsBeforeReset = setup.reloadRequester.tableReloadRequests
            val zeitraumReloadsBeforeReset = setup.reloadRequester.zeitraumReloadRequests

            onEdt {
                triggerResetCurrentFilter(setup.dialog)
            }
            onEdt {}

            assertEquals("Filter 1", setup.controller.currentFilter().name)
            assertEquals(false, setup.controller.state().showNewOnly)
            assertEquals(tableReloadsBeforeReset + 1, setup.reloadRequester.tableReloadRequests)
            assertEquals(zeitraumReloadsBeforeReset, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `locked dialog applies checkbox change without persisting it to selected filter`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            assertFalse(setup.controller.state().showNewOnly)

            onEdt {
                lockCurrentFilterCheckBox(setup.dialog).doClick()
                showNewOnlyCheckBox(setup.dialog).doClick()
            }
            onEdt {}

            assertTrue(setup.controller.areCurrentFilterChangesLocked())
            assertTrue(setup.controller.state().showNewOnly)

            onEdt {
                setup.comboBox.selectedItem = setup.secondFilter
            }
            onEdt {}
            onEdt {
                setup.comboBox.selectedItem = setup.controller.availableFilters().first()
            }
            onEdt {}

            assertFalse(setup.controller.state().showNewOnly)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }

    @Test
    fun `lock checkbox follows persisted state of selected filter`() {
        assumeUiAvailable()

        val setup = createDialogSetup()

        try {
            onEdt {
                lockCurrentFilterCheckBox(setup.dialog).doClick()
            }
            onEdt {}

            assertTrue(setup.controller.areCurrentFilterChangesLocked())

            onEdt {
                setup.comboBox.selectedItem = setup.secondFilter
            }
            onEdt {}

            assertFalse(setup.controller.areCurrentFilterChangesLocked())
            assertFalse(lockCurrentFilterCheckBox(setup.dialog).isSelected)

            onEdt {
                setup.comboBox.selectedItem = setup.controller.availableFilters().first()
            }
            onEdt {}

            assertTrue(setup.controller.areCurrentFilterChangesLocked())
            assertTrue(lockCurrentFilterCheckBox(setup.dialog).isSelected)
        } finally {
            onEdt {
                setup.dialog.dispose()
                setup.model.close()
            }
        }
    }
}
