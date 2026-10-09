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

package mediathek.gui.tabs.tab_film.filter_selection

import mediathek.config.application.FilterConfiguration
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.tool.FilterDTO
import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Assertions.assertTrue
import org.junit.jupiter.api.Test
import java.util.*

internal class FilmFilterSelectionSynchronizerTest {

    @Test
    fun `combo model selection changes restore the active filter without the dialog`() {
        val setup = createSetup()

        try {
            setup.model.selectedItem = setup.secondFilter

            assertEquals(setup.secondFilter, setup.controller.currentFilter())
            assertTrue(setup.controller.state().showNewOnly)
            assertEquals(1, setup.reloadRequester.tableReloadRequests)
            assertEquals(0, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            setup.close()
        }
    }

    @Test
    fun `combo model selection changes request zeitraum reload when needed`() {
        val setup = createSetup()

        try {
            setup.model.selectedItem = setup.zeitraumFilter

            assertEquals(setup.zeitraumFilter, setup.controller.currentFilter())
            assertEquals("7", setup.controller.state().zeitraum)
            assertEquals(0, setup.reloadRequester.tableReloadRequests)
            assertEquals(1, setup.reloadRequester.zeitraumReloadRequests)
        } finally {
            setup.close()
        }
    }

    private fun createSetup(): TestSetup {
        val firstFilter = FilterDTO(UUID.randomUUID(), "Filter 1")
        val secondFilter = FilterDTO(UUID.randomUUID(), "Filter 2")
        val zeitraumFilter = FilterDTO(UUID.randomUUID(), "Filter 3")
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        filterConfiguration.addNewFilter(firstFilter)
        filterConfiguration.addNewFilter(secondFilter)
        filterConfiguration.addNewFilter(zeitraumFilter)

        filterConfiguration.setCurrentFilter(secondFilter)
        filterConfiguration.setShowNewOnly(true)
        filterConfiguration.setCurrentFilter(zeitraumFilter)
        filterConfiguration.setZeitraum("7")
        filterConfiguration.setCurrentFilter(firstFilter)

        val reloadRequester = RecordingReloadRequester()
        val controller = FilmFilterController(filterConfiguration, reloadRequester = reloadRequester)
        val model = FilterSelectionComboBoxModel(
            selectedFilterSupplier = controller::currentFilter,
            availableFiltersSupplier = controller::availableFilters,
            filterLockedReader = controller::isFilterLocked,
            selectionObserverRegistry = controller.selectionObserverRegistry(),
        )
        val synchronizer = FilmFilterSelectionSynchronizer(model, controller, reloadRequester)

        return TestSetup(
            secondFilter = secondFilter,
            zeitraumFilter = zeitraumFilter,
            controller = controller,
            model = model,
            synchronizer = synchronizer,
            reloadRequester = reloadRequester,
        )
    }

    private data class TestSetup(
        val secondFilter: FilterDTO,
        val zeitraumFilter: FilterDTO,
        val controller: FilmFilterController,
        val model: FilterSelectionComboBoxModel,
        val synchronizer: FilmFilterSelectionSynchronizer,
        val reloadRequester: RecordingReloadRequester,
    ) {
        fun close() {
            synchronizer.close()
            model.close()
        }
    }

    private class RecordingReloadRequester : FilmFilterController.ReloadRequester {
        var tableReloadRequests = 0
        var zeitraumReloadRequests = 0

        override fun requestTableReload() {
            tableReloadRequests++
        }

        override fun requestZeitraumReload() {
            zeitraumReloadRequests++
        }
    }

    private class TestFilterConfiguration(configuration: XMLConfiguration) : FilterConfiguration(configuration)
}
