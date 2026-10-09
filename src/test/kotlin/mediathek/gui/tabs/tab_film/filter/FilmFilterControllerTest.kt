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

import mediathek.config.application.FilterConfiguration
import mediathek.tool.FilterDTO
import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Test
import java.util.*

internal class FilmFilterControllerTest {

    @Test
    fun `reconcileThema keeps stored thema while film data is not ready`() {
        val controller = createController()
        controller.restoreCurrentFilterSelection(DEFAULT_FILTER)
        controller.onThemaChanged("...von oben")
        controller.initializeFilmData(false)

        val reconciled = controller.reconcileThema(emptyList())

        assertEquals("...von oben", reconciled.thema)
        assertEquals("...von oben", controller.state().thema)
    }

    @Test
    fun `reconcileThema clears invalid thema after film data becomes ready`() {
        val controller = createController()
        controller.restoreCurrentFilterSelection(DEFAULT_FILTER)
        controller.onThemaChanged("invalid")
        controller.onFilmDataLoaded()

        val reconciled = controller.reconcileThema(listOf("...von oben"))

        assertEquals("", reconciled.thema)
        assertEquals("", controller.state().thema)
    }

    @Test
    fun `reconcileThema keeps valid thema after film data becomes ready`() {
        val controller = createController()
        controller.restoreCurrentFilterSelection(DEFAULT_FILTER)
        controller.onThemaChanged("...von oben")
        controller.onFilmDataLoaded()

        val reconciled = controller.reconcileThema(listOf("...von oben"))

        assertEquals("...von oben", reconciled.thema)
        assertEquals("...von oben", controller.state().thema)
    }

    @Test
    fun `sender selection intent updates state and requests table reload`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)

        controller.onSenderSelectionChanged(setOf("3Sat", "ARD"))

        assertEquals(setOf("3Sat", "ARD"), controller.state().checkedChannels)
        assertEquals(1, reloadRequester.tableReloadRequests)
        assertEquals(0, reloadRequester.zeitraumReloadRequests)
    }

    @Test
    fun `repeated identical sender selection is a no-op`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)

        controller.onSenderSelectionChanged(setOf("ARD"))
        controller.onSenderSelectionChanged(setOf("ARD"))

        assertEquals(setOf("ARD"), controller.state().checkedChannels)
        assertEquals(1, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `film length intent updates state and requests table reload`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)

        controller.onFilmLengthChanged(15, 90)

        assertEquals(15, controller.state().filmLengthMin)
        assertEquals(90, controller.state().filmLengthMax)
        assertEquals(1, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `film length state truncates persisted doubles and persists ints as doubles`() {
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        filterConfiguration.addNewFilter(DEFAULT_FILTER)
        filterConfiguration.setCurrentFilter(DEFAULT_FILTER)
        filterConfiguration.setFilmLengthMin(15.9)
        filterConfiguration.setFilmLengthMax(90.4)

        val controller = FilmFilterController(filterConfiguration)

        assertEquals(15, controller.state().filmLengthMin)
        assertEquals(90, controller.state().filmLengthMax)
        assertEquals(15.9, filterConfiguration.filmLengthMin)
        assertEquals(90.4, filterConfiguration.filmLengthMax)

        controller.onFilmLengthChanged(22, 77)

        assertEquals(22.0, filterConfiguration.filmLengthMin)
        assertEquals(77.0, filterConfiguration.filmLengthMax)
    }

    @Test
    fun `thema reset clears thema and requests table reload`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)
        controller.onThemaChanged("...von oben")

        controller.onThemaReset()

        assertEquals("", controller.state().thema)
        assertEquals(2, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `repeated identical thema is a no-op`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)

        controller.onThemaChanged("...von oben")
        controller.onThemaChanged("...von oben")

        assertEquals("...von oben", controller.state().thema)
        assertEquals(1, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `zeitraum intents update state and use correct reload callback`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)

        controller.onZeitraumChanged("7")
        controller.requestZeitraumReload()

        assertEquals("7", controller.state().zeitraum)
        assertEquals(0, reloadRequester.tableReloadRequests)
        assertEquals(1, reloadRequester.zeitraumReloadRequests)
    }

    @Test
    fun `checkbox intents update state without immediate reload`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)

        controller.onShowNewOnlyChanged(true)
        controller.onDontShowAbosChanged(true)
        controller.onShowSubtitlesOnlyChanged(true)

        val state = controller.state()
        assertTrue(state.showNewOnly)
        assertTrue(state.dontShowAbos)
        assertTrue(state.showSubtitlesOnly)
        assertEquals(0, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `locked checkbox changes update app state without persisting to filter configuration`() {
        val reloadRequester = RecordingReloadRequester()
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        filterConfiguration.addNewFilter(DEFAULT_FILTER)
        filterConfiguration.setCurrentFilter(DEFAULT_FILTER)
        val controller = FilmFilterController(filterConfiguration, reloadRequester = reloadRequester)
        controller.setCurrentFilterChangesLocked(true)

        controller.onShowNewOnlyChanged(true)

        assertTrue(controller.state().showNewOnly)
        assertFalse(filterConfiguration.isShowNewOnly)
        assertEquals(0, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `locked sender changes update app state and reload without persisting to filter configuration`() {
        val reloadRequester = RecordingReloadRequester()
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        filterConfiguration.addNewFilter(DEFAULT_FILTER)
        filterConfiguration.setCurrentFilter(DEFAULT_FILTER)
        val controller = FilmFilterController(filterConfiguration, reloadRequester = reloadRequester)
        controller.setCurrentFilterChangesLocked(true)

        controller.onSenderSelectionChanged(setOf("ARD"))

        assertEquals(setOf("ARD"), controller.state().checkedChannels)
        assertTrue(filterConfiguration.checkedChannels.isEmpty())
        assertEquals(1, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `locked reset restores persisted filter state without clearing configuration`() {
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        filterConfiguration.addNewFilter(DEFAULT_FILTER)
        filterConfiguration.setCurrentFilter(DEFAULT_FILTER)
        filterConfiguration.setShowNewOnly(true)
        filterConfiguration.thema = "...von oben"
        val controller = FilmFilterController(filterConfiguration)
        controller.setCurrentFilterChangesLocked(true)
        controller.onShowNewOnlyChanged(false)
        controller.onThemaChanged("temporary")

        controller.resetCurrentFilter()

        assertTrue(controller.state().showNewOnly)
        assertEquals("...von oben", controller.state().thema)
        assertTrue(filterConfiguration.isShowNewOnly)
        assertEquals("...von oben", filterConfiguration.thema)
    }

    @Test
    fun `repeated identical checkbox value is a no-op`() {
        val reloadRequester = RecordingReloadRequester()
        val controller = createController(reloadRequester)

        controller.onShowNewOnlyChanged(true)
        controller.onShowNewOnlyChanged(true)

        assertTrue(controller.state().showNewOnly)
        assertEquals(0, reloadRequester.tableReloadRequests)
    }

    @Test
    fun `add delete rename and reset filter lifecycle works`() {
        val controller = createController()

        val added = controller.addFilter("Second Filter")
        assertTrue(added is FilmFilterController.AddFilterResult.Added)
        assertEquals("Second Filter", controller.currentFilter().name)

        val renameResult = controller.renameCurrentFilter("Renamed Filter")
        assertEquals(FilmFilterController.RenameFilterResult.Renamed, renameResult)
        assertEquals("Renamed Filter", controller.currentFilter().name)

        controller.resetCurrentFilter()
        assertFalse(controller.state().showNewOnly)
        assertEquals("", controller.state().thema)

        controller.deleteCurrentFilter()
        assertEquals(1, controller.availableFilters().size)
        assertEquals(DEFAULT_FILTER.name, controller.currentFilter().name)
    }

    @Test
    fun `adding a filter resets zeitraum to unlimited`() {
        val controller = createController()
        controller.onZeitraumChanged("7")

        val added = controller.addFilter("Second Filter")

        assertTrue(added is FilmFilterController.AddFilterResult.Added)
        assertEquals(ZeitraumSpinner.INFINITE_TEXT, controller.state().zeitraum)
    }

    @Test
    fun `cloning current filter copies state and appends kopie to name`() {
        val controller = createController()
        controller.onShowNewOnlyChanged(true)
        controller.onZeitraumChanged("7")

        val cloned = controller.cloneCurrentFilter()

        assertEquals("Filter 1 Kopie", cloned.filter.name)
        assertEquals(cloned.filter, controller.currentFilter())
        assertTrue(controller.state().showNewOnly)
        assertEquals("7", controller.state().zeitraum)
        assertEquals(2, controller.availableFilters().size)
    }

    @Test
    fun `cloning original filter uses incremented kopie names when needed`() {
        val controller = createController()

        controller.cloneCurrentFilter()
        controller.restoreCurrentFilterSelection(DEFAULT_FILTER)
        val secondClone = controller.cloneCurrentFilter()

        assertEquals("Filter 1 Kopie 2", secondClone.filter.name)
    }

    @Test
    fun `restoring already selected filter is a no-op`() {
        val controller = createController()

        controller.restoreCurrentFilterSelection(DEFAULT_FILTER)

        assertEquals(DEFAULT_FILTER, controller.currentFilter())
        assertEquals(1, controller.availableFilters().size)
    }

    @Test
    fun `renaming current filter to same name is a no-op`() {
        val controller = createController()

        val renameResult = controller.renameCurrentFilter(DEFAULT_FILTER.name)

        assertEquals(FilmFilterController.RenameFilterResult.Renamed, renameResult)
        assertEquals(DEFAULT_FILTER.name, controller.currentFilter().name)
        assertEquals(1, controller.availableFilters().size)
    }

    private fun createController(
        reloadRequester: FilmFilterController.ReloadRequester = RecordingReloadRequester()
    ): FilmFilterController {
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        filterConfiguration.addNewFilter(DEFAULT_FILTER)
        filterConfiguration.setCurrentFilter(DEFAULT_FILTER)
        return FilmFilterController(filterConfiguration, reloadRequester = reloadRequester)
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

    private companion object {
        val DEFAULT_FILTER = FilterDTO(UUID.randomUUID(), "Filter 1")
    }
}
