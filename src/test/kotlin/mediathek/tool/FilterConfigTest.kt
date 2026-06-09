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

package mediathek.tool

import mediathek.config.application.FilterConfiguration
import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider
import mediathek.gui.tabs.tab_film.filter.ZeitraumSpinner
import org.apache.commons.configuration2.Configuration
import org.apache.commons.configuration2.XMLConfiguration
import org.junit.jupiter.api.Assertions
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Test
import org.junit.jupiter.params.ParameterizedTest
import org.junit.jupiter.params.provider.Arguments
import org.junit.jupiter.params.provider.Arguments.arguments
import org.junit.jupiter.params.provider.MethodSource
import java.util.*
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.atomic.AtomicReference
import java.util.function.Supplier
import java.util.stream.Stream

internal class FilterConfigTest {

    @DisplayName("Check if a new filter is created when no current filter id is set and no other filters exist")
    @Test
    fun getCurrentFilterId_noFiltersExist_IdOfNewlyCreatedFilter() {
        val config = filterConfiguration()
        Assertions.assertNotNull(config)
        Assertions.assertFalse(config.availableFilterIds.any { it == config })
    }

    @DisplayName("Check if a existing filter will be set as current if no current filter is set but others exist")
    @Test
    fun getCurrentFilterId_notSetFiltersExist_IdOfExistingFilter() {
        val config = filterConfiguration()
        config.addNewFilter(UUID.randomUUID(), "Available bot not current filter")
        Assertions.assertTrue(config.availableFilterIds.contains(config.currentFilterID))
    }

    @DisplayName("Check if clearCurrentFilter resets to correct value")
    @ParameterizedTest(name = "{index} ==> {0} expecting value {2}")
    @MethodSource("clearFilterTestSource")
    fun clearCurrentFilter_clear_CurrentFilterConfigCleared(
        configName: String,
        configGetterCall: Supplier<*>,
        awaitedConfigValue: Any,
    ) {
        Assertions.assertEquals(configGetterCall.get(), awaitedConfigValue)
    }

    @DisplayName("Check if two new filters have their correct values")
    @Test
    fun addNewFilter_twoNewFilter_bothFilterAccessibleWithCorrectValues() {
        val config = filterConfiguration()
        val firstFilterID = UUID.randomUUID()
        config.addNewFilter(firstFilterID, "First filter")

        val secondFilterID = UUID.randomUUID()
        config.addNewFilter(secondFilterID, "Second filter")

        config.setCurrentFilter(firstFilterID)
        config.setDontShowAbos(true)
        config.setShowNewOnly(true)
        config.setFilmLengthMax(42.0)

        config.setCurrentFilter(secondFilterID)
        config.clearCurrentFilter()
        config.setFilmLengthMax(21.0)
        config.setShowLivestreamsOnly(true)
        config.setZeitraum("3")

        config.setCurrentFilter(firstFilterID)

        Assertions.assertEquals(config.currentFilterID, firstFilterID)
        Assertions.assertTrue(config.isDontShowAbos)
        Assertions.assertTrue(config.isShowNewOnly)
        Assertions.assertEquals(42.0, config.filmLengthMax)
        Assertions.assertFalse(config.isShowLivestreamsOnly)
        Assertions.assertEquals(ZeitraumSpinner.INFINITE_TEXT, config.zeitraum)

        config.setCurrentFilter(secondFilterID)
        Assertions.assertEquals(secondFilterID, config.currentFilterID)
        Assertions.assertFalse(config.isDontShowAbos)
        Assertions.assertFalse(config.isShowNewOnly)
        Assertions.assertEquals(21.0, config.filmLengthMax)
        Assertions.assertTrue(config.isShowLivestreamsOnly)
        Assertions.assertEquals("3", config.zeitraum)
    }

    @DisplayName("Check if with a existing filter and a new one both have their correct values")
    @Test
    fun addNewFilter_onNewFilterOneExisting_bothFilterAccessibleWithCorrectValues() {
        val xmlConfiguration = XMLConfiguration()

        val firstFilterID = UUID.randomUUID()
        xmlConfiguration.addProperty(FilterConfiguration.FILTER_PANEL_AVAILABLE_FILTERS + firstFilterID, "First test filter")
        xmlConfiguration.addProperty(
            FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.key.format(firstFilterID),
            true,
        )
        xmlConfiguration.addProperty(
            FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.key.format(firstFilterID),
            true,
        )
        xmlConfiguration.addProperty(
            FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.key.format(firstFilterID),
            42.0,
        )

        val config = filterConfiguration(xmlConfiguration)

        val secondFilterID = UUID.randomUUID()
        config.addNewFilter(secondFilterID, "Second filter")

        config.setCurrentFilter(secondFilterID)
        config.clearCurrentFilter()
        config.setFilmLengthMax(21.0)
        config.setShowLivestreamsOnly(true)
        config.setZeitraum("3")

        config.setCurrentFilter(firstFilterID)

        Assertions.assertEquals(firstFilterID, config.currentFilterID)
        Assertions.assertTrue(config.isDontShowAbos)
        Assertions.assertTrue(config.isShowNewOnly)
        Assertions.assertEquals(42.0, config.filmLengthMax)
        Assertions.assertFalse(config.isShowLivestreamsOnly)
        Assertions.assertEquals(ZeitraumSpinner.INFINITE_TEXT, config.zeitraum)

        config.setCurrentFilter(secondFilterID)
        Assertions.assertEquals(secondFilterID, config.currentFilterID)
        Assertions.assertFalse(config.isDontShowAbos)
        Assertions.assertFalse(config.isShowNewOnly)
        Assertions.assertEquals(21.0, config.filmLengthMax)
        Assertions.assertTrue(config.isShowLivestreamsOnly)
        Assertions.assertEquals("3", config.zeitraum)
    }

    @DisplayName("Check if all old filters migrated correctly")
    @Test
    fun initializeFilterConfig_migrateConfig_OldFilterConfigMigrated() {
        val xmlConfiguration = XMLConfiguration()
        addOldMigrationProperties(xmlConfiguration)

        val config = filterConfiguration(xmlConfiguration)

        Assertions.assertNotNull(config.currentFilterID)
        Assertions.assertEquals(1, config.availableFilterIds.size)

        Assertions.assertTrue(config.isDontShowAbos)
        Assertions.assertTrue(config.isDontShowAudioVersions)
        Assertions.assertFalse(config.isDontShowSignLanguage)
        Assertions.assertTrue(config.isDontShowTrailers)
        Assertions.assertEquals(85.0, config.filmLengthMax)
        Assertions.assertEquals(23.0, config.filmLengthMin)
        Assertions.assertTrue(config.isShowHighQualityOnly)
        Assertions.assertFalse(config.isShowLivestreamsOnly)
        Assertions.assertTrue(config.isShowNewOnly)
        Assertions.assertTrue(config.isShowSubtitlesOnly)
        Assertions.assertFalse(config.isShowUnseenOnly)
        Assertions.assertEquals("5", config.zeitraum)
    }

    @DisplayName("Check if all old filters are deleted afer migration")
    @Test
    fun initializeFilterConfig_migrateConfig_OldFiltersDeleted() {
        val xmlConfiguration = XMLConfiguration()
        addOldMigrationProperties(xmlConfiguration)

        filterConfiguration(xmlConfiguration)

        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.oldKey))
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.oldKey))
    }

    @DisplayName("Check if filter names have correct ids")
    @Test
    fun getFilterName_existingIdWithOtherFilters_correctName() {
        val filterConfig = filterConfiguration()
        filterConfig.addNewFilter(UUID.randomUUID(), "Filter 1")
        val secondFilterId = UUID.randomUUID()
        val secondFilterName = "Filter 2"
        filterConfig.addNewFilter(secondFilterId, secondFilterName)
        filterConfig.addNewFilter(UUID.randomUUID(), "Filter 3")

        Assertions.assertEquals(secondFilterName, filterConfig.getFilterName(secondFilterId))
    }

    @DisplayName("Check if list of filter names have all correct values")
    @Test
    fun getAvailableFilterNames_fourNewFiltersWithNames_allCorrectNames() {
        val filterConfig = filterConfiguration()
        val filterNames = listOf("Filter 1", "Filter 2", "Filter 3", "Test Filter 4")
        filterNames.forEach { filterConfig.addNewFilter(UUID.randomUUID(), it) }

        Assertions.assertEquals(filterNames.size, filterConfig.availableFilters.size)
        for (name in filterNames) {
            Assertions.assertTrue(filterConfig.availableFilterNames.contains(name))
        }
    }

    @DisplayName("Check if list of available filters have all correct values")
    @Test
    fun getAvailableFilter_fourNewFilters_allCorrect() {
        val filterConfig = filterConfiguration()
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            FilterDTO(UUID.randomUUID(), "Filter 2"),
            FilterDTO(UUID.randomUUID(), "Filter 3"),
            FilterDTO(UUID.randomUUID(), "Test Filter 4"),
        )
        filters.forEach(filterConfig::addNewFilter)

        for (filter in filters) {
            Assertions.assertTrue(filterConfig.availableFilters.contains(filter))
        }

        val filterId = UUID.randomUUID()
        val filterName = "Other add new filter test"
        filterConfig.addNewFilter(filterId, filterName)
        Assertions.assertTrue(filterConfig.availableFilters.contains(FilterDTO(filterId, filterName)))
    }

    @DisplayName("Check if reading checked channels does not normalize config by writing")
    @Test
    fun getCheckedChannels_collectionProperty_readDoesNotMutateConfiguration() {
        val xmlConfiguration = XMLConfiguration()
        val filterConfig = filterConfiguration(xmlConfiguration)
        val filterId = UUID.randomUUID()
        filterConfig.addNewFilter(filterId, "Filter 1")
        filterConfig.setCurrentFilter(filterId)

        val key = FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_CHECKED_CHANNELS.key.format(filterId)
        xmlConfiguration.setProperty(key, listOf("ARD", "3Sat"))

        Assertions.assertEquals(listOf("ARD", "3Sat"), ArrayList(filterConfig.checkedChannels))
        Assertions.assertTrue(xmlConfiguration.getProperty(key) is List<*>)
    }

    @DisplayName("Check if current filter lock state is stored per filter and survives recreation")
    @Test
    fun currentFilterLock_persistedPerFilter_survivesConfigurationReload() {
        val xmlConfiguration = XMLConfiguration()
        val filterConfig = filterConfiguration(xmlConfiguration)
        val firstFilterId = UUID.randomUUID()
        val secondFilterId = UUID.randomUUID()
        filterConfig.addNewFilter(firstFilterId, "Filter 1")
        filterConfig.addNewFilter(secondFilterId, "Filter 2")

        filterConfig.setCurrentFilter(firstFilterId)
        filterConfig.setCurrentFilterLocked(true)
        filterConfig.setCurrentFilter(secondFilterId)
        filterConfig.setCurrentFilterLocked(false)

        val reloaded = filterConfiguration(xmlConfiguration)

        reloaded.setCurrentFilter(firstFilterId)
        Assertions.assertTrue(reloaded.isCurrentFilterLocked)
        reloaded.setCurrentFilter(secondFilterId)
        Assertions.assertFalse(reloaded.isCurrentFilterLocked)
    }

    @DisplayName("Check if deleting a filter removes its persisted lock state")
    @Test
    fun deleteFilter_lockedFilterDeleted_lockPropertyRemoved() {
        val xmlConfiguration = XMLConfiguration()
        val filterConfig = filterConfiguration(xmlConfiguration)
        val filterId = UUID.randomUUID()
        filterConfig.addNewFilter(filterId, "Filter 1")
        filterConfig.setCurrentFilter(filterId)
        filterConfig.setCurrentFilterLocked(true)

        filterConfig.deleteFilter(filterId)

        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FILTER_PANEL_LOCKED.format(filterId)))
    }

    @DisplayName("Check if filter is removed correctly after delete by filter")
    @Test
    fun deleteFilter_addThreeFiltersDeleteOneByFilter_deleteNotInConfigAnymore() {
        val filterConfig = filterConfiguration(XMLConfiguration())
        val filterToDelete = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            filterToDelete,
            FilterDTO(UUID.randomUUID(), "Filter 3"),
        )
        filters.forEach(filterConfig::addNewFilter)

        filterConfig.deleteFilter(filterToDelete)

        Assertions.assertFalse(filterConfig.availableFilters.contains(filterToDelete))
        Assertions.assertFalse(filterConfig.availableFilterIds.contains(filterToDelete.id))
        Assertions.assertFalse(filterConfig.availableFilterNames.contains(filterToDelete.name))
    }

    @DisplayName("Check if all filter configs are deleted after remove of filter")
    @Test
    fun deleteFilter_addThreeFiltersDeleteOneByFilter_noFilterConfigForFilterExistAnymore() {
        val xmlConfiguration = XMLConfiguration()
        val filterConfig = filterConfiguration(xmlConfiguration)
        val filterToDelete = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            filterToDelete,
            FilterDTO(UUID.randomUUID(), "Filter 3"),
        )
        filters.forEach(filterConfig::addNewFilter)
        filterConfig.setCurrentFilter(filterToDelete)
        filterConfig.setShowNewOnly(true)
        filterConfig.setDontShowTrailers(true)

        filterConfig.deleteFilter(filterToDelete)

        val propertyKeys = ArrayList<String>()
        xmlConfiguration.keys.forEachRemaining(propertyKeys::add)

        Assertions.assertTrue(
            propertyKeys.stream().noneMatch { it.contains(filterToDelete.id.toString()) },
            "Expected no configuration key to contain the filter ID",
        )
    }

    @DisplayName("Check if filter is removed correctly after delete by filter ID")
    @Test
    fun deleteFilter_addThreeFiltersDeleteOneByFilterId_deleteNotInConfigAnymore() {
        val filterConfig = filterConfiguration()
        val filterIdToDelete = UUID.randomUUID()
        val filter2 = FilterDTO(filterIdToDelete, "Filter 2")
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            filter2,
            FilterDTO(UUID.randomUUID(), "Filter 3"),
        )
        filters.forEach(filterConfig::addNewFilter)

        filterConfig.deleteFilter(filterIdToDelete)

        Assertions.assertFalse(filterConfig.availableFilters.contains(filter2))
        Assertions.assertFalse(filterConfig.availableFilterIds.contains(filterIdToDelete))
        Assertions.assertFalse(
            filterConfig.availableFilterNames.contains(filter2.name),
            "Expected available filter names to NOT contain ${filter2.name}",
        )
    }

    @DisplayName("Check if filter isn't the current filter after it's removal")
    @Test
    fun deleteFilter_addThreeFiltersDeleteOneByFilter_deletedNotCurrentAnymore() {
        val filterConfig = filterConfiguration()
        val filter2 = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            filter2,
            FilterDTO(UUID.randomUUID(), "Filter 3"),
        )
        filters.forEach(filterConfig::addNewFilter)
        filterConfig.setCurrentFilter(filter2.id)
        filterConfig.deleteFilter(filter2)

        Assertions.assertNotEquals(filterConfig.currentFilter, filter2)
    }

    @DisplayName("Check if current filter with filter DTO sets the correct filter id as current")
    @Test
    fun setCurrentFilter_setCurrentFilter_currentFilterIsSet() {
        val filterConfig = filterConfiguration()
        val filter2 = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            filter2,
            FilterDTO(UUID.randomUUID(), "Filter 3"),
        )
        filters.forEach(filterConfig::addNewFilter)
        filterConfig.setCurrentFilter(filter2)
        Assertions.assertEquals(filterConfig.currentFilterID, filter2.id)
    }

    @DisplayName("Check if current filter set by ID return correct filter as current")
    @Test
    fun getCurrentFilter_setCurrentFilterByIdGetCurrentFilter_correctFilterIsReturned() {
        val filterConfig = filterConfiguration()
        val filter2 = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            filter2,
            FilterDTO(UUID.randomUUID(), "Filter 3"),
        )
        filters.forEach(filterConfig::addNewFilter)
        filterConfig.setCurrentFilter(filter2.id)
        Assertions.assertEquals(filterConfig.currentFilter, filter2)
    }

    @DisplayName("Check if current filter could be renamed")
    @Test
    fun renameCurrentFilter_changeCurrentFilterName_correctFilterName() {
        val filterConfig = filterConfiguration()
        val filter2 = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filters = listOf(
            FilterDTO(UUID.randomUUID(), "Filter 1"),
            filter2,
            FilterDTO(UUID.randomUUID(), "Filter 3"),
        )
        filters.forEach(filterConfig::addNewFilter)
        filterConfig.setCurrentFilter(filter2.id)
        val newName = "Second Filter"
        filterConfig.renameCurrentFilter(newName)
        Assertions.assertEquals(newName, filterConfig.currentFilter.name)
    }

    @DisplayName("Check if available filter observer callback is called when filter is added")
    @Test
    fun addAvailableFiltersObserver_addFilter_callbackIsCalled() {
        val filterConfig = filterConfiguration()
        filterConfig.addNewFilter(FilterDTO(UUID.randomUUID(), "Filter 1"))

        val called = AtomicBoolean(false)
        filterConfig.addAvailableFiltersObserver { called.set(true) }

        filterConfig.addNewFilter(FilterDTO(UUID.randomUUID(), "Neuer Filter"))

        Assertions.assertTrue(called.get(), "is callback called?")
    }

    @DisplayName("Check if available filter observer callback is called when filter is deleted")
    @Test
    fun addAvailableFiltersObserver_removeFilter_callbackIsCalled() {
        val filterId = UUID.randomUUID()
        val filterConfig = filterConfiguration()
            .addNewFilter(FilterDTO(filterId, "Filter 1"))
        val called = AtomicBoolean(false)
        filterConfig.addAvailableFiltersObserver { called.set(true) }

        filterConfig.deleteFilter(filterId)

        Assertions.assertTrue(called.get(), "is callback called?")
    }

    @DisplayName("Check if available filter observer callback is called when filter is renamed")
    @Test
    fun addAvailableFiltersObserver_renameFilter_callbackIsCalled() {
        val filterId = UUID.randomUUID()
        val filterConfig = filterConfiguration()
            .addNewFilter(FilterDTO(filterId, "Filter 1"))
        val called = AtomicBoolean(false)
        filterConfig.addAvailableFiltersObserver { called.set(true) }

        filterConfig.setCurrentFilter(filterId).renameCurrentFilter("New name")

        Assertions.assertTrue(called.get(), "is callback called?")
    }

    @DisplayName("Check if current filter observer callback is called when filter is renamed")
    @Test
    fun addCurrentFiltersObserver_renameFilter_callbackIsCalledAndGotNewNamedFilter() {
        val filterBeforeRename = FilterDTO(UUID.randomUUID(), "Filter 1")
        val filterConfig = filterConfiguration().addNewFilter(filterBeforeRename)
        val filter = AtomicReference<FilterDTO>()
        filterConfig.addCurrentFiltersObserver(filter::set)

        val newName = "New name"
        filterConfig.setCurrentFilter(filterBeforeRename).renameCurrentFilter(newName)

        Assertions.assertNotNull(filter.get(), "is callback called?")
        Assertions.assertEquals(filterBeforeRename.id, filter.get().id)
        Assertions.assertEquals(newName, filter.get().name)
    }

    @DisplayName("Check if current filter observer callback is called when new filter is set as current")
    @Test
    fun addCurrentFiltersObserver_changeCurrentFilter_callbackIsCalledAndGotCorrectFilter() {
        val filter1 = FilterDTO(UUID.randomUUID(), "Filter 1")
        val filter2 = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filterConfig = filterConfiguration()
            .addNewFilter(filter1)
            .addNewFilter(filter2)
            .setCurrentFilter(filter1)
        val filter = AtomicReference<FilterDTO>()
        filterConfig.addCurrentFiltersObserver(filter::set)

        filterConfig.setCurrentFilter(filter2)

        Assertions.assertEquals(filter.get(), filter2)
    }

    @DisplayName("Check if current filter observer callback is called when current filter is deleted")
    @Test
    fun addCurrentFiltersObserver_deleteCurrentFilter_callbackIsCalledAndGotCorrectFilter() {
        val filter1 = FilterDTO(UUID.randomUUID(), "Filter 1")
        val filter2 = FilterDTO(UUID.randomUUID(), "Filter 2")
        val filterConfig = filterConfiguration()
            .addNewFilter(filter1)
            .addNewFilter(filter2)
            .setCurrentFilter(filter1)
        val filter = AtomicReference<FilterDTO>()
        filterConfig.addCurrentFiltersObserver(filter::set)

        filterConfig.deleteFilter(filter1)

        Assertions.assertEquals(filter.get(), filter2)
    }

    private class TestFilterConfiguration(configuration: Configuration) : FilterConfiguration(configuration)

    companion object {
        private fun filterConfiguration(configuration: Configuration = XMLConfiguration()): FilterConfiguration {
            return TestFilterConfiguration(configuration)
        }

        private fun addOldMigrationProperties(xmlConfiguration: XMLConfiguration) {
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.oldKey, true)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.oldKey, true)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.oldKey, false)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.oldKey, true)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.oldKey, 85.0)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.oldKey, 23.0)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.oldKey, true)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.oldKey, false)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.oldKey, true)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.oldKey, true)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.oldKey, false)
            xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.oldKey, "5")
        }

        @JvmStatic
        private fun clearFilterTestSource(): Stream<Arguments> {
            val filterConfig = filterConfiguration()
            return Stream.of(
                arguments("isDontShowAbos", Supplier { filterConfig.isDontShowAbos }, false),
                arguments("isDontShowAudioVersions", Supplier { filterConfig.isDontShowAudioVersions }, false),
                arguments("isDontShowSignLanguage", Supplier { filterConfig.isDontShowSignLanguage }, false),
                arguments("isDontShowTrailers", Supplier { filterConfig.isDontShowTrailers }, false),
                arguments("isShowHdOnly", Supplier { filterConfig.isShowHighQualityOnly }, false),
                arguments("isShowLivestreamsOnly", Supplier { filterConfig.isShowLivestreamsOnly }, false),
                arguments("isShowNewOnly", Supplier { filterConfig.isShowNewOnly }, false),
                arguments("isShowSubtitlesOnly", Supplier { filterConfig.isShowSubtitlesOnly }, false),
                arguments("isShowUnseenOnly", Supplier { filterConfig.isShowUnseenOnly }, false),
                arguments("getFilmLengthMax", Supplier { filterConfig.filmLengthMax }, FilmLengthSlider.UNLIMITED_VALUE.toDouble()),
                arguments("getFilmLengthMin", Supplier { filterConfig.filmLengthMin }, 0.0),
                arguments("getZeitraum", Supplier { filterConfig.zeitraum }, ZeitraumSpinner.INFINITE_TEXT),
            )
        }
    }
}
