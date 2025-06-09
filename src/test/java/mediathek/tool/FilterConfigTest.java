package mediathek.tool;

import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider;
import mediathek.gui.tabs.tab_film.filter.zeitraum.ZeitraumSpinnerFormatter;
import org.apache.commons.configuration2.XMLConfiguration;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static org.junit.jupiter.params.provider.Arguments.arguments;

class FilterConfigTest {

    private static Stream<Arguments> clearFilterTestSource() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        return Stream.of(
                arguments("isDontShowAbos", (Supplier<Boolean>) filterConfig::isDontShowAbos, false),
                arguments(
                        "isDontShowAudioVersions",
                        (Supplier<Boolean>) filterConfig::isDontShowAudioVersions,
                        false),
                arguments(
                        "isDontShowSignLanguage",
                        (Supplier<Boolean>) filterConfig::isDontShowSignLanguage,
                        false),
                arguments(
                        "isDontShowTrailers", (Supplier<Boolean>) filterConfig::isDontShowTrailers, false),
                arguments("isShowHdOnly", (Supplier<Boolean>) filterConfig::isShowHighQualityOnly, false),
                arguments(
                        "isShowLivestreamsOnly",
                        (Supplier<Boolean>) filterConfig::isShowLivestreamsOnly,
                        false),
                arguments("isShowNewOnly", (Supplier<Boolean>) filterConfig::isShowNewOnly, false),
                arguments(
                        "isShowSubtitlesOnly", (Supplier<Boolean>) filterConfig::isShowSubtitlesOnly, false),
                arguments("isShowUnseenOnly", (Supplier<Boolean>) filterConfig::isShowUnseenOnly, false),
                arguments("getFilmLengthMax", (Supplier<Double>) filterConfig::getFilmLengthMax, (double)FilmLengthSlider.UNLIMITED_VALUE),
                arguments("getFilmLengthMin", (Supplier<Double>) filterConfig::getFilmLengthMin, 0.0d),
                arguments(
                        "getZeitraum",
                        (Supplier<String>) filterConfig::getZeitraum,
                        ZeitraumSpinnerFormatter.INFINITE_TEXT));
    }

    @DisplayName("Check if a new filter is created when no current filter id is set and no other filters exist")
    @Test
    void getCurrentFilterId_noFiltersExist_IdOfNewlyCreatedFilter() {
        FilterConfiguration config = new FilterConfiguration(new XMLConfiguration());
        Assertions.assertNotNull(config);
        Assertions.assertFalse(config.getAvailableFilterIds().contains(config));
    }

    @DisplayName("Check if a existing filter will be set as current if no current filter is set but others exist")
    @Test
    void getCurrentFilterId_notSetFiltersExist_IdOfExistingFilter() {
        FilterConfiguration config = new FilterConfiguration(new XMLConfiguration());
        config.addNewFilter(UUID.randomUUID(), "Available bot not current filter");
        Assertions.assertTrue(config.getAvailableFilterIds().contains(config.getCurrentFilterID()));
    }

    @DisplayName("Check if clearCurrentFilter resets to correct value")
    @ParameterizedTest(name = "{index} ==> {0} expecting value {2}")
    @MethodSource("clearFilterTestSource")
    void clearCurrentFilter_clear_CurrentFilterConfigCleared(
            String configName, Supplier<?> configGetterCall, Object awaitedConfigValue) {
        Assertions.assertEquals(configGetterCall.get(), awaitedConfigValue);
    }

    @DisplayName("Check if two new filters have their correct values")
    @Test
    void addNewFilter_twoNewFilter_bothFilterAccessibleWithCorrectValues() {
        FilterConfiguration config = new FilterConfiguration(new XMLConfiguration());
        UUID firstFilterID = UUID.randomUUID();
        config.addNewFilter(firstFilterID, "First filter");

        UUID secondFilterID = UUID.randomUUID();
        config.addNewFilter(secondFilterID, "Second filter");

        config.setCurrentFilter(firstFilterID);
        config.setDontShowAbos(true);
        config.setShowNewOnly(true);
        config.setFilmLengthMax(42d);

        config.setCurrentFilter(secondFilterID);
        config.clearCurrentFilter();
        config.setFilmLengthMax(21d);
        config.setShowLivestreamsOnly(true);
        config.setZeitraum("3");

        config.setCurrentFilter(firstFilterID);

        Assertions.assertEquals(config.getCurrentFilterID(), firstFilterID);
        Assertions.assertTrue(config.isDontShowAbos());
        Assertions.assertTrue(config.isShowNewOnly());
        Assertions.assertEquals(42d, config.getFilmLengthMax());
        Assertions.assertFalse(config.isShowLivestreamsOnly());
        Assertions.assertEquals(ZeitraumSpinnerFormatter.INFINITE_TEXT, config.getZeitraum());

        config.setCurrentFilter(secondFilterID);
        Assertions.assertEquals(secondFilterID, config.getCurrentFilterID());
        Assertions.assertFalse(config.isDontShowAbos());
        Assertions.assertFalse(config.isShowNewOnly());
        Assertions.assertEquals(21d, config.getFilmLengthMax());
        Assertions.assertTrue(config.isShowLivestreamsOnly());
        Assertions.assertEquals("3", config.getZeitraum());
    }

    @DisplayName("Check if with a existing filter and a new one both have their correct values")
    @Test
    void addNewFilter_onNewFilterOneExisting_bothFilterAccessibleWithCorrectValues() {
        XMLConfiguration xmlConfiguration = new XMLConfiguration();

        UUID firstFilterID = UUID.randomUUID();
        xmlConfiguration.addProperty(
                FilterConfiguration.FILTER_PANEL_AVAILABLE_FILTERS + firstFilterID, "First test filter");
        xmlConfiguration.addProperty(
                String.format(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey(), firstFilterID), true);
        xmlConfiguration.addProperty(
                String.format(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey(), firstFilterID), true);
        xmlConfiguration.addProperty(
                String.format(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey(), firstFilterID), 42d);

        FilterConfiguration config = new FilterConfiguration(xmlConfiguration);

        UUID secondFilterID = UUID.randomUUID();
        config.addNewFilter(secondFilterID, "Second filter");

        config.setCurrentFilter(secondFilterID);
        config.clearCurrentFilter();
        config.setFilmLengthMax(21d);
        config.setShowLivestreamsOnly(true);
        config.setZeitraum("3");

        config.setCurrentFilter(firstFilterID);

        Assertions.assertEquals(firstFilterID, config.getCurrentFilterID());
        Assertions.assertTrue(config.isDontShowAbos());
        Assertions.assertTrue(config.isShowNewOnly());
        Assertions.assertEquals(42d, config.getFilmLengthMax());
        Assertions.assertFalse(config.isShowLivestreamsOnly());
        Assertions.assertEquals(ZeitraumSpinnerFormatter.INFINITE_TEXT, config.getZeitraum());

        config.setCurrentFilter(secondFilterID);
        Assertions.assertEquals(secondFilterID, config.getCurrentFilterID());
        Assertions.assertFalse(config.isDontShowAbos());
        Assertions.assertFalse(config.isShowNewOnly());
        Assertions.assertEquals(21d, config.getFilmLengthMax());
        Assertions.assertTrue(config.isShowLivestreamsOnly());
        Assertions.assertEquals("3", config.getZeitraum());
    }

    @DisplayName("Check if all old filters migrated correctly")
    @Test
    void initializeFilterConfig_migrateConfig_OldFilterConfigMigrated() {
        XMLConfiguration xmlConfiguration = new XMLConfiguration();
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey(), false);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getOldKey(), 85d);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getOldKey(), 23d);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey(), false);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey(), false);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getOldKey(), "5");

        FilterConfiguration config = new FilterConfiguration(xmlConfiguration);

        Assertions.assertNotNull(config.getCurrentFilterID());
        Assertions.assertEquals(1, config.getAvailableFilterIds().size());

        Assertions.assertTrue(config.isDontShowAbos());
        Assertions.assertTrue(config.isDontShowAudioVersions());
        Assertions.assertFalse(config.isDontShowSignLanguage());
        Assertions.assertTrue(config.isDontShowTrailers());
        Assertions.assertEquals(85d, config.getFilmLengthMax());
        Assertions.assertEquals(23d, config.getFilmLengthMin());
        Assertions.assertTrue(config.isShowHighQualityOnly());
        Assertions.assertFalse(config.isShowLivestreamsOnly());
        Assertions.assertTrue(config.isShowNewOnly());
        Assertions.assertTrue(config.isShowSubtitlesOnly());
        Assertions.assertFalse(config.isShowUnseenOnly());
        Assertions.assertEquals("5", config.getZeitraum());
    }

    @DisplayName("Check if all old filters are deleted afer migration")
    @Test
    void initializeFilterConfig_migrateConfig_OldFiltersDeleted() {
        XMLConfiguration xmlConfiguration = new XMLConfiguration();
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey(), false);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getOldKey(), 85d);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getOldKey(), 23d);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey(), false);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey(), true);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey(), false);
        xmlConfiguration.addProperty(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getOldKey(), "5");

        new FilterConfiguration(xmlConfiguration);

        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey()));
        Assertions.assertFalse(xmlConfiguration.containsKey(FilterConfiguration.FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getOldKey()));
    }

    @DisplayName("Check if filter names have correct ids")
    @Test
    void getFilterName_existingIdWithOtherFilters_correctName() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        filterConfig.addNewFilter(UUID.randomUUID(), "Filter 1");
        UUID secondFilterId = UUID.randomUUID();
        String secondFilterName = "Filter 2";
        filterConfig.addNewFilter(secondFilterId, secondFilterName);
        filterConfig.addNewFilter(UUID.randomUUID(), "Filter 3");

        Assertions.assertEquals(secondFilterName, filterConfig.getFilterName(secondFilterId));
    }

    @DisplayName("Check if list of filter names have all correct values")
    @Test
    void getAvailableFilterNames_fourNewFiltersWithNames_allCorrectNames() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        List<String> filterNames = List.of("Filter 1", "Filter 2", "Filter 3", "Test Filter 4");
        filterNames.forEach(name -> filterConfig.addNewFilter(UUID.randomUUID(), name));

        Assertions.assertEquals(filterNames.size(), filterConfig.getAvailableFilters().size());
        for (var name : filterNames) {
            Assertions.assertTrue(filterConfig.getAvailableFilterNames().contains(name));
        }
    }

    @DisplayName("Check if list of available filters have all correct values")
    @Test
    void getAvailableFilter_fourNewFilters_allCorrect() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        List<FilterDTO> filters =
                List.of(new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        new FilterDTO(UUID.randomUUID(), "Filter 2"),
                        new FilterDTO(UUID.randomUUID(), "Filter 3"),
                        new FilterDTO(UUID.randomUUID(), "Test Filter 4"));
        filters.forEach(filterConfig::addNewFilter);

        for (var filter : filters) {
            Assertions.assertTrue(filterConfig.getAvailableFilters().contains(filter));
        }

        UUID filterId = UUID.randomUUID();
        String filterName = "Other add new filter test";
        filterConfig.addNewFilter(filterId, filterName);
        Assertions.assertTrue(filterConfig.getAvailableFilters().contains(new FilterDTO(filterId, filterName)));
    }

    @DisplayName("Check if filter is removed correctly after delete by filter")
    @Test
    void deleteFilter_addThreeFiltersDeleteOneByFilter_deleteNotInConfigAnymore() {
        XMLConfiguration xmlConfiguration = new XMLConfiguration();
        FilterConfiguration filterConfig = new FilterConfiguration(xmlConfiguration);
        FilterDTO filterToDelete = new FilterDTO(UUID.randomUUID(), "Filter 2");
        List<FilterDTO> filters =
                List.of(
                        new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        filterToDelete,
                        new FilterDTO(UUID.randomUUID(), "Filter 3"));
        filters.forEach(filterConfig::addNewFilter);

        filterConfig.deleteFilter(filterToDelete);

        Assertions.assertFalse(filterConfig.getAvailableFilters().contains(filterToDelete));
        Assertions.assertFalse(filterConfig.getAvailableFilterIds().contains(filterToDelete.id()));
        Assertions.assertFalse(filterConfig.getAvailableFilterNames().contains(filterToDelete.name()));
    }

    @DisplayName("Check if all filter configs are deleted after remove of filter")
    @Test
    void deleteFilter_addThreeFiltersDeleteOneByFilter_noFilterConfigForFilterExistAnymore() {
        XMLConfiguration xmlConfiguration = new XMLConfiguration();
        FilterConfiguration filterConfig = new FilterConfiguration(xmlConfiguration);
        FilterDTO filterToDelete = new FilterDTO(UUID.randomUUID(), "Filter 2");
        List<FilterDTO> filters =
                List.of(
                        new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        filterToDelete,
                        new FilterDTO(UUID.randomUUID(), "Filter 3"));
        filters.forEach(filterConfig::addNewFilter);
        filterConfig.setCurrentFilter(filterToDelete);
        filterConfig.setShowNewOnly(true);
        filterConfig.setDontShowTrailers(true);

        filterConfig.deleteFilter(filterToDelete);

        List<String> propertyKeys = new ArrayList<>();
        xmlConfiguration.getKeys().forEachRemaining(propertyKeys::add);

        Assertions.assertTrue(propertyKeys.stream().noneMatch(key -> key.contains(filterToDelete.id().toString())),
                "Expected no configuration key to contain the filter ID");
    }

    @DisplayName("Check if filter is removed correctly after delete by filter ID")
    @Test
    void deleteFilter_addThreeFiltersDeleteOneByFilterId_deleteNotInConfigAnymore() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        UUID filterIdToDelete = UUID.randomUUID();
        FilterDTO filter2 = new FilterDTO(filterIdToDelete, "Filter 2");
        List<FilterDTO> filters =
                List.of(
                        new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        filter2,
                        new FilterDTO(UUID.randomUUID(), "Filter 3"));
        filters.forEach(filterConfig::addNewFilter);

        filterConfig.deleteFilter(filterIdToDelete);

        Assertions.assertFalse(filterConfig.getAvailableFilters().contains(filter2));
        Assertions.assertFalse(filterConfig.getAvailableFilterIds().contains(filterIdToDelete));
        Assertions.assertFalse(filterConfig.getAvailableFilterNames().contains(filter2.name()),
                "Expected available filter names to NOT contain " + filter2.name());
    }

    @DisplayName("Check if filter isn't the current filter after it's removal")
    @Test
    void deleteFilter_addThreeFiltersDeleteOneByFilter_deletedNotCurrentAnymore() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        FilterDTO filter2 = new FilterDTO(UUID.randomUUID(), "Filter 2");
        List<FilterDTO> filters =
                List.of(new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        filter2,
                        new FilterDTO(UUID.randomUUID(), "Filter 3"));
        filters.forEach(filterConfig::addNewFilter);
        filterConfig.setCurrentFilter(filter2.id());
        filterConfig.deleteFilter(filter2);

        Assertions.assertNotEquals(filterConfig.getCurrentFilter(), filter2);
    }

    @DisplayName("Check if current filter with filter DTO sets the correct filter id as current")
    @Test
    void setCurrentFilter_setCurrentFilter_currentFilterIsSet() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        FilterDTO filter2 = new FilterDTO(UUID.randomUUID(), "Filter 2");
        List<FilterDTO> filters =
                List.of(
                        new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        filter2,
                        new FilterDTO(UUID.randomUUID(), "Filter 3"));
        filters.forEach(filterConfig::addNewFilter);
        filterConfig.setCurrentFilter(filter2);
        Assertions.assertEquals(filterConfig.getCurrentFilterID(), filter2.id());
    }

    @DisplayName("Check if current filter set by ID return correct filter as current")
    @Test
    void getCurrentFilter_setCurrentFilterByIdGetCurrentFilter_correctFilterIsReturned() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        FilterDTO filter2 = new FilterDTO(UUID.randomUUID(), "Filter 2");
        List<FilterDTO> filters =
                List.of(
                        new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        filter2,
                        new FilterDTO(UUID.randomUUID(), "Filter 3"));
        filters.forEach(filterConfig::addNewFilter);
        filterConfig.setCurrentFilter(filter2.id());
        Assertions.assertEquals(filterConfig.getCurrentFilter(), filter2);
    }

    @DisplayName("Check if current filter could be renamed")
    @Test
    void renameCurrentFilter_changeCurrentFilterName_correctFilterName() {
        FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
        FilterDTO filter2 = new FilterDTO(UUID.randomUUID(), "Filter 2");
        List<FilterDTO> filters =
                List.of(
                        new FilterDTO(UUID.randomUUID(), "Filter 1"),
                        filter2,
                        new FilterDTO(UUID.randomUUID(), "Filter 3"));
        filters.forEach(filterConfig::addNewFilter);
        filterConfig.setCurrentFilter(filter2.id());
        String newName = "Second Filter";
        filterConfig.renameCurrentFilter(newName);
        Assertions.assertEquals(newName, filterConfig.getCurrentFilter().name());
    }

    @DisplayName("Check if available filter observer callback is called when filter is added")
    @Test
    void addAvailableFiltersObserver_addFilter_callbackIsCalled() {
        new FilterConfiguration(new XMLConfiguration())
                .addNewFilter(new FilterDTO(UUID.randomUUID(), "Filter 1"));

        AtomicBoolean called = new AtomicBoolean(false);
        FilterConfiguration.addAvailableFiltersObserver(() -> called.set(true));

        new FilterConfiguration(new XMLConfiguration())
                .addNewFilter(new FilterDTO(UUID.randomUUID(), "Neuer Filter"));

        Assertions.assertTrue(called.get(), "is callback called?");
    }

    @DisplayName("Check if available filter observer callback is called when filter is deleted")
    @Test
    void addAvailableFiltersObserver_removeFilter_callbackIsCalled() {
        UUID filterId = UUID.randomUUID();
        FilterConfiguration filterConfig =
                new FilterConfiguration(new XMLConfiguration())
                        .addNewFilter(new FilterDTO(filterId, "Filter 1"));
        AtomicBoolean called = new AtomicBoolean(false);
        FilterConfiguration.addAvailableFiltersObserver(() -> called.set(true));

        filterConfig.deleteFilter(filterId);

        Assertions.assertTrue(called.get(), "is callback called?");
    }

    @DisplayName("Check if available filter observer callback is called when filter is renamed")
    @Test
    void addAvailableFiltersObserver_renameFilter_callbackIsCalled() {
        UUID filterId = UUID.randomUUID();
        FilterConfiguration filterConfig =
                new FilterConfiguration(new XMLConfiguration())
                        .addNewFilter(new FilterDTO(filterId, "Filter 1"));
        AtomicBoolean called = new AtomicBoolean(false);
        FilterConfiguration.addAvailableFiltersObserver(() -> called.set(true));

        filterConfig.setCurrentFilter(filterId).renameCurrentFilter("New name");

        Assertions.assertTrue(called.get(), "is callback called?");
    }

    @DisplayName("Check if current filter observer callback is called when filter is renamed")
    @Test
    void addCurrentFiltersObserver_renameFilter_callbackIsCalledAndGotNewNamedFilter() {
        FilterDTO filterBeforeRename = new FilterDTO(UUID.randomUUID(), "Filter 1");
        FilterConfiguration filterConfig =
                new FilterConfiguration(new XMLConfiguration()).addNewFilter(filterBeforeRename);
        AtomicReference<FilterDTO> filter = new AtomicReference<>();
        FilterConfiguration.addCurrentFiltersObserver(filter::set);

        String new_name = "New name";
        filterConfig.setCurrentFilter(filterBeforeRename).renameCurrentFilter(new_name);

        Assertions.assertNotNull(filter.get(), "is callback called?");
        Assertions.assertEquals(filterBeforeRename.id(), filter.get().id());
        Assertions.assertEquals(new_name, filter.get().name());
    }

    @DisplayName("Check if current filter observer callback is called when new filter is set as current")
    @Test
    void addCurrentFiltersObserver_changeCurrentFilter_callbackIsCalledAndGotCorrectFilter() {
        FilterDTO filter1 = new FilterDTO(UUID.randomUUID(), "Filter 1");
        FilterDTO filter2 = new FilterDTO(UUID.randomUUID(), "Filter 2");
        FilterConfiguration filterConfig =
                new FilterConfiguration(new XMLConfiguration())
                        .addNewFilter(filter1)
                        .addNewFilter(filter2)
                        .setCurrentFilter(filter1);
        AtomicReference<FilterDTO> filter = new AtomicReference<>();
        FilterConfiguration.addCurrentFiltersObserver(filter::set);

        filterConfig.setCurrentFilter(filter2);

        Assertions.assertEquals(filter.get(),filter2);
    }

    @DisplayName("Check if current filter observer callback is called when current filter is deleted")
    @Test
    void addCurrentFiltersObserver_deleteCurrentFilter_callbackIsCalledAndGotCorrectFilter() {
        FilterDTO filter1 = new FilterDTO(UUID.randomUUID(), "Filter 1");
        FilterDTO filter2 = new FilterDTO(UUID.randomUUID(), "Filter 2");
        FilterConfiguration filterConfig =
                new FilterConfiguration(new XMLConfiguration())
                        .addNewFilter(filter1)
                        .addNewFilter(filter2)
                        .setCurrentFilter(filter1);
        AtomicReference<FilterDTO> filter = new AtomicReference<>();
        FilterConfiguration.addCurrentFiltersObserver(filter::set);

        filterConfig.deleteFilter(filter1);

        Assertions.assertEquals(filter.get(), filter2);
    }
}
