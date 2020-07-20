package mediathek.tool;

import mediathek.javafx.filterpanel.ZeitraumSpinner;
import org.apache.commons.configuration2.XMLConfiguration;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static mediathek.tool.FilterConfiguration.FILTER_PANEL_AVAILABLE_FILTERS;
import static mediathek.tool.FilterConfiguration.FilterConfigurationKeys.*;
import static org.assertj.core.api.Assertions.assertThat;
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
        arguments("isShowHdOnly", (Supplier<Boolean>) filterConfig::isShowHdOnly, false),
        arguments(
            "isShowLivestreamsOnly",
            (Supplier<Boolean>) filterConfig::isShowLivestreamsOnly,
            false),
        arguments("isShowNewOnly", (Supplier<Boolean>) filterConfig::isShowNewOnly, false),
        arguments(
            "isShowSubtitlesOnly", (Supplier<Boolean>) filterConfig::isShowSubtitlesOnly, false),
        arguments("isShowUnseenOnly", (Supplier<Boolean>) filterConfig::isShowUnseenOnly, false),
        arguments("getFilmLengthMax", (Supplier<Double>) filterConfig::getFilmLengthMax, 110.0d),
        arguments("getFilmLengthMin", (Supplier<Double>) filterConfig::getFilmLengthMin, 0.0d),
        arguments(
            "getSender", (Supplier<List<String>>) filterConfig::getSender, Collections.emptyList()),
        arguments("getThemen", (Supplier<String>) filterConfig::getThema, ""),
        arguments(
            "getZeitraum",
            (Supplier<String>) filterConfig::getZeitraum,
            ZeitraumSpinner.UNLIMITED_VALUE));
  }

  @DisplayName(
      "Check if a new filter is created when no current filter id is set and no other filters exist")
  @Test
  void getCurrentFilterId_noFiltersExist_IdOfNewlyCreatedFilter() {
    FilterConfiguration config = new FilterConfiguration(new XMLConfiguration());
    assertThat(config).isNotNull().isNotIn(config.getAvailableFilterIds());
  }

  @DisplayName(
      "Check if a existing filter will be set as current if no current filter is set but others exist")
  @Test
  void getCurrentFilterId_notSetFiltersExist_IdOfExistingFilter() {
    FilterConfiguration config = new FilterConfiguration(new XMLConfiguration());
    config.addNewFilter(UUID.randomUUID(), "Available bot not current filter");
    assertThat(config.getCurrentFilterID()).isIn(config.getAvailableFilterIds());
  }

  @DisplayName("Check if clearCurrentFilter resets to correct value")
  @ParameterizedTest(name = "{index} ==> {0} expecting value {2}")
  @MethodSource("clearFilterTestSource")
  void clearCurrentFilter_clear_CurrentFilterConfigCleared(
      String configName, Supplier<?> configGetterCall, Object awaitedConfigValue) {
    assertThat(configGetterCall.get()).isEqualTo(awaitedConfigValue);
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
    config.setThema("Tagesschau");
    config.setShowNewOnly(true);
    config.setFilmLengthMax(42d);

    config.setCurrentFilter(secondFilterID);
    config.clearCurrentFilter();
    config.setFilmLengthMax(21d);
    config.setSender(List.of("DW"));
    config.setShowLivestreamsOnly(true);
    config.setZeitraum("3");

    config.setCurrentFilter(firstFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(firstFilterID);
    assertThat(config.isDontShowAbos()).isTrue();
    assertThat(config.getThema()).isEqualTo("Tagesschau");
    assertThat(config.isShowNewOnly()).isTrue();
    assertThat(config.getFilmLengthMax()).isEqualTo(42d);
    assertThat(config.isShowLivestreamsOnly()).isFalse();
    assertThat(config.getZeitraum()).isEqualTo(ZeitraumSpinner.UNLIMITED_VALUE);

    config.setCurrentFilter(secondFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(secondFilterID);
    assertThat(config.isDontShowAbos()).isFalse();
    assertThat(config.getSender()).containsExactly("DW");
    assertThat(config.isShowNewOnly()).isFalse();
    assertThat(config.getFilmLengthMax()).isEqualTo(21d);
    assertThat(config.isShowLivestreamsOnly()).isTrue();
    assertThat(config.getZeitraum()).isEqualTo("3");
  }

  @DisplayName("Check if with a existing filter and a new one both have their correct values")
  @Test
  void addNewFilter_onNewFilterOneExisting_bothFilterAccessibleWithCorrectValues() {
    XMLConfiguration xmlConfiguration = new XMLConfiguration();

    UUID firstFilterID = UUID.randomUUID();
    xmlConfiguration.addProperty(
        FILTER_PANEL_AVAILABLE_FILTERS + firstFilterID, "First test filter");
    xmlConfiguration.addProperty(
        String.format(FILTER_PANEL_DONT_SHOW_ABOS.getKey(), firstFilterID), true);
    xmlConfiguration.addProperty(
        String.format(FILTER_PANEL_SHOW_NEW_ONLY.getKey(), firstFilterID), true);
    xmlConfiguration.addProperty(
        String.format(FILTER_PANEL_FILM_LENGTH_MAX.getKey(), firstFilterID), 42d);

    FilterConfiguration config = new FilterConfiguration(xmlConfiguration);

    UUID secondFilterID = UUID.randomUUID();
    config.addNewFilter(secondFilterID, "Second filter");

    config.setCurrentFilter(secondFilterID);
    config.clearCurrentFilter();
    config.setFilmLengthMax(21d);
    config.setSender(List.of("ARD", "BR"));
    config.setThema("Tagesschau");
    config.setShowLivestreamsOnly(true);
    config.setZeitraum("3");

    config.setCurrentFilter(firstFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(firstFilterID);
    assertThat(config.isDontShowAbos()).isTrue();
    assertThat(config.isShowNewOnly()).isTrue();
    assertThat(config.getFilmLengthMax()).isEqualTo(42d);
    assertThat(config.isShowLivestreamsOnly()).isFalse();
    assertThat(config.getZeitraum()).isEqualTo(ZeitraumSpinner.UNLIMITED_VALUE);

    config.setCurrentFilter(secondFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(secondFilterID);
    assertThat(config.isDontShowAbos()).isFalse();
    assertThat(config.isShowNewOnly()).isFalse();
    assertThat(config.getFilmLengthMax()).isEqualTo(21d);
    assertThat(config.getSender()).containsExactly("ARD", "BR");
    assertThat(config.getThema()).isEqualTo("Tagesschau");
    assertThat(config.isShowLivestreamsOnly()).isTrue();
    assertThat(config.getZeitraum()).isEqualTo("3");
  }

  @DisplayName("Check if all old filters migrated correctly")
  @Test
  void initializeFilterConfig_migrateConfig_OldFilterConfigMigrated() {
    XMLConfiguration xmlConfiguration = new XMLConfiguration();
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_ABOS.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey(), false);
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_FILM_LENGTH_MAX.getOldKey(), 85d);
    xmlConfiguration.addProperty(FILTER_PANEL_FILM_LENGTH_MIN.getOldKey(), 23d);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_HD_ONLY.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey(), false);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_NEW_ONLY.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey(), false);
    xmlConfiguration.addProperty(FILTER_PANEL_ZEITRAUM.getOldKey(), "5");

    FilterConfiguration config = new FilterConfiguration(xmlConfiguration);

    assertThat(config.getCurrentFilterID()).isNotNull();
    assertThat(config.getAvailableFilterIds()).hasSize(1);

    assertThat(config.isDontShowAbos()).isTrue();
    assertThat(config.isDontShowAudioVersions()).isTrue();
    assertThat(config.isDontShowSignLanguage()).isFalse();
    assertThat(config.isDontShowTrailers()).isTrue();
    assertThat(config.getFilmLengthMax()).isEqualTo(85d);
    assertThat(config.getFilmLengthMin()).isEqualTo(23d);
    assertThat(config.isShowHdOnly()).isTrue();
    assertThat(config.isShowLivestreamsOnly()).isFalse();
    assertThat(config.isShowNewOnly()).isTrue();
    assertThat(config.isShowSubtitlesOnly()).isTrue();
    assertThat(config.isShowUnseenOnly()).isFalse();
    assertThat(config.getZeitraum()).isEqualTo("5");
  }

  @DisplayName("Check if all old filters are deleted afer migration")
  @Test
  void initializeFilterConfig_migrateConfig_OldFiltersDeleted() {
    XMLConfiguration xmlConfiguration = new XMLConfiguration();
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_ABOS.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey(), false);
    xmlConfiguration.addProperty(FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_FILM_LENGTH_MAX.getOldKey(), 85d);
    xmlConfiguration.addProperty(FILTER_PANEL_FILM_LENGTH_MIN.getOldKey(), 23d);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_HD_ONLY.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey(), false);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_NEW_ONLY.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey(), true);
    xmlConfiguration.addProperty(FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey(), false);
    xmlConfiguration.addProperty(FILTER_PANEL_ZEITRAUM.getOldKey(), "5");

    new FilterConfiguration(xmlConfiguration);

    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_DONT_SHOW_ABOS.getOldKey())).isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey()))
        .isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey()))
        .isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey())).isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_FILM_LENGTH_MAX.getOldKey())).isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_FILM_LENGTH_MIN.getOldKey())).isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_SHOW_HD_ONLY.getOldKey())).isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey()))
        .isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_SHOW_NEW_ONLY.getOldKey())).isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey()))
        .isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey())).isFalse();
    assertThat(xmlConfiguration.containsKey(FILTER_PANEL_ZEITRAUM.getOldKey())).isFalse();
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

    assertThat(filterConfig.getFilterName(secondFilterId)).isEqualTo(secondFilterName);
  }

  @DisplayName("Check if list of filter names have all correct values")
  @Test
  void getAvailableFilterNames_fourNewFiltersWithNames_allCorrectNames() {
    FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
    List<String> filterNames = List.of("Filter 1", "Filter 2", "Filter 3", "Test Filter 4");
    filterNames.forEach(name -> filterConfig.addNewFilter(UUID.randomUUID(), name));

    assertThat(filterConfig.getAvailableFilterNames())
        .containsExactlyInAnyOrderElementsOf(filterNames);
  }

  @DisplayName("Check if list of available filters have all correct values")
  @Test
  void getAvailableFilter_fourNewFilters_allCorrect() {
    FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
    List<FilterDTO> filters =
        List.of(
            new FilterDTO(UUID.randomUUID(), "Filter 1"),
            new FilterDTO(UUID.randomUUID(), "Filter 2"),
            new FilterDTO(UUID.randomUUID(), "Filter 3"),
            new FilterDTO(UUID.randomUUID(), "Test Filter 4"));
    filters.forEach(filterConfig::addNewFilter);

    assertThat(filterConfig.getAvailableFilters()).containsExactlyInAnyOrderElementsOf(filters);

    UUID filterId = UUID.randomUUID();
    String filterName = "Other add new filter test";
    filterConfig.addNewFilter(filterId, filterName);
    assertThat(filterConfig.getAvailableFilters()).contains(new FilterDTO(filterId, filterName));
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

    assertThat(filterConfig.getAvailableFilters()).doesNotContain(filterToDelete);
    assertThat(filterConfig.getAvailableFilterIds()).doesNotContain(filterToDelete.id());
    assertThat(filterConfig.getAvailableFilterNames()).doesNotContain(filterToDelete.name());
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

    List<String> propertyKes = new ArrayList<>();
    xmlConfiguration.getKeys().forEachRemaining(propertyKes::add);

    assertThat(propertyKes).noneMatch(key -> key.contains(filterToDelete.id().toString()));
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

    assertThat(filterConfig.getAvailableFilters()).doesNotContain(filter2);
    assertThat(filterConfig.getAvailableFilterIds()).doesNotContain(filterIdToDelete);
    assertThat(filterConfig.getAvailableFilterNames()).doesNotContain(filter2.name());
  }

  @DisplayName("Check if filter isn't the current filter after it's removal")
  @Test
  void deleteFilter_addThreeFiltersDeleteOneByFilter_deletedNotCurrentAnymore() {
    FilterConfiguration filterConfig = new FilterConfiguration(new XMLConfiguration());
    FilterDTO filter2 = new FilterDTO(UUID.randomUUID(), "Filter 2");
    List<FilterDTO> filters =
        List.of(
            new FilterDTO(UUID.randomUUID(), "Filter 1"),
            filter2,
            new FilterDTO(UUID.randomUUID(), "Filter 3"));
    filters.forEach(filterConfig::addNewFilter);
    filterConfig.setCurrentFilter(filter2.id());
    filterConfig.deleteFilter(filter2);

    assertThat(filterConfig.getCurrentFilter()).isNotEqualTo(filter2);
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
    assertThat(filterConfig.getCurrentFilterID()).isEqualTo(filter2.id());
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
    assertThat(filterConfig.getCurrentFilter()).isEqualTo(filter2);
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
    assertThat(filterConfig.getCurrentFilter().name()).isEqualTo(newName);
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

    assertThat(called.get()).describedAs("is callback called?").isTrue();
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

    assertThat(called.get()).describedAs("is callback called?").isTrue();
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

    assertThat(called.get()).describedAs("is callback called?").isTrue();
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

    assertThat(filter.get()).describedAs("is callback called?").isNotNull();
    assertThat(filter.get().id()).isEqualTo(filterBeforeRename.id());
    assertThat(filter.get().name()).isEqualTo(new_name);
  }

  @DisplayName(
      "Check if current filter observer callback is called when new filter is set as current")
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

    assertThat(filter.get()).isEqualTo(filter2);
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

    assertThat(filter.get()).isEqualTo(filter2);
  }
}
