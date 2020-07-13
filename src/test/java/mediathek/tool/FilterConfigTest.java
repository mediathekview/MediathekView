package mediathek.tool;

import mediathek.javafx.filterpanel.ZeitraumSpinner;
import org.apache.commons.configuration2.XMLConfiguration;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.UUID;
import java.util.function.Supplier;
import java.util.stream.Stream;

import static mediathek.tool.FilterConfiguration.FILTER_PANEL_AVAILABLE_FILTERS_IDS;
import static mediathek.tool.FilterConfiguration.FILTER_PANEL_AVAILABLE_FILTERS_NAMES;
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

    config.setCurrentFilterID(firstFilterID);
    config.setDontShowAbos(true);
    config.setShowNewOnly(true);
    config.setFilmLengthMax(42d);

    config.setCurrentFilterID(secondFilterID);
    config.clearCurrentFilter();
    config.setFilmLengthMax(21d);
    config.setShowLivestreamsOnly(true);
    config.setZeitraum("3");

    config.setCurrentFilterID(firstFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(firstFilterID);
    assertThat(config.isDontShowAbos()).isTrue();
    assertThat(config.isShowNewOnly()).isTrue();
    assertThat(config.getFilmLengthMax()).isEqualTo(42d);
    assertThat(config.isShowLivestreamsOnly()).isFalse();
    assertThat(config.getZeitraum()).isEqualTo(ZeitraumSpinner.UNLIMITED_VALUE);

    config.setCurrentFilterID(secondFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(secondFilterID);
    assertThat(config.isDontShowAbos()).isFalse();
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
    xmlConfiguration.addProperty(FILTER_PANEL_AVAILABLE_FILTERS_IDS, firstFilterID);
    xmlConfiguration.addProperty(FILTER_PANEL_AVAILABLE_FILTERS_NAMES, "First test filter");
    xmlConfiguration.addProperty(
        String.format(FILTER_PANEL_DONT_SHOW_ABOS.getKey(), firstFilterID), true);
    xmlConfiguration.addProperty(
        String.format(FILTER_PANEL_SHOW_NEW_ONLY.getKey(), firstFilterID), true);
    xmlConfiguration.addProperty(
        String.format(FILTER_PANEL_FILM_LENGTH_MAX.getKey(), firstFilterID), 42d);

    FilterConfiguration config = new FilterConfiguration(xmlConfiguration);

    UUID secondFilterID = UUID.randomUUID();
    config.addNewFilter(secondFilterID, "Second filter");

    config.setCurrentFilterID(secondFilterID);
    config.clearCurrentFilter();
    config.setFilmLengthMax(21d);
    config.setShowLivestreamsOnly(true);
    config.setZeitraum("3");

    config.setCurrentFilterID(firstFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(firstFilterID);
    assertThat(config.isDontShowAbos()).isTrue();
    assertThat(config.isShowNewOnly()).isTrue();
    assertThat(config.getFilmLengthMax()).isEqualTo(42d);
    assertThat(config.isShowLivestreamsOnly()).isFalse();
    assertThat(config.getZeitraum()).isEqualTo(ZeitraumSpinner.UNLIMITED_VALUE);

    config.setCurrentFilterID(secondFilterID);
    assertThat(config.getCurrentFilterID()).isEqualTo(secondFilterID);
    assertThat(config.isDontShowAbos()).isFalse();
    assertThat(config.isShowNewOnly()).isFalse();
    assertThat(config.getFilmLengthMax()).isEqualTo(21d);
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
}
