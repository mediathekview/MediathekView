package mediathek.tool;

import mediathek.javafx.filterpanel.ZeitraumSpinner;
import org.apache.commons.configuration2.Configuration;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.function.Consumer;
import java.util.stream.Collectors;

import static mediathek.tool.ApplicationConfiguration.getConfiguration;

public class FilterConfiguration {
  protected static final String FILTER_PANEL_CURRENT_FILTER_ID = "filter.current.filter";
  protected static final String FILTER_PANEL_AVAILABLE_FILTERS_IDS = "filter.available.filters";
  protected static final String FILTER_PANEL_AVAILABLE_FILTERS_FILTER_NAME =
      "filter.available.filters.%s.name";
  private static final Logger LOG = LoggerFactory.getLogger(FilterConfiguration.class);
  private final Configuration configuration;

  public FilterConfiguration() {
    this(getConfiguration());
  }

  public FilterConfiguration(Configuration configuration) {
    super();
    this.configuration = configuration;
    migrateOldFilterConfigurations();
  }

  private void migrateOldFilterConfigurations() {
    UUID newFilterId = UUID.randomUUID();
    if (migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setDontShowAbos)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setDontShowAudioVersions)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setDontShowSignLanguage)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setDontShowTrailers)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getOldKey(),
            newFilterId,
            Double.class,
            this::setFilmLengthMax)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getOldKey(),
            newFilterId,
            Double.class,
            this::setFilmLengthMin)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setShowHdOnly)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setShowLivestreamsOnly)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setShowNewOnly)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setShowSubtitlesOnly)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey(),
            newFilterId,
            Boolean.class,
            this::setShowUnseenOnly)
        | migrateOldFilterConfiguration(
            FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getOldKey(),
            newFilterId,
            String.class,
            this::setZeitraum)) {
      addNewFilter(newFilterId, "Alter Filter");
      LOG.info("Filter migration abgeschlossen.");
    }
  }

  private <T> boolean migrateOldFilterConfiguration(
      String oldFilterConfigKey,
      UUID newFilterId,
      Class<T> classOfValueType,
      Consumer<T> newFilterSetter) {
    if (configuration.containsKey(oldFilterConfigKey)) {
      LOG.info(
          "Alte Filter Konfiguration {} mit dem Wert {} gefunden. Migriere es zu einer neuen Filter Konfiguration mit der Filter ID {}.",
          oldFilterConfigKey,
          configuration.getString(oldFilterConfigKey),
          newFilterId);
      setCurrentFilter(newFilterId);
      T oldValue = configuration.get(classOfValueType, oldFilterConfigKey);
      if (oldValue == null) {
        LOG.info(
            "Filter Konfiguration {} ist null, ignoriere Konfiguration für Migration.",
            oldFilterConfigKey);
      } else {
        newFilterSetter.accept(oldValue);
        configuration.clearProperty(oldFilterConfigKey);
        return true;
      }
    }
    return false;
  }

  public boolean isShowHdOnly() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getKey()),
        false);
  }

  public FilterConfiguration setShowHdOnly(boolean showHdOnly) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getKey()),
        showHdOnly);
    return this;
  }

  private String toFilterConfigNameWithCurrentFilter(String filterConfigNamePattern) {
    return String.format(filterConfigNamePattern, getCurrentFilterID());
  }

  public boolean isShowSubtitlesOnly() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getKey()),
        false);
  }

  public FilterConfiguration setShowSubtitlesOnly(boolean showSubtitlesOnly) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getKey()),
        showSubtitlesOnly);
    return this;
  }

  public boolean isShowNewOnly() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey()),
        false);
  }

  public FilterConfiguration setShowNewOnly(boolean showNewOnly) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey()),
        showNewOnly);
    return this;
  }

  public boolean isShowUnseenOnly() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getKey()),
        false);
  }

  public FilterConfiguration setShowUnseenOnly(boolean showUnseenOnly) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getKey()),
        showUnseenOnly);
    return this;
  }

  public boolean isShowLivestreamsOnly() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getKey()),
        false);
  }

  public FilterConfiguration setShowLivestreamsOnly(boolean showLivestreamsOnly) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getKey()),
        showLivestreamsOnly);
    return this;
  }

  public boolean isDontShowAbos() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey()),
        false);
  }

  public FilterConfiguration setDontShowAbos(boolean dontShowAbos) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey()),
        dontShowAbos);
    return this;
  }

  public boolean isDontShowTrailers() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getKey()),
        false);
  }

  public FilterConfiguration setDontShowTrailers(boolean dontShowTrailers) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getKey()),
        dontShowTrailers);
    return this;
  }

  public boolean isDontShowSignLanguage() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getKey()),
        false);
  }

  public FilterConfiguration setDontShowSignLanguage(boolean dontShowSignLanguage) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getKey()),
        dontShowSignLanguage);
    return this;
  }

  public boolean isDontShowAudioVersions() {
    return configuration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getKey()),
        false);
  }

  public FilterConfiguration setDontShowAudioVersions(boolean dontShowAudioVersions) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getKey()),
        dontShowAudioVersions);
    return this;
  }

  public double getFilmLengthMin() {
    return configuration.getDouble(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getKey()),
        0.0d);
  }

  public FilterConfiguration setFilmLengthMin(double filmLengthMin) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getKey()),
        filmLengthMin);
    return this;
  }

  public double getFilmLengthMax() {
    return configuration.getDouble(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey()),
        110.0d);
  }

  public FilterConfiguration setFilmLengthMax(double filmLengthMax) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey()),
        filmLengthMax);
    return this;
  }

  public String getZeitraum() {
    return configuration.getString(
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getKey()),
        ZeitraumSpinner.UNLIMITED_VALUE);
  }

  public FilterConfiguration setZeitraum(String zeitraum) {
    configuration.setProperty(
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getKey()),
        zeitraum);
    return this;
  }

  public FilterConfiguration clearCurrentFilter() {
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey()));
    configuration.clearProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getKey()));
    return this;
  }

  public FilterDTO getCurrentFilter() {
    UUID currentFilterID = getCurrentFilterID();
    return new FilterDTO(currentFilterID, getFilterName(currentFilterID));
  }

  public FilterConfiguration setCurrentFilter(FilterDTO currentFilter) {
    return setCurrentFilter(currentFilter.id());
  }

  public FilterConfiguration setCurrentFilter(UUID currentFilterID) {
    configuration.setProperty(FILTER_PANEL_CURRENT_FILTER_ID, currentFilterID);
    return this;
  }

  public UUID getCurrentFilterID() {
    if (!configuration.containsKey(FILTER_PANEL_CURRENT_FILTER_ID)
        || configuration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER_ID) == null) {
      setCurrentFilter(
          getAvailableFilterIds().stream()
              .findFirst()
              .orElseGet(
                  () -> {
                    UUID filterId = UUID.randomUUID();
                    addNewFilter(filterId, "Filter 1");
                    return filterId;
                  }));
    }
    return configuration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER_ID);
  }

  public List<UUID> getAvailableFilterIds() {
    return Collections.unmodifiableList(
        Optional.ofNullable(configuration.getList(UUID.class, FILTER_PANEL_AVAILABLE_FILTERS_IDS))
            .orElse(Collections.emptyList()));
  }

  public List<String> getAvailableFilterNames() {
    return getAvailableFilterIds().stream()
        .map(this::getFilterName)
        .collect(Collectors.toUnmodifiableList());
  }

  public List<FilterDTO> getAvailableFilters() {
    return getAvailableFilterIds().stream()
        .map(id -> new FilterDTO(id, getFilterName(id)))
        .collect(Collectors.toUnmodifiableList());
  }

  public String getFilterName(UUID id) {
    return configuration.getString(String.format(FILTER_PANEL_AVAILABLE_FILTERS_FILTER_NAME, id));
  }

  public FilterConfiguration addNewFilter(FilterDTO filterDTO) {
    configuration.addProperty(FILTER_PANEL_AVAILABLE_FILTERS_IDS, filterDTO.id());
    configuration.addProperty(
        String.format(FILTER_PANEL_AVAILABLE_FILTERS_FILTER_NAME, filterDTO.id()),
        filterDTO.name());
    return this;
  }

  public FilterConfiguration addNewFilter(UUID filterId, String filterName) {
    return addNewFilter(new FilterDTO(filterId, filterName));
  }

  public FilterConfiguration deleteFilter(FilterDTO filterToDelete) {
    return deleteFilter(filterToDelete.id());
  }

  public FilterConfiguration deleteFilter(UUID idToDelete) {
    List<UUID> availableFilterIds = new ArrayList<>(getAvailableFilterIds());
    availableFilterIds.remove(idToDelete);
    configuration.setProperty(FILTER_PANEL_AVAILABLE_FILTERS_IDS, availableFilterIds);
    configuration.clearProperty(
        String.format(FILTER_PANEL_AVAILABLE_FILTERS_FILTER_NAME, idToDelete));
    configuration
        .getKeys()
        .forEachRemaining(key -> clearPropertyWithKeyIfContainsId(idToDelete, key));
    return this;
  }

  private void clearPropertyWithKeyIfContainsId(UUID idToDelete, String key) {
    if (key.contains(idToDelete.toString())) {
      configuration.clearProperty(key);
    }
  }

  protected enum FilterConfigurationKeys {
    FILTER_PANEL_SHOW_HD_ONLY("filter.%s.show.hd_only"),
    FILTER_PANEL_SHOW_SUBTITLES_ONLY("filter.%s.show.subtitles_only"),
    FILTER_PANEL_SHOW_NEW_ONLY("filter.%s.show.new_only"),
    FILTER_PANEL_SHOW_UNSEEN_ONLY("filter.%s.show.unseen_only"),
    FILTER_PANEL_SHOW_LIVESTREAMS_ONLY("filter.%s.show.livestreams_only"),
    FILTER_PANEL_DONT_SHOW_ABOS("filter.%s.dont_show.abos"),
    FILTER_PANEL_DONT_SHOW_TRAILERS("filter.%s.dont_show.trailers"),
    FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE("filter.%s.dont_show.sign_language"),
    FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS("filter.%s.dont_show.audio_versions"),
    FILTER_PANEL_FILM_LENGTH_MIN("filter.%s.film_length.min"),
    FILTER_PANEL_FILM_LENGTH_MAX("filter.%s.film_length.max"),
    FILTER_PANEL_ZEITRAUM("filter.%s.zeitraum");

    private final String key;

    FilterConfigurationKeys(final String key) {
      this.key = key;
    }

    public String getKey() {
      return key;
    }

    public String getOldKey() {
      return key.replace(".%s", "");
    }
  }
}
