package mediathek.tool;

import mediathek.javafx.filterpanel.ZeitraumSpinner;
import org.apache.commons.configuration2.BaseConfiguration;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.event.ConfigurationEvent;
import org.apache.commons.configuration2.event.EventListener;

import java.util.*;
import java.util.stream.Collectors;

public class CachedFilterConfiguration extends FilterConfiguration {
  private final BaseConfiguration cachedConfiguration;

  public CachedFilterConfiguration() {
    this(ApplicationConfiguration.getConfiguration());
  }

  public CachedFilterConfiguration(Configuration configuration) {
    super(configuration);
    this.cachedConfiguration = copyConfiguration(configuration);
  }

  private BaseConfiguration copyConfiguration(Configuration sourceConfiguration) {
    BaseConfiguration copiedConfiguration = new BaseConfiguration();
    copyConfigurationValues(sourceConfiguration, copiedConfiguration);
    return copiedConfiguration;
  }

  private void copyConfigurationValues(
      Configuration sourceConfiguration, Configuration targetConfiguration) {
    sourceConfiguration
        .getKeys()
        .forEachRemaining(
            key -> targetConfiguration.setProperty(key, sourceConfiguration.getProperty(key)));
  }

  public void save() {
    copyConfigurationValues(cachedConfiguration, configuration);
  }

  public void restore() {
    copyConfigurationValues(configuration, cachedConfiguration);
  }

  public void registerEventListener(EventListener<ConfigurationEvent> eventListener)
  {
    cachedConfiguration.addEventListener(ConfigurationEvent.ANY,eventListener);
  }

  public boolean isShowHdOnly() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getKey()),
        false);
  }

  public CachedFilterConfiguration setShowHdOnly(boolean showHdOnly) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getKey()),
        showHdOnly);
    return this;
  }

  private String toFilterConfigNameWithCurrentFilter(String filterConfigNamePattern) {
    return String.format(filterConfigNamePattern, getCurrentFilterID());
  }

  public boolean isShowSubtitlesOnly() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getKey()),
        false);
  }

  public CachedFilterConfiguration setShowSubtitlesOnly(boolean showSubtitlesOnly) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getKey()),
        showSubtitlesOnly);
    return this;
  }

  public boolean isShowNewOnly() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey()),
        false);
  }

  public CachedFilterConfiguration setShowNewOnly(boolean showNewOnly) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey()),
        showNewOnly);
    return this;
  }

  public List<String> getSender() {
    return cachedConfiguration.getList(
        String.class,
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SENDER.getKey()),
        Collections.emptyList());
  }

  public CachedFilterConfiguration setSender(List<String> sender) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SENDER.getKey()),
        sender);
    return this;
  }

  public String getThema() {
    return cachedConfiguration.getString(
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_THEMA.getKey()),
        "");
  }

  public CachedFilterConfiguration setThema(String thema) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_THEMA.getKey()),
        thema);
    return this;
  }

  public boolean isShowBookMarkedOnly() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY.getKey()),
        false);
  }

  public CachedFilterConfiguration setShowBookMarkedOnly(boolean showBookMarkedOnly) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY.getKey()),
        showBookMarkedOnly);
    return this;
  }

  public boolean isShowUnseenOnly() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getKey()),
        false);
  }

  public CachedFilterConfiguration setShowUnseenOnly(boolean showUnseenOnly) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getKey()),
        showUnseenOnly);
    return this;
  }

  public boolean isShowLivestreamsOnly() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getKey()),
        false);
  }

  public CachedFilterConfiguration setShowLivestreamsOnly(boolean showLivestreamsOnly) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getKey()),
        showLivestreamsOnly);
    return this;
  }

  public boolean isDontShowAbos() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey()),
        false);
  }

  public CachedFilterConfiguration setDontShowAbos(boolean dontShowAbos) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey()),
        dontShowAbos);
    return this;
  }

  public boolean isDontShowTrailers() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getKey()),
        false);
  }

  public CachedFilterConfiguration setDontShowTrailers(boolean dontShowTrailers) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getKey()),
        dontShowTrailers);
    return this;
  }

  public boolean isDontShowSignLanguage() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getKey()),
        false);
  }

  public CachedFilterConfiguration setDontShowSignLanguage(boolean dontShowSignLanguage) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getKey()),
        dontShowSignLanguage);
    return this;
  }

  public boolean isDontShowAudioVersions() {
    return cachedConfiguration.getBoolean(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getKey()),
        false);
  }

  public CachedFilterConfiguration setDontShowAudioVersions(boolean dontShowAudioVersions) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getKey()),
        dontShowAudioVersions);
    return this;
  }

  public double getFilmLengthMin() {
    return cachedConfiguration.getDouble(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getKey()),
        0.0d);
  }

  public CachedFilterConfiguration setFilmLengthMin(double filmLengthMin) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getKey()),
        filmLengthMin);
    return this;
  }

  public double getFilmLengthMax() {
    return cachedConfiguration.getDouble(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey()),
        110.0d);
  }

  public CachedFilterConfiguration setFilmLengthMax(double filmLengthMax) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(
            FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey()),
        filmLengthMax);
    return this;
  }

  public String getZeitraum() {
    return cachedConfiguration.getString(
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getKey()),
        ZeitraumSpinner.UNLIMITED_VALUE);
  }

  public CachedFilterConfiguration setZeitraum(String zeitraum) {
    cachedConfiguration.setProperty(
        toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getKey()),
        zeitraum);
    return this;
  }

  public CachedFilterConfiguration clearCurrentFilter() {
    Arrays.stream(FilterConfigurationKeys.values())
        .map(FilterConfigurationKeys::getKey)
        .map(this::toFilterConfigNameWithCurrentFilter)
        .forEach(configuration::clearProperty);
    return this;
  }

  public UUID getCurrentFilterID() {
    return getCurrentFilter().id();
  }

  public FilterDTO getCurrentFilter() {
    if (!cachedConfiguration.containsKey(FILTER_PANEL_CURRENT_FILTER)
        || cachedConfiguration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER) == null) {
      setCurrentFilter(
          getAvailableFilters().stream()
              .findFirst()
              .orElseGet(
                  () -> {
                    FilterDTO newFilter = new FilterDTO(UUID.randomUUID(), "Filter 1");
                    addNewFilter(newFilter);
                    return newFilter;
                  }));
    }
    UUID currentFilterId = cachedConfiguration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER);
    return new FilterDTO(currentFilterId, getFilterName(currentFilterId));
  }

  public CachedFilterConfiguration setCurrentFilter(FilterDTO currentFilter) {
    return setCurrentFilter(currentFilter.id());
  }

  public CachedFilterConfiguration setCurrentFilter(UUID currentFilterID) {
    cachedConfiguration.setProperty(FILTER_PANEL_CURRENT_FILTER, currentFilterID);
    currentFilterChangedCallbacks.forEach(consumer -> consumer.accept(getCurrentFilter()));
    return this;
  }

  public List<UUID> getAvailableFilterIds() {
    return getAvailableFilters().stream()
        .map(FilterDTO::id)
        .collect(Collectors.toUnmodifiableList());
  }

  public List<String> getAvailableFilterNames() {
    return getAvailableFilters().stream()
        .map(FilterDTO::name)
        .collect(Collectors.toUnmodifiableList());
  }

  public List<FilterDTO> getAvailableFilters() {
    List<String> availableFilterKeys = new ArrayList<>();
    configuration
        .getKeys()
        .forEachRemaining(
            key -> {
              if (key.startsWith(FILTER_PANEL_AVAILABLE_FILTERS)) {
                availableFilterKeys.add(key);
              }
            });
    return availableFilterKeys.stream()
        .map(
            key ->
                new FilterDTO(
                    UUID.fromString(key.split(KEY_UUID_SPLITERATOR)[1]),
                    cachedConfiguration.getProperty(key).toString()))
        .collect(Collectors.toUnmodifiableList());
  }

  public String getFilterName(UUID id) {
    return getAvailableFilters().stream()
        .filter(filter -> filter.id().equals(id))
        .map(FilterDTO::name)
        .findFirst()
        .orElse("");
  }

  public CachedFilterConfiguration addNewFilter(FilterDTO filterDTO) {
    cachedConfiguration.addProperty(
        FILTER_PANEL_AVAILABLE_FILTERS + filterDTO.id(), filterDTO.name());
    availableFiltersChangedCallbacks.forEach(Runnable::run);
    return this;
  }

  public CachedFilterConfiguration addNewFilter(UUID filterId, String filterName) {
    return addNewFilter(new FilterDTO(filterId, filterName));
  }

  public CachedFilterConfiguration deleteFilter(FilterDTO filterToDelete) {
    return deleteFilter(filterToDelete.id());
  }

  public CachedFilterConfiguration deleteFilter(UUID idToDelete) {
    boolean filterToDeleteIsCurrentFilter = idToDelete.equals(getCurrentFilterID());
    if (filterToDeleteIsCurrentFilter) {
      cachedConfiguration.clearProperty(FILTER_PANEL_CURRENT_FILTER);
    }
    configuration
        .getKeys()
        .forEachRemaining(key -> clearPropertyWithKeyIfContainsId(idToDelete, key));
    availableFiltersChangedCallbacks.forEach(Runnable::run);
    if (filterToDeleteIsCurrentFilter) {
      currentFilterChangedCallbacks.forEach(consumer -> consumer.accept(getCurrentFilter()));
    }
    return this;
  }

  private void clearPropertyWithKeyIfContainsId(UUID idToDelete, String key) {
    if (key.contains(idToDelete.toString())) {
      cachedConfiguration.clearProperty(key);
    }
  }

  public CachedFilterConfiguration renameCurrentFilter(String newName) {
    cachedConfiguration.setProperty(FILTER_PANEL_AVAILABLE_FILTERS + getCurrentFilterID(), newName);
    availableFiltersChangedCallbacks.forEach(Runnable::run);
    currentFilterChangedCallbacks.forEach(consumer -> consumer.accept(getCurrentFilter()));
    return this;
  }
}
