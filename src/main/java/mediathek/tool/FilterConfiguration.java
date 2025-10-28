package mediathek.tool;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider;
import mediathek.gui.tabs.tab_film.filter.zeitraum.ZeitraumSpinnerFormatter;
import org.apache.commons.configuration2.Configuration;
import org.jetbrains.annotations.NotNull;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.*;
import java.util.concurrent.CopyOnWriteArraySet;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.Supplier;

public class FilterConfiguration {
    protected static final String FILTER_PANEL_CURRENT_FILTER = "filter.current.filter";
    protected static final String FILTER_PANEL_AVAILABLE_FILTERS = "filter.available.filters.filter_";
    private static final String KEY_UUID_SPLITERATOR = "_";
    private static final Logger LOG = LoggerFactory.getLogger(FilterConfiguration.class);
    private static final CopyOnWriteArraySet<Runnable> availableFiltersChangedCallbacks = new CopyOnWriteArraySet<>();
    private static final CopyOnWriteArraySet<Consumer<FilterDTO>> currentFilterChangedCallbacks = new CopyOnWriteArraySet<>();
    private final Configuration configuration;

    public FilterConfiguration() {
        this(ApplicationConfiguration.getConfiguration());
    }

    protected FilterConfiguration(Configuration configuration) {
        super();
        this.configuration = configuration;
        migrateOldFilterConfigurations();
    }

    public static void addAvailableFiltersObserver(Runnable availableFiltersChangedCallback) {
        availableFiltersChangedCallbacks.add(availableFiltersChangedCallback);
    }

    public static void addCurrentFiltersObserver(Consumer<FilterDTO> currentFilterChangedCallback) {
        currentFilterChangedCallbacks.add(currentFilterChangedCallback);
    }

    private void migrateOldFilterConfigurations() {
        FilterDTO newFilter = new FilterDTO(UUID.randomUUID(), "Alter Filter");
        if (migrateAll(() -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getOldKey(), newFilter, Boolean.class, this::setDontShowAbos),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getOldKey(), newFilter, Boolean.class, this::setDontShowAudioVersions),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getOldKey(), newFilter, Boolean.class, this::setDontShowSignLanguage),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getOldKey(), newFilter, Boolean.class, this::setDontShowTrailers),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getOldKey(), newFilter, Double.class, this::setFilmLengthMax),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getOldKey(), newFilter, Double.class, this::setFilmLengthMin),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getOldKey(), newFilter, Boolean.class, this::setShowHighQualityOnly),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getOldKey(), newFilter, Boolean.class, this::setShowLivestreamsOnly),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getOldKey(), newFilter, Boolean.class, this::setShowNewOnly),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY.getOldKey(), newFilter, Boolean.class, this::setShowBookMarkedOnly),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getOldKey(), newFilter, Boolean.class, this::setShowSubtitlesOnly),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getOldKey(), newFilter, Boolean.class, this::setShowUnseenOnly),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getOldKey(), newFilter, String.class, this::setZeitraum),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_CHECKED_CHANNELS.getOldKey(), newFilter, String.class, json -> setCheckedChannels(parseJsonToSet(json))),

                () -> migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_THEMA.getOldKey(), newFilter, String.class, this::setThema))) {
            addNewFilter(newFilter);
            LOG.info("Filter migration abgeschlossen.");
        }
    }

    @SafeVarargs
    private boolean migrateAll(Supplier<Boolean>... migrationSteps) {
        return !Arrays.stream(migrationSteps).map(Supplier::get).filter(Boolean::booleanValue).toList().isEmpty();
    }

    private <T> boolean migrateOldFilterConfiguration(String oldFilterConfigKey, FilterDTO newFilter, Class<T> classOfValueType, Consumer<T> newFilterSetter) {
        if (configuration.containsKey(oldFilterConfigKey)) {
            LOG.info("Alte Filter Konfiguration {} mit dem Wert {} gefunden. Migriere es zu einer neuen Filter Konfiguration mit der Filter ID {}.", oldFilterConfigKey, configuration.getString(oldFilterConfigKey), newFilter.id());
            setCurrentFilter(newFilter);
            T oldValue = configuration.get(classOfValueType, oldFilterConfigKey);
            if (oldValue == null) {
                LOG.info("Filter Konfiguration {} ist null, ignoriere Konfiguration für Migration.", oldFilterConfigKey);
            } else {
                newFilterSetter.accept(oldValue);
                configuration.clearProperty(oldFilterConfigKey);
                return true;
            }
        }
        return false;
    }

    public boolean noFiltersAreSet() {
        /*
         * If conditions are met, no filmlength filter is set.
         * return true if filtering is not needed, false if needed.
         */
        final BooleanSupplier filmLengthFilterIsNotSet = () -> {
            var filmLengthMin = (long)getFilmLengthMin();
            var filmLengthMax = (long)getFilmLengthMax();
            return filmLengthMin == 0 && filmLengthMax == FilmLengthSlider.UNLIMITED_VALUE;
        };

        return getCheckedChannels().isEmpty()
                && getThema().isEmpty()
                && filmLengthFilterIsNotSet.getAsBoolean()
                && !isDontShowAbos()
                && !isShowUnseenOnly()
                && !isShowHighQualityOnly()
                && !isShowSubtitlesOnly()
                && !isShowLivestreamsOnly()
                && !isShowNewOnly()
                && !isShowBookMarkedOnly()
                && !isDontShowTrailers()
                && !isDontShowSignLanguage()
                && !isDontShowAudioVersions()
                && !isDontShowDuplicates()
                && getZeitraum().equalsIgnoreCase(ZeitraumSpinnerFormatter.INFINITE_TEXT);
    }

    public boolean isShowHighQualityOnly() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getKey()), false);
    }

    public FilterConfiguration setShowHighQualityOnly(boolean showHdOnly) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.getKey()), showHdOnly);
        return this;
    }

    private String toFilterConfigNameWithCurrentFilter(String filterConfigNamePattern) {
        return String.format(filterConfigNamePattern, getCurrentFilterID());
    }

    public boolean isShowSubtitlesOnly() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getKey()), false);
    }

    public FilterConfiguration setShowSubtitlesOnly(boolean showSubtitlesOnly) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.getKey()), showSubtitlesOnly);
        return this;
    }

    public boolean isShowNewOnly() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey()), false);
    }

    public FilterConfiguration setShowNewOnly(boolean showNewOnly) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.getKey()), showNewOnly);
        return this;
    }

    public boolean isShowBookMarkedOnly() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY.getKey()), false);
    }

    public FilterConfiguration setShowBookMarkedOnly(boolean showBookMarkedOnly) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY.getKey()), showBookMarkedOnly);
        return this;
    }

    public boolean isShowUnseenOnly() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getKey()), false);
    }

    public FilterConfiguration setShowUnseenOnly(boolean showUnseenOnly) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.getKey()), showUnseenOnly);
        return this;
    }

    public boolean isDontShowDuplicates() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_DUPLICATES.getKey()), false);
    }

    public FilterConfiguration setDontShowDuplicates(boolean dontShowDuplicates) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_DUPLICATES.getKey()), dontShowDuplicates);
        return this;
    }

    public boolean isShowLivestreamsOnly() {

        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getKey()), false);
    }

    public FilterConfiguration setShowLivestreamsOnly(boolean showLivestreamsOnly) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.getKey()), showLivestreamsOnly);
        return this;
    }

    public boolean isDontShowAbos() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey()), false);
    }

    public FilterConfiguration setDontShowAbos(boolean dontShowAbos) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.getKey()), dontShowAbos);
        return this;
    }

    public boolean isDontShowTrailers() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getKey()), false);
    }

    public FilterConfiguration setDontShowTrailers(boolean dontShowTrailers) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.getKey()), dontShowTrailers);
        return this;
    }

    public boolean isDontShowSignLanguage() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getKey()), false);
    }

    public FilterConfiguration setDontShowSignLanguage(boolean dontShowSignLanguage) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.getKey()), dontShowSignLanguage);
        return this;
    }

    public boolean isDontShowAudioVersions() {
        return configuration.getBoolean(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getKey()), false);
    }

    public FilterConfiguration setDontShowAudioVersions(boolean dontShowAudioVersions) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.getKey()), dontShowAudioVersions);
        return this;
    }


    public double getFilmLengthMin() {
        return configuration.getDouble(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getKey()), 0.0d);
    }

    public FilterConfiguration setFilmLengthMin(double filmLengthMin) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.getKey()), filmLengthMin);
        return this;
    }

    public double getFilmLengthMax() {
        return configuration.getDouble(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey()), FilmLengthSlider.UNLIMITED_VALUE);
    }

    public FilterConfiguration setFilmLengthMax(double filmLengthMax) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.getKey()), filmLengthMax);
        return this;
    }

    public String getZeitraum() {
        return configuration.getString(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getKey()),
                ZeitraumSpinnerFormatter.INFINITE_TEXT);
    }

    public FilterConfiguration setZeitraum(@NotNull String zeitraum) {
        configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.getKey()), zeitraum);
        return this;
    }

    public Set<String> getCheckedChannels() {
        String json = configuration.getString(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_CHECKED_CHANNELS.getKey()), "[]");
        return parseJsonToSet(json);
    }

    public FilterConfiguration setCheckedChannels(@NotNull Collection<String> newList) {
        try {
            var objectMapper = new ObjectMapper();
            String json = objectMapper.writeValueAsString(newList);
            configuration.setProperty(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_CHECKED_CHANNELS.getKey()), json);
        } catch (Exception e) {
            LOG.error("Fehler beim Speichern der Checked Channels", e);
        }
        return this;
    }


    public String getThema() {
        return configuration.getString(toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_THEMA.getKey()), "");
    }

    public FilterConfiguration setThema(String thema) {
        String key = toFilterConfigNameWithCurrentFilter(FilterConfigurationKeys.FILTER_PANEL_THEMA.getKey());

        if (thema == null || thema.trim().isEmpty()) {
            configuration.clearProperty(key);
        } else {
            configuration.setProperty(key, thema);
        }
        return this;
    }


    private Set<String> parseJsonToSet(String json) {
        try {
            var objectMapper = new ObjectMapper();
            return objectMapper.readValue(json, new TypeReference<>() {
            });
        } catch (Exception e) {
            LOG.error("Fehler beim Konvertieren der alten Senderliste aus JSON", e);
            return new HashSet<>();
        }
    }


    public FilterConfiguration clearCurrentFilter() {
        Arrays.stream(FilterConfigurationKeys.values()).map(FilterConfigurationKeys::getKey).map(this::toFilterConfigNameWithCurrentFilter).forEach(configuration::clearProperty);
        return this;
    }

    public UUID getCurrentFilterID() {
        return getCurrentFilter().id();
    }

    public FilterDTO getCurrentFilter() {
        if (!configuration.containsKey(FILTER_PANEL_CURRENT_FILTER) || configuration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER) == null) {
            setCurrentFilter(getAvailableFilters().stream().findFirst().orElseGet(() -> {
                FilterDTO newFilter = new FilterDTO(UUID.randomUUID(), "Filter 1");
                addNewFilter(newFilter);
                return newFilter;
            }));
        }
        UUID currentFilterId = configuration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER);
        return new FilterDTO(currentFilterId, getFilterName(currentFilterId));
    }

    public FilterConfiguration setCurrentFilter(FilterDTO currentFilter) {
        return setCurrentFilter(currentFilter.id());
    }

    public FilterConfiguration setCurrentFilter(UUID currentFilterID) {
        configuration.setProperty(FILTER_PANEL_CURRENT_FILTER, currentFilterID);
        currentFilterChangedCallbacks.forEach(consumer -> consumer.accept(getCurrentFilter()));
        return this;
    }

    public List<UUID> getAvailableFilterIds() {
        return getAvailableFilters().stream().map(FilterDTO::id).toList();
    }

    public List<String> getAvailableFilterNames() {
        return getAvailableFilters().stream().map(FilterDTO::name).toList();
    }

    public int getAvailableFilterCount() {
        int count = 0;
        var keys = configuration.getKeys();

        while (keys.hasNext()) {
            String key = keys.next();
            if (key.startsWith(FILTER_PANEL_AVAILABLE_FILTERS)) {
                count++;
            }
        }
        return count;
    }

    public List<FilterDTO> getAvailableFilters() {
        List<String> availableFilterKeys = new ArrayList<>();
        configuration.getKeys().forEachRemaining(key -> {
            if (key.startsWith(FILTER_PANEL_AVAILABLE_FILTERS)) {
                availableFilterKeys.add(key);
            }
        });
        return availableFilterKeys.stream().map(key -> new FilterDTO(UUID.fromString(key.split(KEY_UUID_SPLITERATOR)[1]), configuration.getProperty(key).toString())).toList();
    }

    public String getFilterName(UUID id) {
        return getAvailableFilters().stream().filter(filter -> filter.id().equals(id)).map(FilterDTO::name).findFirst().orElse("");
    }

    public FilterConfiguration addNewFilter(FilterDTO filterDTO) {
        configuration.addProperty(FILTER_PANEL_AVAILABLE_FILTERS + filterDTO.id(), filterDTO.name());
        availableFiltersChangedCallbacks.forEach(Runnable::run);
        return this;
    }

    public FilterConfiguration addNewFilter(UUID filterId, String filterName) {
        return addNewFilter(new FilterDTO(filterId, filterName));
    }

    public FilterConfiguration deleteFilter(FilterDTO filterToDelete) {
        return deleteFilter(filterToDelete.id());
    }

    public FilterConfiguration deleteFilter(UUID idToDelete) {
        boolean filterToDeleteIsCurrentFilter = idToDelete.equals(getCurrentFilterID());
        if (filterToDeleteIsCurrentFilter) {
            configuration.clearProperty(FILTER_PANEL_CURRENT_FILTER);
        }
        configuration.getKeys().forEachRemaining(key -> clearPropertyWithKeyIfContainsId(idToDelete, key));
        availableFiltersChangedCallbacks.forEach(Runnable::run);
        if (filterToDeleteIsCurrentFilter) {
            currentFilterChangedCallbacks.forEach(consumer -> consumer.accept(getCurrentFilter()));
        }
        return this;
    }

    private void clearPropertyWithKeyIfContainsId(UUID idToDelete, String key) {
        if (key.contains(idToDelete.toString())) {
            configuration.clearProperty(key);
        }
    }

    public FilterConfiguration renameCurrentFilter(String newName) {
        configuration.setProperty(FILTER_PANEL_AVAILABLE_FILTERS + getCurrentFilterID(), newName);
        availableFiltersChangedCallbacks.forEach(Runnable::run);
        currentFilterChangedCallbacks.forEach(consumer -> consumer.accept(getCurrentFilter()));
        return this;
    }

    public Optional<FilterDTO> findFilterForName(String name) {
        return getAvailableFilters().stream().filter(filter -> filter.name().equals(name)).findFirst();
    }

    protected enum FilterConfigurationKeys {
        FILTER_PANEL_SHOW_HD_ONLY("filter.filter_%s.show.hd_only"),
        FILTER_PANEL_SHOW_SUBTITLES_ONLY("filter.filter_%s.show.subtitles_only"),
        FILTER_PANEL_SHOW_BOOK_MARKED_ONLY("filter.filter_%s.show.book_marked_only"),
        FILTER_PANEL_SHOW_NEW_ONLY("filter.filter_%s.show.new_only"),
        FILTER_PANEL_SHOW_UNSEEN_ONLY("filter.filter_%s.show.unseen_only"),
        FILTER_PANEL_SHOW_LIVESTREAMS_ONLY("filter.filter_%s.show.livestreams_only"),
        FILTER_PANEL_DONT_SHOW_ABOS("filter.filter_%s.dont_show.abos"),
        FILTER_PANEL_DONT_SHOW_TRAILERS("filter.filter_%s.dont_show.trailers"),
        FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE("filter.filter_%s.dont_show.sign_language"),
        FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS("filter.filter_%s.dont_show.audio_versions"),
        FILTER_PANEL_FILM_LENGTH_MIN("filter.filter_%s.film_length.min"),
        FILTER_PANEL_FILM_LENGTH_MAX("filter.filter_%s.film_length.max"),
        FILTER_PANEL_ZEITRAUM("filter.filter_%s.zeitraum"),
        FILTER_PANEL_DONT_SHOW_DUPLICATES("filter.filter_%s.dont_show_duplicates"),
        FILTER_PANEL_CHECKED_CHANNELS("filter.filter_%s.checked_channels"),
        FILTER_PANEL_THEMA("filter.filter_%s.thema");
        private final String key;

        FilterConfigurationKeys(final String key) {
            this.key = key;
        }

        public String getKey() {
            return key;
        }

        public String getOldKey() {
            return key.replace(".filter_%s", "");
        }
    }
}
