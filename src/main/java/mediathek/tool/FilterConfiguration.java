package mediathek.tool;

import org.apache.commons.configuration2.Configuration;

import java.util.Collections;
import java.util.List;
import java.util.UUID;

import static mediathek.tool.ApplicationConfiguration.getConfiguration;

public class FilterConfiguration {
    protected static final String FILTER_PANEL_CURRENT_FILTER_ID = "filter.current.filter.id";
    private static final String FILTER_PANEL_AVAILABLE_FILTERS_IDS = "filter.available.filters.ids";
    private static final String FILTER_PANEL_AVAILABLE_FILTERS_NAMES =
            "filter.available.filters.names.";
    private static final String FILTER_PANEL_SHOW_HD_ONLY = "filter.%s.show.hd_only";
    private static final String FILTER_PANEL_SHOW_SUBTITLES_ONLY = "filter.%s.show.subtitles_only";
    private static final String FILTER_PANEL_SHOW_NEW_ONLY = "filter.%s.show.new_only";
    private static final String FILTER_PANEL_SHOW_UNSEEN_ONLY = "filter.%s.show.unseen_only";
    private static final String FILTER_PANEL_SHOW_LIVESTREAMS_ONLY =
            "filter.%s.show.livestreams_only";
    private static final String FILTER_PANEL_DONT_SHOW_ABOS = "filter.%s.dont_show.abos";
    private static final String FILTER_PANEL_DONT_SHOW_TRAILERS = "filter.%s.dont_show.trailers";
    private static final String FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE =
            "filter.%s.dont_show.sign_language";
    private static final String FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS =
            "filter.%s.dont_show.audio_versions";
    private static final String FILTER_PANEL_FILM_LENGTH_MIN = "filter.%s.film_length.min";
    private static final String FILTER_PANEL_FILM_LENGTH_MAX = "filter.%s.film_length.max";
    private static final String FILTER_PANEL_ZEITRAUM = "filter.%s.zeitraum";
    private final Configuration configuration;

    public FilterConfiguration() {
        this(getConfiguration());
    }

    public FilterConfiguration(Configuration configuration) {
        this.configuration = configuration;
    }

    public boolean isShowHdOnly() {
        return getConfiguration()
                .getBoolean(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_HD_ONLY), false);
    }

    public FilterConfiguration setShowHdOnly(boolean showHdOnly) {
        getConfiguration()
                .setProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_HD_ONLY), showHdOnly);
        return this;
    }

    private String toFilterConfigNameWithCurrentFilter(String filterConfigNamePattern) {
        return String.format(filterConfigNamePattern, getCurrentFilterID());
    }

    public boolean isShowSubtitlesOnly() {
        return configuration.getBoolean(
                toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_SUBTITLES_ONLY), false);
    }

    public FilterConfiguration setShowSubtitlesOnly(boolean showSubtitlesOnly) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_SUBTITLES_ONLY),
                        showSubtitlesOnly);
        return this;
    }

    public boolean isShowNewOnly() {
        return getConfiguration()
                .getBoolean(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_NEW_ONLY), false);
    }

    public FilterConfiguration setShowNewOnly(boolean showNewOnly) {
        getConfiguration()
                .setProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_NEW_ONLY), showNewOnly);
        return this;
    }

    public boolean isShowUnseenOnly() {
        return getConfiguration()
                .getBoolean(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_UNSEEN_ONLY), false);
    }

    public FilterConfiguration setShowUnseenOnly(boolean showUnseenOnly) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_UNSEEN_ONLY), showUnseenOnly);
        return this;
    }

    public boolean isShowLivestreamsOnly() {
        return getConfiguration()
                .getBoolean(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_LIVESTREAMS_ONLY), false);
    }

    public FilterConfiguration setShowLivestreamsOnly(boolean showLivestreamsOnly) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_LIVESTREAMS_ONLY),
                        showLivestreamsOnly);
        return this;
    }

    public boolean isDontShowAbos() {
        return getConfiguration()
                .getBoolean(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_ABOS), false);
    }

    public FilterConfiguration setDontShowAbos(boolean dontShowAbos) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_ABOS), dontShowAbos);
        return this;
    }

    public boolean isDontShowTrailers() {
        return getConfiguration()
                .getBoolean(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_TRAILERS), false);
    }

    public FilterConfiguration setDontShowTrailers(boolean dontShowTrailers) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_TRAILERS), dontShowTrailers);
        return this;
    }

    public boolean isDontShowSignLanguage() {
        return getConfiguration()
                .getBoolean(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE), false);
    }

    public FilterConfiguration setDontShowSignLanguage(boolean dontShowSignLanguage) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE),
                        dontShowSignLanguage);
        return this;
    }

    public boolean isDontShowAudioVersions() {
        return getConfiguration()
                .getBoolean(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS), false);
    }

    public FilterConfiguration setDontShowAudioVersions(boolean dontShowAudioVersions) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS),
                        dontShowAudioVersions);
        return this;
    }

    public double getFilmLengthMin() {
        return getConfiguration()
                .getDouble(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_FILM_LENGTH_MIN), 110.0d);
    }

    public FilterConfiguration setFilmLengthMin(int filmLengthMin) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_FILM_LENGTH_MIN), filmLengthMin);
        return this;
    }

    public double getFilmLengthMax() {
        return getConfiguration()
                .getDouble(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_FILM_LENGTH_MAX), 0.0d);
    }

    public FilterConfiguration setFilmLengthMax(int filmLengthMax) {
        getConfiguration()
                .setProperty(
                        toFilterConfigNameWithCurrentFilter(FILTER_PANEL_FILM_LENGTH_MAX), filmLengthMax);
        return this;
    }

    public String getZeitraum() {
        return getConfiguration()
                .getString(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_ZEITRAUM), "∞");
    }

    public FilterConfiguration setZeitraum(String zeitraum) {
        getConfiguration()
                .setProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_ZEITRAUM), zeitraum);
        return this;
    }

    public FilterConfiguration clearCurrentFilter() {
        configuration.clearProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_HD_ONLY));
        configuration.clearProperty(
                toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_SUBTITLES_ONLY));
        configuration.clearProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_NEW_ONLY));
        configuration.clearProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_UNSEEN_ONLY));
        configuration.clearProperty(
                toFilterConfigNameWithCurrentFilter(FILTER_PANEL_SHOW_LIVESTREAMS_ONLY));
        configuration.clearProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_ABOS));
        configuration.clearProperty(
                toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_TRAILERS));
        configuration.clearProperty(
                toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE));
        configuration.clearProperty(
                toFilterConfigNameWithCurrentFilter(FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS));
        configuration.clearProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_FILM_LENGTH_MIN));
        configuration.clearProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_FILM_LENGTH_MAX));
        configuration.clearProperty(toFilterConfigNameWithCurrentFilter(FILTER_PANEL_ZEITRAUM));
        return this;
    }

    public UUID getCurrentFilterID() {
        if (!configuration.containsKey(FILTER_PANEL_CURRENT_FILTER_ID)
                || configuration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER_ID) == null) {
            setCurrentFilterID(UUID.randomUUID());
        }
        return configuration.get(UUID.class, FILTER_PANEL_CURRENT_FILTER_ID);
    }

    public FilterConfiguration setCurrentFilterID(UUID currentFilterID) {
        configuration.setProperty(FILTER_PANEL_CURRENT_FILTER_ID, currentFilterID);
        return this;
    }

    public List<UUID> getAvailableFilterIds() {
        return Collections.unmodifiableList(
                configuration.getList(UUID.class, FILTER_PANEL_AVAILABLE_FILTERS_IDS));
    }

    public List<String> getFilterNames() {
        return Collections.unmodifiableList(
                configuration.getList(String.class, FILTER_PANEL_AVAILABLE_FILTERS_NAMES));
    }

    public FilterConfiguration addNewFilter(UUID filterId, String filterName) {
        configuration.getList(UUID.class, FILTER_PANEL_AVAILABLE_FILTERS_IDS).add(filterId);
        configuration.getList(String.class, FILTER_PANEL_AVAILABLE_FILTERS_NAMES).add(filterName);
        return this;
    }
}
