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

package mediathek.config.application

import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider
import mediathek.gui.tabs.tab_film.filter.ZeitraumSpinner
import mediathek.tool.FilterDTO
import mediathek.tool.JsonStringUtils
import org.apache.commons.configuration2.Configuration
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.CopyOnWriteArraySet
import java.util.function.Consumer
import java.util.regex.Pattern

open class FilterConfiguration protected constructor(
    private val configuration: Configuration,
) {

    private val availableFiltersChangedCallbacks = CopyOnWriteArraySet<Runnable>()
    private val currentFilterChangedCallbacks = CopyOnWriteArraySet<Consumer<FilterDTO>>()
    private val availableFiltersCache = LinkedHashMap<UUID, FilterDTO>()
    private var availableFiltersCacheInitialized = false
    private var currentFilterIdCache: UUID? = null
    private var currentFilterCacheInitialized = false

    init {
        migrateOldFilterConfigurations()
    }

    fun addAvailableFiltersObserver(availableFiltersChangedCallback: Runnable) {
        availableFiltersChangedCallbacks.add(availableFiltersChangedCallback)
    }

    fun addCurrentFiltersObserver(currentFilterChangedCallback: Consumer<FilterDTO>) {
        currentFilterChangedCallbacks.add(currentFilterChangedCallback)
    }

    private fun migrateOldFilterConfigurations() {
        val newFilter = FilterDTO(UUID.randomUUID(), "Alter Filter")
        if (
            migrateAll(
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS.oldKey, newFilter, Boolean::class.javaObjectType) { setDontShowAbos(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_GEOBLOCKED.oldKey, newFilter, Boolean::class.javaObjectType) { setDontShowGeoblocked(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS.oldKey, newFilter, Boolean::class.javaObjectType) { setDontShowAudioVersions(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE.oldKey, newFilter, Boolean::class.javaObjectType) { setDontShowSignLanguage(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS.oldKey, newFilter, Boolean::class.javaObjectType) { setDontShowTrailers(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX.oldKey, newFilter, Double::class.javaObjectType) { setFilmLengthMax(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN.oldKey, newFilter, Double::class.javaObjectType) { setFilmLengthMin(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY.oldKey, newFilter, Boolean::class.javaObjectType) { setShowHighQualityOnly(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY.oldKey, newFilter, Boolean::class.javaObjectType) { setShowLivestreamsOnly(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY.oldKey, newFilter, Boolean::class.javaObjectType) { setShowNewOnly(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY.oldKey, newFilter, Boolean::class.javaObjectType) { setShowBookMarkedOnly(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY.oldKey, newFilter, Boolean::class.javaObjectType) { setShowSubtitlesOnly(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY.oldKey, newFilter, Boolean::class.javaObjectType) { setShowUnseenOnly(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM.oldKey, newFilter, String::class.java) { setZeitraum(it) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_CHECKED_CHANNELS.oldKey, newFilter, String::class.java) { setCheckedChannels(parseJsonToSet(it)) } },
                { migrateOldFilterConfiguration(FilterConfigurationKeys.FILTER_PANEL_THEMA.oldKey, newFilter, String::class.java) { setThema(it) } },
            )
        ) {
            addNewFilter(newFilter)
            LOG.info("Filter migration abgeschlossen.")
        }
    }

    private fun migrateAll(vararg migrationSteps: () -> Boolean): Boolean {
        return migrationSteps.map { it() }.any { it }
    }

    private fun <T : Any> migrateOldFilterConfiguration(
        oldFilterConfigKey: String,
        newFilter: FilterDTO,
        classOfValueType: Class<T>,
        newFilterSetter: (T) -> Unit,
    ): Boolean {
        if (configuration.containsKey(oldFilterConfigKey)) {
            LOG.info(
                "Alte Filter Konfiguration {} mit dem Wert {} gefunden. Migriere es zu einer neuen Filter Konfiguration mit der Filter ID {}.",
                oldFilterConfigKey,
                configuration.getString(oldFilterConfigKey),
                newFilter.id,
            )
            setCurrentFilter(newFilter)
            val oldValue = configuration[classOfValueType, oldFilterConfigKey]
            if (oldValue == null) {
                LOG.info("Filter Konfiguration {} ist null, ignoriere Konfiguration für Migration.", oldFilterConfigKey)
            } else {
                newFilterSetter(oldValue)
                configuration.clearProperty(oldFilterConfigKey)
                return true
            }
        }
        return false
    }

    val isShowHighQualityOnly: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY, false)

    fun setShowHighQualityOnly(showHdOnly: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_SHOW_HD_ONLY, showHdOnly)
        return this
    }

    private fun currentFilterConfigName(filterConfigurationKey: FilterConfigurationKeys): String {
        return toFilterConfigName(filterConfigurationKey, requireCurrentFilterId())
    }

    private fun toFilterConfigName(filterConfigurationKey: FilterConfigurationKeys, filterId: UUID): String {
        return filterConfigurationKey.key.format(filterId)
    }

    private fun toAvailableFilterKey(filterId: UUID): String = FILTER_PANEL_AVAILABLE_FILTERS + filterId

    val isShowSubtitlesOnly: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY, false)

    fun setShowSubtitlesOnly(showSubtitlesOnly: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_SHOW_SUBTITLES_ONLY, showSubtitlesOnly)
        return this
    }

    val isShowNewOnly: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY, false)

    fun setShowNewOnly(showNewOnly: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_SHOW_NEW_ONLY, showNewOnly)
        return this
    }

    val isShowBookMarkedOnly: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY, false)

    fun setShowBookMarkedOnly(showBookMarkedOnly: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_SHOW_BOOK_MARKED_ONLY, showBookMarkedOnly)
        return this
    }

    val isShowUnseenOnly: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY, false)

    fun setShowUnseenOnly(showUnseenOnly: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_SHOW_UNSEEN_ONLY, showUnseenOnly)
        return this
    }

    val isDontShowDuplicates: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_DUPLICATES, false)

    fun setDontShowDuplicates(dontShowDuplicates: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_DUPLICATES, dontShowDuplicates)
        return this
    }

    val isShowLivestreamsOnly: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY, false)

    fun setShowLivestreamsOnly(showLivestreamsOnly: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_SHOW_LIVESTREAMS_ONLY, showLivestreamsOnly)
        return this
    }

    val isDontShowAbos: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS, false)

    fun setDontShowAbos(dontShowAbos: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_ABOS, dontShowAbos)
        return this
    }

    val isDontShowTrailers: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS, false)

    fun setDontShowTrailers(dontShowTrailers: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_TRAILERS, dontShowTrailers)
        return this
    }

    val isDontShowSignLanguage: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE, false)

    fun setDontShowSignLanguage(dontShowSignLanguage: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE, dontShowSignLanguage)
        return this
    }

    val isDontShowGeoblocked: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_GEOBLOCKED, false)

    fun setDontShowGeoblocked(dontShowGeoblocked: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_GEOBLOCKED, dontShowGeoblocked)
        return this
    }

    val isDontShowAudioVersions: Boolean
        get() = getCurrentFilterBoolean(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS, false)

    fun setDontShowAudioVersions(dontShowAudioVersions: Boolean): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS, dontShowAudioVersions)
        return this
    }

    val filmLengthMin: Double
        get() = getCurrentFilterDouble(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN, 0.0)

    fun setFilmLengthMin(filmLengthMin: Double): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MIN, filmLengthMin)
        return this
    }

    val filmLengthMax: Double
        get() = getCurrentFilterDouble(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX, FilmLengthSlider.UNLIMITED_VALUE.toDouble())

    fun setFilmLengthMax(filmLengthMax: Double): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_FILM_LENGTH_MAX, filmLengthMax)
        return this
    }

    val zeitraum: String
        get() = getCurrentFilterString(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM, ZeitraumSpinner.INFINITE_TEXT)

    fun setZeitraum(zeitraum: String): FilterConfiguration {
        setCurrentFilterProperty(FilterConfigurationKeys.FILTER_PANEL_ZEITRAUM, zeitraum)
        return this
    }

    val isCurrentFilterLocked: Boolean
        get() = configuration.getBoolean(currentFilterLockConfigName(), false)

    fun isFilterLocked(filterId: UUID): Boolean = configuration.getBoolean(toFilterLockConfigName(filterId), false)

    fun setCurrentFilterLocked(locked: Boolean): FilterConfiguration {
        val key = currentFilterLockConfigName()
        if (locked) {
            configuration.setProperty(key, true)
        } else {
            configuration.clearProperty(key)
        }
        return this
    }

    val checkedChannels: Set<String>
        get() {
            val key = currentFilterConfigName(FilterConfigurationKeys.FILTER_PANEL_CHECKED_CHANNELS)
            when (val value = configuration.getProperty(key)) {
                is Collection<*> -> return value.mapNotNullTo(LinkedHashSet()) { it?.toString() }
            }

            val raw = configuration.getString(key, "")
            if (raw == null || raw.isBlank()) {
                return HashSet()
            }

            val legacyParsed = parseLegacyJsonArray(raw)
            if (legacyParsed.isNotEmpty() || raw.trim() == "[]") {
                return legacyParsed
            }

            return HashSet(Collections.singletonList(raw))
        }

    fun setCheckedChannels(newList: Collection<String>): FilterConfiguration {
        val distinctValues = LinkedHashSet(newList)
        val json = JsonStringUtils.toJsonStringArray(distinctValues)
        configuration.setProperty(currentFilterConfigName(FilterConfigurationKeys.FILTER_PANEL_CHECKED_CHANNELS), json)
        return this
    }

    var thema: String
        get() = getCurrentFilterString(FilterConfigurationKeys.FILTER_PANEL_THEMA, "")
        @JvmName("setThemaValue")
        set(value) {
            setThema(value)
        }

    fun setThema(thema: String?): FilterConfiguration {
        val key = currentFilterConfigName(FilterConfigurationKeys.FILTER_PANEL_THEMA)

        if (thema == null || thema.trim().isEmpty()) {
            configuration.clearProperty(key)
        } else {
            configuration.setProperty(key, thema)
        }
        return this
    }

    private fun parseJsonToSet(json: String): Set<String> = parseLegacyJsonArray(json)

    private fun parseLegacyJsonArray(json: String?): Set<String> {
        return try {
            val trimmed = json?.trim().orEmpty()
            if (!trimmed.startsWith("[") || !trimmed.endsWith("]")) {
                return HashSet()
            }

            val result = HashSet<String>()
            val matcher = JSON_STRING_PATTERN.matcher(trimmed)
            while (matcher.find()) {
                result.add(JsonStringUtils.unescapeJsonString(matcher.group(1)))
            }
            result
        } catch (e: Exception) {
            LOG.error("Fehler beim Konvertieren der alten Senderliste aus JSON", e)
            HashSet()
        }
    }

    fun clearCurrentFilter(): FilterConfiguration {
        val currentFilterId = requireCurrentFilterId()
        FilterConfigurationKeys.entries
            .map { toFilterConfigName(it, currentFilterId) }
            .forEach(configuration::clearProperty)
        return this
    }

    val currentFilterID: UUID
        get() = currentFilter.id

    var currentFilter: FilterDTO
        get() {
            val currentFilterId = requireCurrentFilterId()

            if (availableFiltersCache.isEmpty()) {
                return FilterDTO(currentFilterId, configuration.getString(toAvailableFilterKey(currentFilterId), ""))
            }

            val currentFilter = availableFiltersCache[currentFilterId]
            if (currentFilter == null) {
                val filter = getFirstAvailableFilter().orElseGet {
                    val newFilter = FilterDTO(UUID.randomUUID(), "Filter 1")
                    addNewFilter(newFilter)
                    newFilter
                }
                setCurrentFilter(filter)
                return filter
            }
            return currentFilter
        }
        @JvmName("setCurrentFilterValue")
        set(value) {
            setCurrentFilter(value)
        }

    fun setCurrentFilter(currentFilter: FilterDTO): FilterConfiguration = setCurrentFilter(currentFilter.id)

    fun setCurrentFilter(currentFilterID: UUID): FilterConfiguration {
        ensureAvailableFiltersCacheInitialized()
        configuration.setProperty(FILTER_PANEL_CURRENT_FILTER, currentFilterID)
        currentFilterIdCache = currentFilterID
        currentFilterCacheInitialized = true
        notifyCurrentFilterChanged(resolveCurrentFilterForNotification(currentFilterID))
        return this
    }

    val availableFilterIds: List<UUID>
        get() = availableFilters.map(FilterDTO::id)

    val availableFilterNames: List<String>
        get() = availableFilters.map(FilterDTO::name)

    val availableFilterCount: Int
        get() {
            ensureAvailableFiltersCacheInitialized()
            return availableFiltersCache.size
        }

    val availableFilters: List<FilterDTO>
        get() {
            ensureAvailableFiltersCacheInitialized()
            return availableFiltersCache.values.toList()
        }

    fun getFilterName(id: UUID): String {
        ensureAvailableFiltersCacheInitialized()
        return availableFiltersCache[id]?.name.orEmpty()
    }

    fun addNewFilter(filterDTO: FilterDTO): FilterConfiguration {
        ensureAvailableFiltersCacheInitialized()
        configuration.addProperty(toAvailableFilterKey(filterDTO.id), filterDTO.name)
        availableFiltersCache[filterDTO.id] = filterDTO
        notifyAvailableFiltersChanged()
        return this
    }

    fun addNewFilter(filterId: UUID, filterName: String): FilterConfiguration = addNewFilter(FilterDTO(filterId, filterName))

    fun deleteFilter(filterToDelete: FilterDTO): FilterConfiguration = deleteFilter(filterToDelete.id)

    fun deleteFilter(idToDelete: UUID): FilterConfiguration {
        ensureAvailableFiltersCacheInitialized()
        ensureCurrentFilterCacheInitialized()

        val filterToDeleteIsCurrentFilter = idToDelete == currentFilterIdCache
        if (filterToDeleteIsCurrentFilter) {
            configuration.clearProperty(FILTER_PANEL_CURRENT_FILTER)
            currentFilterIdCache = null
            currentFilterCacheInitialized = true
        }
        clearFilterProperties(idToDelete)
        availableFiltersCache.remove(idToDelete)
        notifyAvailableFiltersChanged()
        if (filterToDeleteIsCurrentFilter) {
            notifyCurrentFilterChanged(currentFilter)
        }
        return this
    }

    private fun clearFilterProperties(filterId: UUID) {
        configuration.clearProperty(toAvailableFilterKey(filterId))
        configuration.clearProperty(toFilterLockConfigName(filterId))
        FilterConfigurationKeys.entries
            .map { toFilterConfigName(it, filterId) }
            .forEach(configuration::clearProperty)
    }

    fun renameCurrentFilter(newName: String): FilterConfiguration {
        val currentFilterId = currentFilterID
        configuration.setProperty(toAvailableFilterKey(currentFilterId), newName)
        ensureAvailableFiltersCacheInitialized()
        availableFiltersCache[currentFilterId] = FilterDTO(currentFilterId, newName)
        notifyAvailableFiltersChanged()
        notifyCurrentFilterChanged(currentFilter)
        return this
    }

    fun findFilterForName(name: String): Optional<FilterDTO> {
        ensureAvailableFiltersCacheInitialized()
        return availableFiltersCache.values.stream().filter { it.name == name }.findFirst()
    }

    private fun ensureAvailableFiltersCacheInitialized() {
        if (availableFiltersCacheInitialized) {
            return
        }

        availableFiltersCache.clear()
        configuration.keys.asSequence()
            .filter { it.startsWith(FILTER_PANEL_AVAILABLE_FILTERS) }
            .forEach {
                val filterId = UUID.fromString(it.substring(FILTER_PANEL_AVAILABLE_FILTERS.length))
                availableFiltersCache[filterId] = FilterDTO(filterId, configuration.getProperty(it).toString())
            }
        availableFiltersCacheInitialized = true
    }

    private fun ensureCurrentFilterCacheInitialized() {
        if (currentFilterCacheInitialized) {
            return
        }

        currentFilterIdCache = configuration[UUID::class.java, FILTER_PANEL_CURRENT_FILTER, null]
        currentFilterCacheInitialized = true
    }

    private fun getFirstAvailableFilter(): Optional<FilterDTO> {
        ensureAvailableFiltersCacheInitialized()
        return availableFiltersCache.values.stream().findFirst()
    }

    private fun notifyAvailableFiltersChanged() {
        availableFiltersChangedCallbacks.forEach(Runnable::run)
    }

    private fun notifyCurrentFilterChanged(filter: FilterDTO) {
        currentFilterChangedCallbacks.forEach { it.accept(filter) }
    }

    private fun resolveCurrentFilterForNotification(currentFilterID: UUID): FilterDTO {
        val currentFilter = availableFiltersCache[currentFilterID]
        if (currentFilter != null) {
            return currentFilter
        }
        return FilterDTO(currentFilterID, configuration.getString(toAvailableFilterKey(currentFilterID), ""))
    }

    private fun getCurrentFilterBoolean(key: FilterConfigurationKeys, defaultValue: Boolean): Boolean {
        return configuration.getBoolean(currentFilterConfigName(key), defaultValue)
    }

    private fun getCurrentFilterDouble(key: FilterConfigurationKeys, defaultValue: Double): Double {
        return configuration.getDouble(currentFilterConfigName(key), defaultValue)
    }

    private fun getCurrentFilterString(key: FilterConfigurationKeys, defaultValue: String): String {
        return configuration.getString(currentFilterConfigName(key), defaultValue)
    }

    private fun setCurrentFilterProperty(key: FilterConfigurationKeys, value: Any) {
        configuration.setProperty(currentFilterConfigName(key), value)
    }

    private fun currentFilterLockConfigName(): String = toFilterLockConfigName(requireCurrentFilterId())

    private fun toFilterLockConfigName(filterId: UUID): String = FILTER_PANEL_LOCKED.format(filterId)

    private fun requireCurrentFilterId(): UUID {
        ensureAvailableFiltersCacheInitialized()
        ensureCurrentFilterCacheInitialized()

        if (currentFilterIdCache == null) {
            val filter = getFirstAvailableFilter().orElseGet {
                val newFilter = FilterDTO(UUID.randomUUID(), "Filter 1")
                addNewFilter(newFilter)
                newFilter
            }
            setCurrentFilter(filter)
        }

        return checkNotNull(currentFilterIdCache)
    }

    enum class FilterConfigurationKeys(val key: String) {
        FILTER_PANEL_SHOW_HD_ONLY("filter.filter_%s.show.hd_only"),
        FILTER_PANEL_SHOW_SUBTITLES_ONLY("filter.filter_%s.show.subtitles_only"),
        FILTER_PANEL_SHOW_BOOK_MARKED_ONLY("filter.filter_%s.show.book_marked_only"),
        FILTER_PANEL_SHOW_NEW_ONLY("filter.filter_%s.show.new_only"),
        FILTER_PANEL_SHOW_UNSEEN_ONLY("filter.filter_%s.show.unseen_only"),
        FILTER_PANEL_SHOW_LIVESTREAMS_ONLY("filter.filter_%s.show.livestreams_only"),
        FILTER_PANEL_DONT_SHOW_ABOS("filter.filter_%s.dont_show.abos"),
        FILTER_PANEL_DONT_SHOW_GEOBLOCKED("filter.filter_%s.dont_show.geoblocked"),
        FILTER_PANEL_DONT_SHOW_TRAILERS("filter.filter_%s.dont_show.trailers"),
        FILTER_PANEL_DONT_SHOW_SIGN_LANGUAGE("filter.filter_%s.dont_show.sign_language"),
        FILTER_PANEL_DONT_SHOW_AUDIO_VERSIONS("filter.filter_%s.dont_show.audio_versions"),
        FILTER_PANEL_FILM_LENGTH_MIN("filter.filter_%s.film_length.min"),
        FILTER_PANEL_FILM_LENGTH_MAX("filter.filter_%s.film_length.max"),
        FILTER_PANEL_ZEITRAUM("filter.filter_%s.zeitraum"),
        FILTER_PANEL_DONT_SHOW_DUPLICATES("filter.filter_%s.dont_show_duplicates"),
        FILTER_PANEL_CHECKED_CHANNELS("filter.filter_%s.checked_channels"),
        FILTER_PANEL_THEMA("filter.filter_%s.thema");

        val oldKey: String
            get() = key.replace(".filter_%s", "")
    }

    companion object {
        const val FILTER_PANEL_CURRENT_FILTER = "filter.current.filter"
        const val FILTER_PANEL_AVAILABLE_FILTERS = "filter.available.filters.filter_"
        const val FILTER_PANEL_LOCKED = "filter.filter_%s.locked"
        private val JSON_STRING_PATTERN = Pattern.compile("\"((?:\\\\.|[^\"])*)\"")
        private val LOG = LogManager.getLogger(FilterConfiguration::class.java)
    }
}
