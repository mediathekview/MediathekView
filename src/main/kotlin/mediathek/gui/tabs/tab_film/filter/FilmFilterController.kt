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

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import mediathek.tool.FilterConfiguration
import mediathek.tool.FilterDTO
import org.apache.logging.log4j.LogManager
import java.util.*
import java.util.concurrent.CopyOnWriteArraySet
import java.util.function.Consumer

class FilmFilterController(
    private val filterConfig: FilterConfiguration,
    private val dataProvider: DataProvider = NoOpDataProvider,
    private val reloadRequester: ReloadRequester = NoOpReloadRequester
) {
    private var currentState: FilmFilterState = FilmFilterState.from(filterConfig)
    private var filmDataState: FilmDataState = FilmDataState.Loading
    private val availableFiltersObservers = CopyOnWriteArraySet<Runnable>()
    private val currentFilterObservers = CopyOnWriteArraySet<Consumer<FilterDTO>>()
    private val selectionObserverRegistry = object : SelectionObserverRegistry {
        override fun addAvailableFiltersObserver(observer: Runnable) {
            availableFiltersObservers.add(observer)
        }

        override fun removeAvailableFiltersObserver(observer: Runnable) {
            availableFiltersObservers.remove(observer)
        }

        override fun addCurrentFilterObserver(observer: Consumer<FilterDTO>) {
            currentFilterObservers.add(observer)
        }

        override fun removeCurrentFilterObserver(observer: Consumer<FilterDTO>) {
            currentFilterObservers.remove(observer)
        }
    }

    companion object {
        private const val COPY_SUFFIX = " Kopie"
        private val logger = LogManager.getLogger()
    }

    interface DataProvider {
        fun senderList(): EventList<String>
        fun getThemen(senders: Collection<String>): List<String>
    }

    sealed interface FilmDataState {
        data object Loading : FilmDataState
        data object Ready : FilmDataState
    }

    interface ReloadRequester {
        fun requestTableReload()
        fun requestZeitraumReload()
    }

    interface SelectionObserverRegistry {
        fun addAvailableFiltersObserver(observer: Runnable)
        fun removeAvailableFiltersObserver(observer: Runnable)
        fun addCurrentFilterObserver(observer: Consumer<FilterDTO>)
        fun removeCurrentFilterObserver(observer: Consumer<FilterDTO>)
    }

    private object NoOpReloadRequester : ReloadRequester {
        override fun requestTableReload() = Unit
        override fun requestZeitraumReload() = Unit
    }

    private object NoOpDataProvider : DataProvider {
        override fun senderList(): EventList<String> = BasicEventList()
        override fun getThemen(senders: Collection<String>): List<String> = emptyList()
    }

    sealed interface AddFilterResult {
        data class Added(val filter: FilterDTO) : AddFilterResult
        data object NameAlreadyExists : AddFilterResult
    }

    sealed interface RenameFilterResult {
        data object Renamed : RenameFilterResult
        data object NameAlreadyExists : RenameFilterResult
    }

    data class RenderModel(
        val state: FilmFilterState,
        val availableThemen: List<String>,
        val canDeleteCurrentFilter: Boolean
    )

    private data class StateUpdateResult(
        val state: FilmFilterState,
        val changed: Boolean
    )

    fun state(): FilmFilterState = currentState

    fun areCurrentFilterChangesLocked(): Boolean = filterConfig.isCurrentFilterLocked

    fun setCurrentFilterChangesLocked(locked: Boolean) {
        filterConfig.setCurrentFilterLocked(locked)
        currentFilterObservers.forEach { it.accept(currentState.currentFilter) }
    }

    fun currentFilter(): FilterDTO = currentState.currentFilter

    fun isFilterLocked(filter: FilterDTO): Boolean = filterConfig.isFilterLocked(filter.id)

    fun availableFilters(): List<FilterDTO> = filterConfig.availableFilters

    fun senderList(): EventList<String> = dataProvider.senderList()

    fun canDeleteCurrentFilter(): Boolean = filterConfig.availableFilterCount > 1

    fun selectionObserverRegistry(): SelectionObserverRegistry = selectionObserverRegistry

    fun renderModel(): RenderModel {
        val availableThemen = dataProvider.getThemen(currentState.checkedChannels)
        val reconciledState = reconcileThema(availableThemen)
        return RenderModel(
            state = reconciledState,
            availableThemen = availableThemen,
            canDeleteCurrentFilter = canDeleteCurrentFilter()
        )
    }

    fun updateFilterState(transform: (FilmFilterState) -> FilmFilterState): FilmFilterState {
        return updateFilterStateInternal(transform).state
    }

    fun updateFilterStateAndReload(transform: (FilmFilterState) -> FilmFilterState): FilmFilterState {
        return updateFilterStateInternal(transform).also {
            if (it.changed) {
                reloadRequester.requestTableReload()
            }
        }.state
    }

    private fun updateFilterStateInternal(transform: (FilmFilterState) -> FilmFilterState): StateUpdateResult {
        val previousState = currentState
        val updatedState = transform(previousState)
        if (updatedState == previousState) {
            return StateUpdateResult(previousState, changed = false)
        }
        currentState = updatedState
        persist(updatedState)
        return StateUpdateResult(updatedState, changed = true)
    }

    fun onShowNewOnlyChanged(selected: Boolean) {
        updateFilterState { it.copy(showNewOnly = selected) }
    }

    fun onShowBookMarkedOnlyChanged(selected: Boolean) {
        updateFilterState { it.copy(showBookMarkedOnly = selected) }
    }

    fun onShowHighQualityOnlyChanged(selected: Boolean) {
        updateFilterState { it.copy(showHighQualityOnly = selected) }
    }

    fun onShowSubtitlesOnlyChanged(selected: Boolean) {
        updateFilterState { it.copy(showSubtitlesOnly = selected) }
    }

    fun onShowLivestreamsOnlyChanged(selected: Boolean) {
        updateFilterState { it.copy(showLivestreamsOnly = selected) }
    }

    fun onShowUnseenOnlyChanged(selected: Boolean) {
        updateFilterState { it.copy(showUnseenOnly = selected) }
    }

    fun onDontShowAbosChanged(selected: Boolean) {
        updateFilterState { it.copy(dontShowAbos = selected) }
    }

    fun onDontShowSignLanguageChanged(selected: Boolean) {
        updateFilterState { it.copy(dontShowSignLanguage = selected) }
    }

    fun onDontShowGeoblockedChanged(selected: Boolean) {
        updateFilterState { it.copy(dontShowGeoblocked = selected) }
    }

    fun onDontShowTrailersChanged(selected: Boolean) {
        updateFilterState { it.copy(dontShowTrailers = selected) }
    }

    fun onDontShowAudioVersionsChanged(selected: Boolean) {
        updateFilterState { it.copy(dontShowAudioVersions = selected) }
    }

    fun onDontShowDuplicatesChanged(selected: Boolean) {
        updateFilterState { it.copy(dontShowDuplicates = selected) }
    }

    fun initializeFilmData(hasAvailableThemen: Boolean) {
        filmDataState = if (hasAvailableThemen) FilmDataState.Ready else FilmDataState.Loading
    }

    fun initializeFilmData() {
        initializeFilmData(dataProvider.getThemen(emptySet()).isNotEmpty())
    }

    fun onFilmDataLoaded() {
        filmDataState = FilmDataState.Ready
    }

    fun onFilmLengthChanged(min: Int, max: Int) {
        updateFilterStateAndReload { it.copy(filmLengthMin = min, filmLengthMax = max) }
    }

    fun onThemaChanged(thema: String) {
        updateFilterStateAndReload { it.copy(thema = thema) }
    }

    fun onThemaReset() {
        updateFilterStateAndReload { it.copy(thema = "") }
    }

    fun onSenderSelectionChanged(checkedChannels: Set<String>) {
        updateFilterStateAndReload { it.copy(checkedChannels = checkedChannels) }
    }

    fun onZeitraumChanged(value: String) {
        updateFilterState { it.copy(zeitraum = value) }
    }

    fun onZeitraumFallbackChanged(value: String) {
        updateFilterState { it.copy(zeitraum = value) }
    }

    fun requestTableReload() {
        reloadRequester.requestTableReload()
    }

    fun requestZeitraumReload() {
        reloadRequester.requestZeitraumReload()
    }

    fun restoreCurrentFilterSelection(filter: FilterDTO): Boolean {
        if (filterConfig.currentFilter != filter) {
            logger.trace("Updating filter lifecycle for reason=selectFilter: from={} to={}", currentState.currentFilter, filter)
            filterConfig.currentFilter = filter
            syncStateFromConfig("selectFilter")
            return true
        }
        return false
    }

    fun reconcileThema(availableThemen: List<String>): FilmFilterState {
        val reconciledThema = when {
            currentState.thema.isEmpty() -> ""
            filmDataState is FilmDataState.Loading -> currentState.thema
            availableThemen.contains(currentState.thema) -> currentState.thema
            else -> ""
        }

        if (reconciledThema == currentState.thema) {
            return currentState
        }

        return updateFilterState { it.copy(thema = reconciledThema) }
    }

    fun nextFilterNameSuggestion(): String = "Filter ${filterConfig.availableFilters.size + 1}"

    fun addFilter(name: String): AddFilterResult {
        if (filterConfig.findFilterForName(name).isPresent) {
            return AddFilterResult.NameAlreadyExists
        }

        val newFilter = FilterDTO(UUID.randomUUID(), name)
        logger.trace("Updating filter lifecycle for reason=addFilter: newFilter={}", newFilter)
        filterConfig.addNewFilter(newFilter)
        filterConfig.currentFilter = newFilter
        filterConfig.clearCurrentFilter()
        syncStateFromConfig("addFilter", notifyAvailableFilters = true)
        return AddFilterResult.Added(newFilter)
    }

    fun cloneCurrentFilter(): AddFilterResult.Added {
        val sourceState = currentState
        val newFilter = FilterDTO(UUID.randomUUID(), nextCloneFilterName(sourceState.currentFilter.name))
        logger.trace(
            "Updating filter lifecycle for reason=cloneFilter: sourceFilter={} newFilter={}",
            sourceState.currentFilter,
            newFilter
        )
        filterConfig.addNewFilter(newFilter)
        persist(sourceState.copy(currentFilter = newFilter))
        syncStateFromConfig("cloneFilter", notifyAvailableFilters = true)
        return AddFilterResult.Added(newFilter)
    }

    fun deleteCurrentFilter() {
        logger.trace("Updating filter lifecycle for reason=deleteFilter: currentFilter={}", currentState.currentFilter)
        filterConfig.deleteFilter(filterConfig.currentFilter)
        syncStateFromConfig("deleteFilter", notifyAvailableFilters = true)
    }

    fun resetCurrentFilter() {
        logger.trace("Updating filter lifecycle for reason=resetFilter: currentFilter={}", currentState.currentFilter)
        if (filterConfig.isCurrentFilterLocked) {
            syncStateFromConfig("resetFilter")
            return
        }
        filterConfig.clearCurrentFilter()
        syncStateFromConfig("resetFilter")
    }

    fun renameCurrentFilter(newName: String): RenameFilterResult {
        if (currentState.currentFilter.name == newName) {
            return RenameFilterResult.Renamed
        }
        if (filterConfig.findFilterForName(newName).isPresent) {
            return RenameFilterResult.NameAlreadyExists
        }

        logger.trace(
            "Updating filter lifecycle for reason=renameFilter: currentFilter={} newName={}",
            currentState.currentFilter,
            newName
        )
        filterConfig.renameCurrentFilter(newName)
        syncStateFromConfig("renameFilter", notifyAvailableFilters = true)
        return RenameFilterResult.Renamed
    }

    private fun persist(state: FilmFilterState) {
        if (filterConfig.isCurrentFilterLocked) {
            //logger.trace("Skipping filter persistence for reason={} because current filter changes are locked", reason)
            return
        }
        if (filterConfig.currentFilter != state.currentFilter) {
            filterConfig.currentFilter = state.currentFilter
        }
        filterConfig.setShowNewOnly(state.showNewOnly)
        filterConfig.setShowBookMarkedOnly(state.showBookMarkedOnly)
        filterConfig.setShowHighQualityOnly(state.showHighQualityOnly)
        filterConfig.setShowSubtitlesOnly(state.showSubtitlesOnly)
        filterConfig.setShowLivestreamsOnly(state.showLivestreamsOnly)
        filterConfig.setShowUnseenOnly(state.showUnseenOnly)
        filterConfig.setDontShowAbos(state.dontShowAbos)
        filterConfig.setDontShowSignLanguage(state.dontShowSignLanguage)
        filterConfig.setDontShowGeoblocked(state.dontShowGeoblocked)
        filterConfig.setDontShowTrailers(state.dontShowTrailers)
        filterConfig.setDontShowAudioVersions(state.dontShowAudioVersions)
        filterConfig.setDontShowDuplicates(state.dontShowDuplicates)
        filterConfig.setCheckedChannels(state.checkedChannels)
        filterConfig.thema = state.thema
        filterConfig.setFilmLengthMin(state.filmLengthMin.toDouble())
        filterConfig.setFilmLengthMax(state.filmLengthMax.toDouble())
        filterConfig.setZeitraum(state.zeitraum)
    }

    private fun syncStateFromConfig(reason: String, notifyAvailableFilters: Boolean = false) {
        val previousState = currentState
        currentState = FilmFilterState.from(filterConfig)
        logger.trace("Synced filter lifecycle state for reason={}: {}", reason, currentState)
        if (notifyAvailableFilters) {
            availableFiltersObservers.forEach(Runnable::run)
        }
        if (previousState.currentFilter != currentState.currentFilter) {
            currentFilterObservers.forEach { it.accept(currentState.currentFilter) }
        }
    }

    private fun nextCloneFilterName(sourceName: String): String {
        val baseName = "$sourceName$COPY_SUFFIX"
        if (filterConfig.findFilterForName(baseName).isEmpty) {
            return baseName
        }

        var suffixIndex = 2
        while (true) {
            val candidate = "$baseName $suffixIndex"
            if (filterConfig.findFilterForName(candidate).isEmpty) {
                return candidate
            }
            suffixIndex++
        }
    }
}
