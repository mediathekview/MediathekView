/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.tabs.tab_film.helpers

import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.filter.FilmFilterState
import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider
import mediathek.gui.tabs.tab_film.filter.ZeitraumSpinner
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import java.util.stream.Stream
import kotlin.time.Duration.Companion.minutes

fun interface FilmQueryEngine {
    fun query(): List<DatenFilm>
}

internal class GuiModelHelperSupport(
    private val searchFieldData: SearchFieldData,
    private val filterController: FilmFilterController,
) {
    fun getFilteredFilms(
        allFilms: Collection<DatenFilm>,
        filteredFilmSupplier: (FilterExecutionContext) -> Collection<DatenFilm>,
    ): List<DatenFilm> {
        if (allFilms.isEmpty()) {
            return emptyList()
        }
        val filterContext = createFilterExecutionContext()
        if (filterContext.noFiltersAreSet) {
            return allFilms as? List<DatenFilm> ?: allFilms.toList()
        }
        val filteredFilms = filteredFilmSupplier(filterContext)
        return filteredFilms as? List<DatenFilm> ?: filteredFilms.toList()
    }

    fun applyCommonFilters(
        source: Stream<DatenFilm>,
        filterContext: FilterExecutionContext,
    ): Stream<DatenFilm> =
        source.filter { film ->
            matchesThemaFilter(film, filterContext) &&
                    matchesMaxLengthFilter(film, filterContext.lengthFilterRange) &&
                    matchesSeenFilter(film, filterContext) &&
                    minLengthCheck(film, filterContext.lengthFilterRange)
        }

    fun createFilterExecutionContext(): FilterExecutionContext {
        val state = state()
        val selectedSenders = getSelectedSendersFromFilter(state)
        val searchTerms = searchFieldData.evaluateThemaTitel().toList()
        val searchThroughDescriptions = searchFieldData.searchThroughDescriptions()
        return FilterExecutionContext(
            state = state,
            lengthFilterRange = createLengthFilterRange(state),
            selectedSenders = selectedSenders,
            filterThema = state.thema,
            searchFieldText = searchFieldData.searchFieldText,
            searchThroughDescriptions = searchThroughDescriptions,
            searchTerms = searchTerms,
            senderFilter = { film -> selectedSenders.isEmpty() || film.sender in selectedSenders },
            finalStageFilter = if (searchTerms.isEmpty()) {
                { true }
            } else {
                createFinalStageFilter(
                    searchThroughDescriptions,
                    searchTerms.toTypedArray(),
                )::test
            },
            noFiltersAreSet = noFiltersAreSet(state) && searchFieldData.isEmpty(),
        )
    }

    fun state(): FilmFilterState = filterController.state()

    private fun noFiltersAreSet(state: FilmFilterState): Boolean =
        state.checkedChannels.isEmpty() &&
                state.thema.isEmpty() &&
                state.filmLengthMin == 0 &&
                state.filmLengthMax == FilmLengthSlider.UNLIMITED_VALUE &&
                !state.dontShowAbos &&
                !state.showUnseenOnly &&
                !state.showHighQualityOnly &&
                !state.showSubtitlesOnly &&
                !state.showLivestreamsOnly &&
                !state.showNewOnly &&
                !state.showBookMarkedOnly &&
                !state.dontShowTrailers &&
                !state.dontShowSignLanguage &&
                !state.dontShowGeoblocked &&
                !state.dontShowAudioVersions &&
                !state.dontShowDuplicates &&
                state.zeitraum.equals(ZeitraumSpinner.INFINITE_TEXT, ignoreCase = true)

    private fun minLengthCheck(film: DatenFilm, lengthFilterRange: LengthFilterRange): Boolean {
        val filmLength = film.filmLength
        if (filmLength == 0) {
            return true
        }
        return filmLength >= lengthFilterRange.minLengthInSeconds
    }

    private fun matchesThemaFilter(film: DatenFilm, filterContext: FilterExecutionContext): Boolean =
        filterContext.filterThema.isEmpty() || film.thema.equals(filterContext.filterThema, ignoreCase = true)

    private fun matchesMaxLengthFilter(film: DatenFilm, lengthFilterRange: LengthFilterRange): Boolean =
        !lengthFilterRange.hasUpperLimit() || film.filmLength < lengthFilterRange.maxLengthInSeconds

    private fun matchesSeenFilter(film: DatenFilm, filterContext: FilterExecutionContext): Boolean =
        !filterContext.state.showUnseenOnly || seenCheck(film)

    private fun getSelectedSendersFromFilter(state: FilmFilterState): Set<String> =
        state.checkedChannels
            .filter(SenderFilmlistLoadApprover::isApproved)
            .toSet()

    private fun seenCheck(film: DatenFilm): Boolean = !film.isSeenInHistory

    private fun createLengthFilterRange(state: FilmFilterState): LengthFilterRange =
        LengthFilterRange(
            minLengthInSeconds = state.filmLengthMin.minutes.inWholeSeconds,
            maxLengthInSeconds = state.filmLengthMax.minutes.inWholeSeconds,
        )

    data class LengthFilterRange(
        val minLengthInSeconds: Long,
        val maxLengthInSeconds: Long,
    ) {
        fun hasUpperLimit(): Boolean = maxLengthInSeconds < UNLIMITED_LENGTH_IN_SECONDS
    }

    data class FilterExecutionContext(
        val state: FilmFilterState,
        val lengthFilterRange: LengthFilterRange,
        val selectedSenders: Set<String>,
        val filterThema: String,
        val searchFieldText: String,
        val searchThroughDescriptions: Boolean,
        val searchTerms: List<String>,
        val senderFilter: (DatenFilm) -> Boolean,
        val finalStageFilter: (DatenFilm) -> Boolean,
        val noFiltersAreSet: Boolean,
    ) {
        val hasSearchTerms: Boolean
            get() = searchTerms.isNotEmpty()

        val hasSelectedSenders: Boolean
            get() = selectedSenders.isNotEmpty()
    }

    private companion object {
        private val UNLIMITED_LENGTH_IN_SECONDS = FilmLengthSlider.UNLIMITED_VALUE.minutes.inWholeSeconds
    }
}
