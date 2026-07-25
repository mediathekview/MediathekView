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

import mediathek.config.application.ApplicationConfiguration
import mediathek.controller.history.FilmSeenHistoryController
import mediathek.daten.DatenFilm
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.tabs.tab_film.filter.FilmFilterController
import mediathek.gui.tabs.tab_film.search.SearchFieldData
import java.util.stream.Stream

class GuiFilmeModelHelper(
    private val filmCatalog: FilmCatalog,
    searchFieldData: SearchFieldData,
    filterController: FilmFilterController,
) : FilmQueryEngine {
    private val support = GuiModelHelperSupport(searchFieldData, filterController)

    override fun query(): List<DatenFilm> {
        val allFilms = allFilms()
        FilmSeenHistoryController.prepareSeenState(allFilms)
        return support.getFilteredFilms(allFilms) { filterContext ->
            filterFilms(allFilms, filterContext)
        }
    }

    private fun allFilms(): Collection<DatenFilm> = filmCatalog.filteredFilms.snapshot()

    private fun filterFilms(
        allFilms: Collection<DatenFilm>,
        filterContext: GuiModelHelperSupport.FilterExecutionContext,
    ): Collection<DatenFilm> {
        val state = filterContext.state
        var stream = allFilms.parallelStream()
            .filterIf(filterContext.hasSelectedSenders) { film -> filterContext.senderFilter(film) }
            .filterIf(state.showNewOnly, DatenFilm::isNew)
            .filterIf(state.showBookMarkedOnly, DatenFilm::isBookmarked)
            .filterIf(state.showLivestreamsOnly, DatenFilm::isLivestream)
            .filterIf(state.showHighQualityOnly, DatenFilm::isHighQuality)
            .filterIf(state.dontShowTrailers) { film -> !film.isTrailerTeaser }
            .filterIf(state.dontShowSignLanguage) { film -> !film.isSignLanguage }

        if (state.dontShowGeoblocked) {
            val geographicLocation = ApplicationConfiguration.getInstance().geographicLocation
            stream = stream.filter { film -> !film.isGeoBlockedForLocation(geographicLocation) }
        }
        stream = stream
            .filterIf(state.dontShowAudioVersions) { film -> !film.isAudioVersion }
            .filterIf(state.dontShowAbos) { film -> film.abo == null }
            .filterIf(state.dontShowDuplicates) { film -> !film.isDuplicate }
            .filterIf(state.showSubtitlesOnly, DatenFilm::hasAnySubtitles)

        stream = support.applyCommonFilters(stream, filterContext)
        if (filterContext.hasSearchTerms) {
            stream = stream.filter { film -> filterContext.finalStageFilter(film) }
        }

        return stream.toList()
    }
}

private inline fun <T> Stream<T>.filterIf(
    enabled: Boolean,
    crossinline predicate: (T) -> Boolean,
): Stream<T> = if (enabled) {
    filter { item -> predicate(item) }
} else {
    this
}
