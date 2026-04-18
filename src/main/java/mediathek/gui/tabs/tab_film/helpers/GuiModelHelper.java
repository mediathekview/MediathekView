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

package mediathek.gui.tabs.tab_film.helpers;

import mediathek.controller.SenderFilmlistLoadApprover;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.filter.FilmFilterController;
import mediathek.gui.tabs.tab_film.filter.FilmFilterState;
import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider;
import mediathek.tool.models.TModelFilm;
import org.jspecify.annotations.NonNull;

import javax.swing.table.TableModel;
import java.util.Collection;
import java.util.List;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.function.Predicate;
import java.util.function.Supplier;
import java.util.stream.Stream;

public sealed interface GuiModelHelper permits GuiFilmeModelHelper, LuceneGuiFilmeModelHelper {
    TableModel getFilteredTableModel();
}

final class GuiModelHelperSupport {
    private static final long UNLIMITED_LENGTH_IN_SECONDS = TimeUnit.SECONDS.convert(FilmLengthSlider.UNLIMITED_VALUE, TimeUnit.MINUTES);

    private final SearchFieldData searchFieldData;
    private final FilmFilterController filterController;

    GuiModelHelperSupport(@NonNull SearchFieldData searchFieldData,
                          @NonNull FilmFilterController filterController) {
        this.searchFieldData = searchFieldData;
        this.filterController = filterController;
    }

    TableModel getFilteredTableModel(@NonNull Collection<DatenFilm> allFilms,
                                     @NonNull Supplier<Collection<DatenFilm>> filteredFilmSupplier) {
        if (allFilms.isEmpty()) {
            return createEmptyFilmTableModel();
        }
        if (noFiltersAreSet()) {
            return createFilmTableModel(allFilms);
        }
        return createFilmTableModel(filteredFilmSupplier.get());
    }

    Stream<DatenFilm> applyCommonFilters(Stream<DatenFilm> stream,
                                         final String filterThema,
                                         @NonNull LengthFilterRange lengthFilterRange) {
        if (!filterThema.isEmpty()) {
            stream = stream.filter(film -> film.getThema().equalsIgnoreCase(filterThema));
        }
        if (lengthFilterRange.hasUpperLimit()) {
            stream = stream.filter(film -> film.getFilmLength() < lengthFilterRange.maxLengthInSeconds());
        }
        if (state().getShowUnseenOnly()) {
            stream = stream.filter(this::seenCheck);
        }
        return stream.filter(film -> minLengthCheck(film, lengthFilterRange));
    }

    boolean noFiltersAreSet() {
        return noFiltersAreSet(state()) && searchFieldData.isEmpty();
    }

    private boolean noFiltersAreSet(@NonNull FilmFilterState state) {
        return state.getCheckedChannels().isEmpty()
                && state.getThema().isEmpty()
                && state.getFilmLengthMin() == 0
                && state.getFilmLengthMax() == FilmLengthSlider.UNLIMITED_VALUE
                && !state.getDontShowAbos()
                && !state.getShowUnseenOnly()
                && !state.getShowHighQualityOnly()
                && !state.getShowSubtitlesOnly()
                && !state.getShowLivestreamsOnly()
                && !state.getShowNewOnly()
                && !state.getShowBookMarkedOnly()
                && !state.getDontShowTrailers()
                && !state.getDontShowSignLanguage()
                && !state.getDontShowGeoblocked()
                && !state.getDontShowAudioVersions()
                && !state.getDontShowDuplicates()
                && state.getZeitraum().equalsIgnoreCase(mediathek.gui.tabs.tab_film.filter.ZeitraumSpinner.INFINITE_TEXT);
    }

    FilterExecutionContext createFilterExecutionContext() {
        var state = state();
        var selectedSenders = getSelectedSendersFromFilter();
        var searchTerms = List.of(searchFieldData.evaluateThemaTitel());
        return new FilterExecutionContext(
                createLengthFilterRange(),
                selectedSenders,
                state.getThema(),
                searchFieldData.searchFieldText(),
                searchFieldData.searchThroughDescriptions(),
                searchTerms,
                film -> selectedSenders.isEmpty() || selectedSenders.contains(film.getSender()),
                searchTerms.isEmpty()
                        ? _ -> true
                        : FinalStageFilterFactory.createFinalStageFilter(
                                searchFieldData.searchThroughDescriptions(),
                                searchTerms.toArray(String[]::new)));
    }

    FilmFilterState state() {
        return filterController.state();
    }

    private boolean minLengthCheck(DatenFilm film, @NonNull LengthFilterRange lengthFilterRange) {
        var filmLength = film.getFilmLength();
        if (filmLength == 0) {
            return true;
        }
        return filmLength >= lengthFilterRange.minLengthInSeconds();
    }

    private Set<String> getSelectedSendersFromFilter() {
        return state().getCheckedChannels().stream()
                .filter(SenderFilmlistLoadApprover::isApproved)
                .collect(java.util.stream.Collectors.toUnmodifiableSet());
    }

    private boolean seenCheck(DatenFilm film) {
        return !SeenHistoryController.hasBeenSeenFromSharedCache(film);
    }

    private LengthFilterRange createLengthFilterRange() {
        var state = state();
        return new LengthFilterRange(
                TimeUnit.SECONDS.convert(state.getFilmLengthMin(), TimeUnit.MINUTES),
                TimeUnit.SECONDS.convert(state.getFilmLengthMax(), TimeUnit.MINUTES));
    }

    private TModelFilm createFilmTableModel(@NonNull Collection<DatenFilm> films) {
        var filmModel = new TModelFilm(films.size());
        filmModel.addAll(films instanceof List<DatenFilm> filmList ? filmList : List.copyOf(films));
        return filmModel;
    }

    private TModelFilm createEmptyFilmTableModel() {
        return new TModelFilm();
    }

    record LengthFilterRange(long minLengthInSeconds, long maxLengthInSeconds) {
        boolean hasUpperLimit() {
            return maxLengthInSeconds < UNLIMITED_LENGTH_IN_SECONDS;
        }
    }

    record FilterExecutionContext(LengthFilterRange lengthFilterRange,
                                  Set<String> selectedSenders,
                                  String filterThema,
                                  String searchFieldText,
                                  boolean searchThroughDescriptions,
                                  List<String> searchTerms,
                                  Predicate<DatenFilm> senderFilter,
                                  Predicate<DatenFilm> finalStageFilter) {
        boolean hasSearchTerms() {
            return !searchTerms.isEmpty();
        }

        boolean hasSelectedSenders() {
            return !selectedSenders.isEmpty();
        }
    }
}
