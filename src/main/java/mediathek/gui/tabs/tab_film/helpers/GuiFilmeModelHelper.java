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

import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.Country;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.filter.FilmFilterController;
import mediathek.tool.ApplicationConfiguration;
import org.jspecify.annotations.NonNull;

import javax.swing.table.TableModel;
import java.util.Collection;
import java.util.List;
import java.util.function.BooleanSupplier;
import java.util.function.Predicate;
import java.util.stream.Stream;

public final class GuiFilmeModelHelper implements GuiModelHelper {
    private final GuiModelHelperSupport support;

    public GuiFilmeModelHelper(@NonNull SearchFieldData searchFieldData,
                               @NonNull FilmFilterController filterController) {
        support = new GuiModelHelperSupport(searchFieldData, filterController);
    }

    @Override
    public TableModel getFilteredTableModel() {
        var allFilms = getAllFilms();
        return support.getFilteredTableModel(allFilms, this::filterFilms);
    }

    private Collection<DatenFilm> getAllFilms() {
        return Daten.getInstance().getListeFilmeNachBlackList();
    }

    private Collection<DatenFilm> filterFilms() {
        var filterContext = support.createFilterExecutionContext();

        if (support.state().getShowUnseenOnly()) {
            SeenHistoryController.prepareSharedMemoryCache();
        }

        var stream = Daten.getInstance().getListeFilmeNachBlackList().parallelStream();
        if (filterContext.hasSelectedSenders()) {
            stream = stream.filter(filterContext.senderFilter());
        }
        stream = applyConfiguredPredicates(stream);

        stream = support.applyCommonFilters(stream, filterContext.filterThema(), filterContext.lengthFilterRange());

        if (filterContext.hasSearchTerms()) {
            stream = stream.filter(filterContext.finalStageFilter());
        }

        return stream.toList();
    }

    private Stream<DatenFilm> applyConfiguredPredicates(Stream<DatenFilm> stream) {
        for (var predicateSpec : createPredicateSpecs()) {
            if (predicateSpec.enabled().getAsBoolean()) {
                stream = stream.filter(predicateSpec.predicate());
            }
        }
        return stream;
    }

    private List<PredicateSpec> createPredicateSpecs() {
        final Country geographicLocation = ApplicationConfiguration.getInstance().getGeographicLocation();

        return List.of(
                predicateSpec(() -> support.state().getShowNewOnly(), DatenFilm::isNew),
                predicateSpec(() -> support.state().getShowBookMarkedOnly(), DatenFilm::isBookmarked),
                predicateSpec(() -> support.state().getShowLivestreamsOnly(), DatenFilm::isLivestream),
                predicateSpec(() -> support.state().getShowHighQualityOnly(), DatenFilm::isHighQuality),
                predicateSpec(() -> support.state().getDontShowTrailers(), film -> !film.isTrailerTeaser()),
                predicateSpec(() -> support.state().getDontShowSignLanguage(), film -> !film.isSignLanguage()),
                predicateSpec(
                        () -> support.state().getDontShowGeoblocked(),
                        film -> !film.isGeoBlockedForLocation(geographicLocation)),
                predicateSpec(() -> support.state().getDontShowAudioVersions(), film -> !film.isAudioVersion()),
                predicateSpec(() -> support.state().getDontShowAbos(), film -> film.getAbo() == null),
                predicateSpec(() -> support.state().getDontShowDuplicates(), film -> !film.isDuplicate()),
                predicateSpec(() -> support.state().getShowSubtitlesOnly(), DatenFilm::hasAnySubtitles));
    }

    private PredicateSpec predicateSpec(@NonNull BooleanSupplier enabled, @NonNull Predicate<DatenFilm> predicate) {
        return new PredicateSpec(enabled, predicate);
    }

    private record PredicateSpec(BooleanSupplier enabled, Predicate<DatenFilm> predicate) {}
}
