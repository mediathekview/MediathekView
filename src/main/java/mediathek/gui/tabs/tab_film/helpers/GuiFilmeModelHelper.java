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
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.FilterConfiguration;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;
import java.util.List;
import java.util.function.BooleanSupplier;
import java.util.function.Predicate;
import java.util.stream.Stream;

public final class GuiFilmeModelHelper implements GuiModelHelper {
    private final GuiModelHelperSupport support;

    public GuiFilmeModelHelper(@NotNull SeenHistoryController historyController,
                               @NotNull SearchFieldData searchFieldData,
                               @NotNull FilterConfiguration filterConfiguration) {
        support = new GuiModelHelperSupport(historyController, searchFieldData, filterConfiguration);
    }

    @Override
    public javax.swing.table.TableModel getFilteredTableModel() {
        var allFilms = getAllFilms();
        return support.getFilteredTableModel(allFilms, this::filterFilms);
    }

    private Collection<DatenFilm> getAllFilms() {
        return Daten.getInstance().getListeFilmeNachBlackList();
    }

    private Collection<DatenFilm> filterFilms() {
        var filterContext = support.createFilterExecutionContext();

        if (support.filterConfiguration().isShowUnseenOnly()) {
            support.prepareHistoryMemoryCache();
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
        return List.of(
                predicateSpec(support.filterConfiguration()::isShowNewOnly, DatenFilm::isNew),
                predicateSpec(support.filterConfiguration()::isShowBookMarkedOnly, DatenFilm::isBookmarked),
                predicateSpec(support.filterConfiguration()::isShowLivestreamsOnly, DatenFilm::isLivestream),
                predicateSpec(support.filterConfiguration()::isShowHighQualityOnly, DatenFilm::isHighQuality),
                predicateSpec(support.filterConfiguration()::isDontShowTrailers, film -> !film.isTrailerTeaser()),
                predicateSpec(support.filterConfiguration()::isDontShowSignLanguage, film -> !film.isSignLanguage()),
                predicateSpec(
                        support.filterConfiguration()::isDontShowGeoblocked,
                        film -> !film.isGeoBlockedForLocation(ApplicationConfiguration.getInstance().getGeographicLocation())),
                predicateSpec(support.filterConfiguration()::isDontShowAudioVersions, film -> !film.isAudioVersion()),
                predicateSpec(support.filterConfiguration()::isDontShowAbos, film -> film.getAbo() == null),
                predicateSpec(support.filterConfiguration()::isDontShowDuplicates, film -> !film.isDuplicate()),
                predicateSpec(support.filterConfiguration()::isShowSubtitlesOnly, DatenFilm::hasAnySubtitles));
    }

    private PredicateSpec predicateSpec(@NotNull BooleanSupplier enabled, @NotNull Predicate<DatenFilm> predicate) {
        return new PredicateSpec(enabled, predicate);
    }

    private record PredicateSpec(BooleanSupplier enabled, Predicate<DatenFilm> predicate) {}
}
