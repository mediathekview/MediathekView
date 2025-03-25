/*
 * Copyright (c) 2025 derreisende77.
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

import javafx.collections.ObservableList;
import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPattern;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPatternWithDescription;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilter;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilterWithDescription;
import mediathek.javafx.filterpanel.FilterActionPanel;
import mediathek.tool.Filter;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.models.TModelFilm;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableModel;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

public class GuiFilmeModelHelper extends GuiModelHelper {
    private TModelFilm filmModel;
    private String[] arrIrgendwo;

    public GuiFilmeModelHelper(@NotNull FilterActionPanel filterActionPanel,
                               @NotNull SeenHistoryController historyController,
                               @NotNull SearchFieldData searchFieldData,
                               @NotNull FilterConfiguration filterConfiguration) {
        super(filterActionPanel, historyController, searchFieldData, filterConfiguration);
    }

    private void performTableFiltering() {
        arrIrgendwo = searchFieldData.evaluateThemaTitel();

        calculateFilmLengthSliderValues();

        final String filterThema = getFilterThema();
        final ObservableList<String> selectedSenders = filterActionPanel.getViewSettingsPane().senderCheckList.getCheckModel().getCheckedItems();

        if (filterActionPanel.isShowUnseenOnly())
            historyController.prepareMemoryCache();

        var stream = Daten.getInstance().getListeFilmeNachBlackList().parallelStream();
        if (!selectedSenders.isEmpty()) {
            //ObservableList.contains() is insanely slow...this speeds up to factor 250!
            Set<String> senderSet = new HashSet<>(selectedSenders.size());
            senderSet.addAll(selectedSenders);
            stream = stream.filter(f -> senderSet.contains(f.getSender()));
        }
        if (filterActionPanel.isShowNewOnly())
            stream = stream.filter(DatenFilm::isNew);
        if (filterActionPanel.isShowBookMarkedOnly())
            stream = stream.filter(DatenFilm::isBookmarked);
        if (filterActionPanel.isShowLivestreamsOnly())
            stream = stream.filter(DatenFilm::isLivestream);
        if (filterActionPanel.isShowOnlyHighQuality())
            stream = stream.filter(DatenFilm::isHighQuality);
        if (filterActionPanel.isDontShowTrailers())
            stream = stream.filter(film -> !film.isTrailerTeaser());
        if (filterActionPanel.isDontShowSignLanguage())
            stream = stream.filter(film -> !film.isSignLanguage());
        if (filterActionPanel.isDontShowAudioVersions())
            stream = stream.filter(film -> !film.isAudioVersion());
        if (filterActionPanel.isDontShowAbos())
            stream = stream.filter(film -> film.getAbo() == null);
        if (filterActionPanel.isDontShowDuplicates()) {
            stream = stream.filter(film -> !film.isDuplicate());
        }
        if (filterActionPanel.isShowSubtitlesOnly()) {
            stream = stream.filter(this::subtitleCheck);
        }

        stream = applyCommonFilters(stream, filterThema);

        //final stage filtering...
        final boolean searchFieldEmpty = arrIrgendwo.length == 0;
        if (!searchFieldEmpty) {
            stream = stream.filter(createFinalStageFilter());
        }

        var list = stream.toList();
        stream.close();

        //adjust initial capacity
        filmModel = new TModelFilm(list.size());
        filmModel.addAll(list);

        if (filterActionPanel.isShowUnseenOnly())
            historyController.emptyMemoryCache();
    }

    private Predicate<DatenFilm> createFinalStageFilter() {
        //if arrIrgendwo contains more than one search fields fall back to "old" pattern search
        //otherwise use more optimized search
        boolean isPattern = Filter.isPattern(arrIrgendwo[0]) || arrIrgendwo.length > 1;
        Predicate<DatenFilm> filter;
        if (searchFieldData.searchThroughDescriptions()) {
            if (isPattern)
                filter = new FinalStagePatternFilterWithDescription(arrIrgendwo);
            else
                filter = new FinalStageFilterNoPatternWithDescription(arrIrgendwo);
        }
        else {
            if (isPattern)
                filter = new FinalStagePatternFilter(arrIrgendwo);
            else
                filter = new FinalStageFilterNoPattern(arrIrgendwo);
        }
        return filter;
    }

    private boolean subtitleCheck(DatenFilm film) {
        return film.hasSubtitle() || film.hasBurnedInSubtitles();
    }

    @Override
    public TableModel getFilteredTableModel() {
        final var listeFilme = Daten.getInstance().getListeFilmeNachBlackList();

        if (!listeFilme.isEmpty()) {
            if (noFiltersAreSet()) {
                //adjust initial capacity
                filmModel = new TModelFilm(listeFilme.size());
                filmModel.addAll(listeFilme);
            } else {
                performTableFiltering();
            }
        } else
            return new TModelFilm();

        return filmModel;
    }
}
