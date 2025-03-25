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

import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.javafx.filterpanel.FilterActionPanel;
import mediathek.tool.FilterConfiguration;
import mediathek.tool.models.TModelFilm;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableModel;

public class GuiFilmeModelHelper extends GuiModelHelper {
    private TModelFilm filmModel;

    public GuiFilmeModelHelper(@NotNull FilterActionPanel filterActionPanel,
                               @NotNull SeenHistoryController historyController,
                               @NotNull SearchFieldData searchFieldData,
                               @NotNull FilterConfiguration filterConfiguration) {
        super(filterActionPanel, historyController, searchFieldData, filterConfiguration);
    }

    private void performTableFiltering() {
        calculateFilmLengthSliderValues();

        final String filterThema = getFilterThema();

        if (filterConfiguration.isShowUnseenOnly())
            historyController.prepareMemoryCache();

        var stream = Daten.getInstance().getListeFilmeNachBlackList().parallelStream();
        var selectedSenders = filterConfiguration.getCheckedChannels();
        if (!selectedSenders.isEmpty()) {
            stream = stream.filter(f -> selectedSenders.contains(f.getSender()));
        }
        if (filterConfiguration.isShowNewOnly())
            stream = stream.filter(DatenFilm::isNew);
        if (filterConfiguration.isShowBookMarkedOnly())
            stream = stream.filter(DatenFilm::isBookmarked);
        if (filterConfiguration.isShowLivestreamsOnly())
            stream = stream.filter(DatenFilm::isLivestream);
        if (filterConfiguration.isShowHdOnly())
            stream = stream.filter(DatenFilm::isHighQuality);
        if (filterConfiguration.isDontShowTrailers())
            stream = stream.filter(film -> !film.isTrailerTeaser());
        if (filterConfiguration.isDontShowSignLanguage())
            stream = stream.filter(film -> !film.isSignLanguage());
        if (filterConfiguration.isDontShowAudioVersions())
            stream = stream.filter(film -> !film.isAudioVersion());
        if (filterConfiguration.isDontShowAbos())
            stream = stream.filter(film -> film.getAbo() == null);
        if (filterConfiguration.isDontShowDuplicates()) {
            stream = stream.filter(film -> !film.isDuplicate());
        }
        if (filterConfiguration.isShowSubtitlesOnly()) {
            stream = stream.filter(DatenFilm::hasAnySubtitles);
        }

        stream = applyCommonFilters(stream, filterThema);

        //final stage filtering...
        String[] arrIrgendwo = searchFieldData.evaluateThemaTitel();
        final boolean searchFieldEmpty = arrIrgendwo.length == 0;
        if (!searchFieldEmpty) {
            stream = stream.filter(FinalStageFilterFactory
                    .createFinalStageFilter(searchFieldData.searchThroughDescriptions(), arrIrgendwo));
        }

        var list = stream.toList();
        stream.close();

        //adjust initial capacity
        filmModel = new TModelFilm(list.size());
        filmModel.addAll(list);

        if (filterConfiguration.isShowUnseenOnly())
            historyController.emptyMemoryCache();
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
