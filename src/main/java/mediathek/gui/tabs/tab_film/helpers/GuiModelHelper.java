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

import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.SearchFieldData;
import mediathek.gui.tabs.tab_film.filter.FilmLengthSlider;
import mediathek.tool.FilterConfiguration;
import org.jetbrains.annotations.NotNull;

import javax.swing.table.TableModel;
import java.util.concurrent.TimeUnit;
import java.util.stream.Stream;

public abstract class GuiModelHelper {
    private final static long UNLIMITED_LENGTH_IN_SECONDS = TimeUnit.SECONDS.convert(FilmLengthSlider.UNLIMITED_VALUE, TimeUnit.MINUTES);
    protected final SeenHistoryController historyController;
    protected final SearchFieldData searchFieldData;
    protected final FilterConfiguration filterConfiguration;
    private long minLengthInSeconds;
    private long maxLengthInSeconds = UNLIMITED_LENGTH_IN_SECONDS;

    protected GuiModelHelper(@NotNull SeenHistoryController historyController,
                             @NotNull SearchFieldData searchFieldData,
                             @NotNull FilterConfiguration filterConfiguration) {
        this.historyController = historyController;
        this.searchFieldData = searchFieldData;
        this.filterConfiguration = filterConfiguration;
    }

    /**
     * Filter the filmlist.
     *
     * @return the filtered table model.
     */
    public abstract TableModel getFilteredTableModel();

    protected boolean maxLengthCheck(DatenFilm film) {
        return film.getFilmLength() < maxLengthInSeconds;
    }

    protected boolean minLengthCheck(DatenFilm film) {
        var filmLength = film.getFilmLength();
        if (filmLength == 0)
            return true; // always show entries with length 0, which are internally "no length"
        else
            return filmLength >= minLengthInSeconds;
    }

    public Stream<DatenFilm> applyCommonFilters(Stream<DatenFilm> stream, final String filterThema) {
        if (!filterThema.isEmpty()) {
            stream = stream.filter(film -> film.getThema().equalsIgnoreCase(filterThema));
        }
        if (maxLengthInSeconds < UNLIMITED_LENGTH_IN_SECONDS) {
            stream = stream.filter(this::maxLengthCheck);
        }
        if (filterConfiguration.isShowUnseenOnly()) {
            stream = stream.filter(this::seenCheck);
        }
        //perform min length filtering after all others may have reduced the available entries...
        return stream.filter(this::minLengthCheck);
    }

    protected boolean noFiltersAreSet() {
        return filterConfiguration.noFiltersAreSet() && searchFieldData.isEmpty();
    }

    protected boolean seenCheck(DatenFilm film) {
        return !historyController.hasBeenSeenFromCache(film);
    }

    /**
     * Convert slider values for faster user later.
     */
    protected void calculateFilmLengthSliderValues() {
        minLengthInSeconds = TimeUnit.SECONDS.convert((long)filterConfiguration.getFilmLengthMin(), TimeUnit.MINUTES);
        maxLengthInSeconds = TimeUnit.SECONDS.convert((long)filterConfiguration.getFilmLengthMax(), TimeUnit.MINUTES);
    }
}
