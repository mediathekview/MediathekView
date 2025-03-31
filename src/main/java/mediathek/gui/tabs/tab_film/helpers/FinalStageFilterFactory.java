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

import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPattern;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStageFilterNoPatternWithDescription;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilter;
import mediathek.gui.tabs.tab_film.searchfilters.FinalStagePatternFilterWithDescription;
import mediathek.tool.Filter;
import org.jetbrains.annotations.NotNull;

import java.util.function.Predicate;

class FinalStageFilterFactory {
    public static Predicate<DatenFilm> createFinalStageFilter(boolean searchThroughDescription,
                                                              @NotNull String[] arrIrgendwo) {
        //if arrIrgendwo contains more than one search fields fall back to "old" pattern search
        //otherwise use more optimized search
        boolean isPattern = Filter.isPattern(arrIrgendwo[0]) || arrIrgendwo.length > 1;
        Predicate<DatenFilm> filter;
        if (searchThroughDescription) {
            if (isPattern)
                filter = new FinalStagePatternFilterWithDescription(arrIrgendwo);
            else
                filter = new FinalStageFilterNoPatternWithDescription(arrIrgendwo);
        } else {
            if (isPattern)
                filter = new FinalStagePatternFilter(arrIrgendwo);
            else
                filter = new FinalStageFilterNoPattern(arrIrgendwo);
        }
        return filter;
    }
}
