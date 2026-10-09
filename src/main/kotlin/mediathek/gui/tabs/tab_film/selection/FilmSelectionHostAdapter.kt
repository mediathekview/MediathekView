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

package mediathek.gui.tabs.tab_film.selection

import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.FilmResolution
import mediathek.tool.table.MVFilmTable
import java.awt.Component
import java.util.function.Consumer

class FilmSelectionHostAdapter(
    private val tableProvider: () -> MVFilmTable,
    private val parentComponent: Component,
    private val saveFilmsAction: (List<DatenFilm>, DatenPset?, FilmResolution.Enum?) -> Unit,
    private val startFilmWithProgramAction: (DatenPset, DatenFilm, String) -> Unit,
    private val showHighQualityOnlyProvider: () -> Boolean,
    private val currentFilm: Consumer<DatenFilm?>,
) : FilmSelectionController.Host {
    override fun table(): MVFilmTable = tableProvider()

    override fun parentComponent(): Component = parentComponent

    override fun saveFilms(films: List<DatenFilm>, pSet: DatenPset?, requestedResolution: FilmResolution.Enum?) {
        saveFilmsAction(films, pSet, requestedResolution)
    }

    override fun startFilmWithProgram(pSet: DatenPset, film: DatenFilm, resolution: String) {
        startFilmWithProgramAction(pSet, film, resolution)
    }

    override fun showHighQualityOnly(): Boolean = showHighQualityOnlyProvider()

    override fun updateCurrentFilm(film: DatenFilm?) {
        currentFilm.accept(film)
    }
}
