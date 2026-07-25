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
import mediathek.gui.tabs.tab_film.table.FilmTableModelBinding
import mediathek.tool.NoSelectionErrorDialog
import java.awt.Component
import java.util.*

class FilmSelectionController(private val host: Host) {
    interface Host {
        fun tableBinding(): FilmTableModelBinding
        fun parentComponent(): Component
        fun saveFilms(films: List<DatenFilm>, pSet: DatenPset?, requestedResolution: FilmResolution.Enum?)
        fun startFilmWithProgram(pSet: DatenPset, film: DatenFilm, resolution: String)
        fun showHighQualityOnly(): Boolean
        fun updateCurrentFilm(film: DatenFilm?)
    }

    fun getTableRowCount(): Int = host.tableBinding().rowCount

    @Synchronized
    fun saveFilm(pSet: DatenPset?) {
        val requestedResolution = if (host.showHighQualityOnly()) {
            FilmResolution.Enum.HIGH_QUALITY
        } else {
            null
        }
        host.saveFilms(getSelectedFilms(), pSet, requestedResolution)
    }

    fun startFilm(pSet: DatenPset) {
        if (host.tableBinding().table.selectedRow == -1) {
            NoSelectionErrorDialog.show(host.parentComponent())
        } else if (pSet.istSpeichern()) {
            saveFilm(pSet)
        } else {
            val resolution = if (host.showHighQualityOnly()) {
                FilmResolution.Enum.HIGH_QUALITY.toString()
            } else {
                ""
            }

            getCurrentlySelectedFilm().ifPresent { film ->
                host.startFilmWithProgram(pSet, film, resolution)
            }
        }
    }

    fun getFilm(tableRow: Int): Optional<DatenFilm> {
        return Optional.ofNullable(host.tableBinding().filmAtViewRow(tableRow))
    }

    fun getCurrentlySelectedFilm(): Optional<DatenFilm> {
        val selectedTableRow = host.tableBinding().table.selectedRow
        return if (selectedTableRow != -1) {
            Optional.ofNullable(host.tableBinding().filmAtViewRow(selectedTableRow))
        } else {
            Optional.empty()
        }
    }

    fun getSelectedFilms(): List<DatenFilm> {
        val films = host.tableBinding().selectedFilms()
        if (films.isEmpty()) {
            NoSelectionErrorDialog.show(host.parentComponent())
        }
        return films
    }

    fun updateFilmData() {
        host.updateCurrentFilm(getCurrentlySelectedFilm().orElse(null))
    }
}
