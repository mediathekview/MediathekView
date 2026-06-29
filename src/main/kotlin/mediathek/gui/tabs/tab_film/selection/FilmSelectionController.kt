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
import mediathek.tool.NoSelectionErrorDialog
import mediathek.tool.table.MVFilmTable
import java.awt.Component
import java.util.*

class FilmSelectionController(private val host: Host) {
    interface Host {
        fun table(): MVFilmTable
        fun parentComponent(): Component
        fun saveFilms(films: List<DatenFilm>, pSet: DatenPset?, requestedResolution: FilmResolution.Enum?)
        fun startFilmWithProgram(pSet: DatenPset, film: DatenFilm, resolution: String)
        fun showHighQualityOnly(): Boolean
        fun updateCurrentFilm(film: DatenFilm?)
    }

    fun getTableRowCount(): Int = host.table().model.rowCount

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
        if (host.table().selectedRow == -1) {
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
        return if (tableRow >= 0 && tableRow < host.table().rowCount) {
            Optional.of(filmAtModelRow(host.table().convertRowIndexToModel(tableRow)))
        } else {
            Optional.empty()
        }
    }

    fun getCurrentlySelectedFilm(): Optional<DatenFilm> {
        val selectedTableRow = host.table().selectedRow
        return if (selectedTableRow != -1) {
            try {
                Optional.of(filmAtModelRow(host.table().convertRowIndexToModel(selectedTableRow)))
            } catch (_: Exception) {
                Optional.empty()
            }
        } else {
            Optional.empty()
        }
    }

    fun getSelectedFilms(): List<DatenFilm> {
        val films = ArrayList<DatenFilm>()
        val rows = host.table().selectedRows
        if (rows.isNotEmpty()) {
            for (row in rows) {
                films.add(filmAtModelRow(host.table().convertRowIndexToModel(row)))
            }
        } else {
            NoSelectionErrorDialog.show(host.parentComponent())
        }
        return films
    }

    fun updateFilmData() {
        host.updateCurrentFilm(getCurrentlySelectedFilm().orElse(null))
    }

    private fun filmAtModelRow(modelRow: Int): DatenFilm {
        return host.table().model.getValueAt(modelRow, DatenFilm.FILM_REF) as DatenFilm
    }
}
