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

import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.daten.FilmResolution
import mediathek.gui.tabs.tab_film.startDownloads
import mediathek.mainwindow.MediathekGui
import mediathek.tool.NoSelectionErrorDialog
import mediathek.tool.table.MVFilmTable
import java.awt.Component
import java.util.Optional

class FilmSelectionController(private val host: Host) {
    interface Host {
        fun table(): MVFilmTable
        fun parentComponent(): Component
        fun mediathekGui(): MediathekGui
        fun daten(): Daten
        fun showHighQualityOnly(): Boolean
    }

    fun getTableRowCount(): Int = host.table().model.rowCount

    @Synchronized
    fun saveFilm(pSet: DatenPset?) {
        val requestedResolution = if (host.showHighQualityOnly()) {
            FilmResolution.Enum.HIGH_QUALITY
        } else {
            null
        }
        startDownloads(host.mediathekGui(), getSelectedFilms(), pSet, requestedResolution)
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
                host.daten().downloadStartCoordinator.urlMitProgrammStarten(pSet, film, resolution)
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
        host.mediathekGui().filmInfoDialog?.let { infoDialog ->
            getCurrentlySelectedFilm().ifPresent(infoDialog::updateCurrentFilm)
        }
    }

    private fun filmAtModelRow(modelRow: Int): DatenFilm {
        return host.table().model.getValueAt(modelRow, DatenFilm.FILM_REF) as DatenFilm
    }
}
