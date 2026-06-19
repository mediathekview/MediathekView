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
import mediathek.tool.table.MVFilmTable
import java.awt.Component
import java.util.function.Consumer
import javax.swing.JFrame

class FilmSelectionHostAdapter(
    private val tableProvider: () -> MVFilmTable,
    private val parentComponent: Component,
    private val downloadParent: JFrame,
    private val datenProvider: () -> Daten,
    private val showHighQualityOnlyProvider: () -> Boolean,
    private val currentFilm: Consumer<DatenFilm?>,
) : FilmSelectionController.Host {
    override fun table(): MVFilmTable = tableProvider()

    override fun parentComponent(): Component = parentComponent

    override fun downloadParent(): JFrame = downloadParent

    override fun daten(): Daten = datenProvider()

    override fun showHighQualityOnly(): Boolean = showHighQualityOnlyProvider()

    override fun updateCurrentFilm(film: DatenFilm?) {
        currentFilm.accept(film)
    }
}
