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

package mediathek.mainwindow

import mediathek.controller.starter.DownloadServices
import mediathek.filmlisten.FilmCatalog
import mediathek.swing.FilmAgeLabel
import org.jdesktop.swingx.JXStatusBar
import java.awt.Component
import javax.swing.JPanel
import kotlin.time.Duration.Companion.seconds

/**
 * This class tries to fix some redraw issues with JXStatusBar on removal.
 */
class FixedRedrawStatusBar(
    private val filmCatalog: FilmCatalog,
    downloads: DownloadServices,
    filmTableRowCount: FilmTableRowCountProperty,
    selectedListItemsProperty: ListSelectedItemsProperty,
) : JXStatusBar() {
    init {
        add(SelectedListItemsLabel(selectedListItemsProperty))
        add(FilmSizeInfoLabel(filmCatalog, filmTableRowCount))
        add(DownloadInformationLabel(downloads))

        add(JPanel(), Constraint(Constraint.ResizeBehavior.FILL))
        add(FilmListCreationDateLabel(filmCatalog))
        add(
            FilmAgeLabel(
                ageProvider = {
                    filmCatalog.allFilms
                        .metaData
                        .ageInSeconds
                        .coerceAtLeast(0)
                        .seconds
                },
                tooltip = "Alter der Filmliste"
            )
        )
    }

    override fun remove(index: Int) {
        super.remove(index)
        revalidate()
    }

    override fun remove(comp: Component?) {
        super.remove(comp)
        revalidate()
    }

    override fun removeAll() {
        super.removeAll()
        revalidate()
    }
}
