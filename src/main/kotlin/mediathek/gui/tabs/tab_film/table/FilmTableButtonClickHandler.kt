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

package mediathek.gui.tabs.tab_film.table

import mediathek.config.Daten
import mediathek.controller.starter.StartStatus
import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.context.TableContextMenuHandler

class FilmTableButtonClickHandler(
    private val host: TableContextMenuHandler.Host,
    private val daten: Daten,
) {
    fun isButtonColumn(column: Int): Boolean {
        if (column < 0) {
            return false
        }

        return when (host.table().convertColumnIndexToModel(column)) {
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN,
                -> true

            else -> false
        }
    }

    fun handleButtonClick(row: Int, column: Int) {
        if (row == -1) {
            return
        }

        when (host.table().convertColumnIndexToModel(column)) {
            DatenFilm.FILM_ABSPIELEN -> host.getCurrentlySelectedFilm().ifPresent { film ->
                var dontPlay = false
                val download = daten.listeDownloadsButton.getDownloadUrlFilm(film.urlNormalQuality)
                if (download != null && download.start != null && download.start.status == StartStatus.RUNNING) {
                    dontPlay = true
                    daten.listeDownloadsButton.delDownloadButton(film.urlNormalQuality)
                }
                if (!dontPlay) {
                    host.playSelectedFilm()
                }
            }

            DatenFilm.FILM_AUFZEICHNEN -> host.saveSelectedFilm()
            DatenFilm.FILM_MERKEN -> host.getCurrentlySelectedFilm().ifPresent { film ->
                if (!film.isLivestream) {
                    if (film.isBookmarked) {
                        host.actions().bookmarkRemoveFilm.actionPerformed(null)
                    } else {
                        host.actions().bookmarkAddFilm.actionPerformed(null)
                    }
                }
            }
        }
    }
}
