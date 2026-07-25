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

package mediathek.gui.tabs.tab_film

import mediathek.controller.SenderFilmlistLoadApprover
import mediathek.filmlisten.FilmCatalog
import mediathek.gui.tabs.tab_film.filter.FilmFilterController

class FilmFilterDataProviderAdapter(
    private val filmCatalog: FilmCatalog,
) : FilmFilterController.DataProvider {
    override fun senderList(): List<String> =
        filmCatalog.allSenders.filter(SenderFilmlistLoadApprover::isApproved)

    override fun getThemen(senders: Collection<String>): List<String> =
        filmCatalog.filteredFilms.getThemen(senders)

    override fun hasFilmData(): Boolean = filmCatalog.filteredFilms.isNotEmpty()
}
