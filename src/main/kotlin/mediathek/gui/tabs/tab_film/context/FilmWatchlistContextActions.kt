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

package mediathek.gui.tabs.tab_film.context

import mediathek.daten.DatenFilm
import mediathek.daten.watchlist.WatchlistServices
import java.util.*
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

class FilmWatchlistContextActions(
    private val watchlist: WatchlistServices,
) {
    fun addWatchlistMenu(popupMenu: JPopupMenu, selectedFilm: Optional<DatenFilm>) {
        val submenu = JMenu("Watchlist")
        popupMenu.add(submenu)

        val itemWithoutTitle = JMenuItem()
        val itemWithTitle = JMenuItem()

        selectedFilm.ifPresentOrElse(
            { film ->
                configureItem(itemWithoutTitle, film, withTitle = false)
                configureItem(itemWithTitle, film, withTitle = true)
            },
            {
                itemWithoutTitle.text = ADD_WITHOUT_TITLE_LABEL
                itemWithoutTitle.isEnabled = false
                itemWithTitle.text = ADD_WITH_TITLE_LABEL
                itemWithTitle.isEnabled = false
            },
        )

        submenu.add(itemWithoutTitle)
        submenu.add(itemWithTitle)
    }

    private fun configureItem(item: JMenuItem, film: DatenFilm, withTitle: Boolean) {
        val existingEntry = watchlist.findEntryFor(film, withTitle)
        if (existingEntry != null) {
            item.text = if (withTitle) REMOVE_WITH_TITLE_LABEL else REMOVE_WITHOUT_TITLE_LABEL
            item.addActionListener { watchlist.removeEntry(existingEntry.id) }
        } else {
            item.text = if (withTitle) ADD_WITH_TITLE_LABEL else ADD_WITHOUT_TITLE_LABEL
            // Use the film the menu was built for; re-resolving by table coordinate can hit another row.
            item.addActionListener { watchlist.addEntryFromFilm(film, withTitle) }
        }
    }

    private companion object {
        private const val ADD_WITHOUT_TITLE_LABEL = "Sendung auf Watchlist setzen"
        private const val ADD_WITH_TITLE_LABEL = "Sendung mit Titel auf Watchlist setzen"
        private const val REMOVE_WITHOUT_TITLE_LABEL = "Sendung von Watchlist entfernen"
        private const val REMOVE_WITH_TITLE_LABEL = "Sendung mit Titel von Watchlist entfernen"
    }
}
