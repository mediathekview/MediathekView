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
import mediathek.daten.ProgramSetRepository
import java.util.*
import javax.swing.JMenu
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

class FilmContextMenuBuilder(
    private val host: TableContextMenuHandler.Host,
    private val programSets: ProgramSetRepository,
    private val aboAndBlacklistContextActions: FilmAboAndBlacklistContextActions,
    private val filmSpecificContextMenuBuilder: FilmSpecificContextMenuBuilder,
    private val addPrintAndInfoActions: (JPopupMenu, Optional<DatenFilm>) -> Unit,
    private val addFileAndDuplicateActions: (JPopupMenu, DatenFilm) -> Unit,
) {
    fun createContextMenu(selectedFilm: Optional<DatenFilm>): JPopupMenu =
        JPopupMenu().apply {
            addPrimaryContextActions(this, selectedFilm)
            addFilmProgramsMenu(this)
            aboAndBlacklistContextActions.addBlacklistMenu(this)
            selectedFilm.ifPresent { film -> filmSpecificContextMenuBuilder.addFilmSpecificContextActions(this, film) }
            addPrintAndInfoActions(this, selectedFilm)
            selectedFilm.ifPresent { film -> addFileAndDuplicateActions(this, film) }
        }

    private fun addPrimaryContextActions(popupMenu: JPopupMenu, selectedFilm: Optional<DatenFilm>) {
        val actions = host.actions()
        popupMenu.add(actions.playFilm)
        popupMenu.add(actions.saveFilm)

        val bookmarkMenuItem = JMenuItem(actions.bookmarkAddFilm)
        popupMenu.add(bookmarkMenuItem)
        popupMenu.addSeparator()
        aboAndBlacklistContextActions.addAboMenu(popupMenu, selectedFilm)
        updateBookmarkMenuItem(popupMenu, bookmarkMenuItem, selectedFilm)
    }

    private fun updateBookmarkMenuItem(
        popupMenu: JPopupMenu,
        bookmarkMenuItem: JMenuItem,
        selectedFilm: Optional<DatenFilm>,
    ) {
        selectedFilm.ifPresent { film ->
            if (film.isLivestream) {
                popupMenu.remove(bookmarkMenuItem)
            } else {
                bookmarkMenuItem.text = if (film.isBookmarked) {
                    "Film aus Merkliste entfernen"
                } else {
                    "Film merken"
                }
            }
        }
    }

    private fun addFilmProgramsMenu(popupMenu: JPopupMenu) {
        val submenu = JMenu("Film mit Set starten")
        popupMenu.add(submenu)
        val liste = programSets.list.listeButton
        for (pset in liste) {
            if (pset.listeProg.isEmpty() && pset.name.isEmpty()) {
                continue
            }

            val item = JMenuItem(pset.name)
            pset.foregroundColor?.let(item::setForeground)
            if (pset.listeProg.isNotEmpty()) {
                item.addActionListener { host.startFilmWithPset(pset) }
            }
            submenu.add(item)
        }
    }
}
