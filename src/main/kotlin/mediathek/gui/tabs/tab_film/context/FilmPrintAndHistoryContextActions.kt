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
import mediathek.gui.tabs.actions.MarkSingleFilmAsSeenAction
import mediathek.gui.tabs.actions.MarkSingleFilmAsUnseenAction
import mediathek.gui.tabs.actions.hasBeenSeenInHistory
import org.apache.logging.log4j.LogManager
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.awt.print.PrinterException
import java.util.*
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

class FilmPrintAndHistoryContextActions(
    private val host: TableContextMenuHandler.Host,
    private val selectedFilmAtPopupPoint: () -> DatenFilm?,
) {
    private val unseenAction = MarkSingleFilmAsUnseenAction { selectedFilmAtPopupPoint() }
    private val seenAction = MarkSingleFilmAsSeenAction { selectedFilmAtPopupPoint() }
    private val printActionListener = PrintActionListener()

    fun addActions(popupMenu: JPopupMenu, selectedFilm: Optional<DatenFilm>) {
        val printTableMenuItem = JMenuItem("Tabelle drucken")
        printTableMenuItem.addActionListener(printActionListener)
        popupMenu.add(printTableMenuItem)

        popupMenu.add(host.actions().showFilmInformation)
        selectedFilm.ifPresent { film -> setupHistoryContextActions(popupMenu, film) }
    }

    private fun setupHistoryContextActions(popupMenu: JPopupMenu, film: DatenFilm) {
        if (!film.isLivestream) {
            val historyMenuItem = if (hasBeenSeenInHistory(film)) {
                JMenuItem(unseenAction)
            } else {
                JMenuItem(seenAction)
            }
            popupMenu.add(historyMenuItem)
        }
    }

    private inner class PrintActionListener : ActionListener {
        override fun actionPerformed(event: ActionEvent?) {
            try {
                host.table().print()
            } catch (ex: PrinterException) {
                logger.error(ex)
            }
        }
    }

    private companion object {
        private val logger = LogManager.getLogger()
    }
}
