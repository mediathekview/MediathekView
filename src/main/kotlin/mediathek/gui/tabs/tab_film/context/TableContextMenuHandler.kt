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

import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.SupervisorJob
import kotlinx.coroutines.swing.Swing
import mediathek.config.Daten
import mediathek.daten.DatenFilm
import mediathek.daten.DatenPset
import mediathek.gui.tabs.tab_film.JDownloadHelper
import mediathek.gui.tabs.tab_film.PyLoadHelper
import mediathek.gui.tabs.tab_film.actions.FilmUiActions
import mediathek.gui.tabs.tab_film.table.FilmTableButtonClickHandler
import mediathek.mainwindow.MediathekGui
import mediathek.tool.table.MVFilmTable
import java.awt.Point
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import java.util.*

/**
 * Implements the context menu for tab film.
 */
class TableContextMenuHandler(
    private val host: Host,
) : MouseAdapter() {
    interface Host {
        fun table(): MVFilmTable
        fun getCurrentlySelectedFilm(): Optional<DatenFilm>
        fun getFilm(row: Int): Optional<DatenFilm>
        fun playSelectedFilm()
        fun saveSelectedFilm()
        fun startFilmWithPset(pSet: DatenPset)
        fun setSelectionUpdatesSuspended(suspended: Boolean)
        fun gui(): MediathekGui
        fun actions(): FilmUiActions
    }

    private val daten = Daten.getInstance()
    private val uiScope = CoroutineScope(SupervisorJob() + Dispatchers.Swing)
    private val filmTableButtonClickHandler = FilmTableButtonClickHandler(host, daten)
    private val filmAboAndBlacklistContextActions =
        FilmAboAndBlacklistContextActions(host, daten, this::selectedFilmAtPopupPoint)
    private val jDownloadHelper = JDownloadHelper(host.gui())
    private val pyLoadHelper = PyLoadHelper(host.gui())
    private val filmSpecificContextMenuBuilder = FilmSpecificContextMenuBuilder(host, jDownloadHelper, pyLoadHelper)
    private val filmFileAndDuplicateContextActions = FilmFileAndDuplicateContextActions(host, daten, uiScope)
    private val filmPrintAndHistoryContextActions =
        FilmPrintAndHistoryContextActions(host, this::selectedFilmAtPopupPoint)
    private val contextMenuBuilder = FilmContextMenuBuilder(
        host,
        filmAboAndBlacklistContextActions,
        filmSpecificContextMenuBuilder,
        filmPrintAndHistoryContextActions::addActions,
        filmFileAndDuplicateContextActions::addActions,
    )
    private var popupPoint: Point? = null
    private var pressedButtonCell: ButtonCell? = null

    override fun mouseClicked(event: MouseEvent) {
        if (event.button == MouseEvent.BUTTON1) {
            if (event.clickCount > 1) {
                host.gui().filmInfoDialog?.let { infoDialog ->
                    if (!infoDialog.isVisible) {
                        infoDialog.showInfo()
                    }
                }
            }
        }
    }

    override fun mousePressed(event: MouseEvent) {
        if (event.isPopupTrigger) {
            pressedButtonCell = null
            showMenu(event)
            return
        }

        pressedButtonCell = event.buttonCell()
    }

    override fun mouseReleased(event: MouseEvent) {
        if (event.isPopupTrigger) {
            pressedButtonCell = null
            showMenu(event)
            return
        }

        val buttonCell = pressedButtonCell
        pressedButtonCell = null
        if (buttonCell != null && buttonCell == event.buttonCell()) {
            filmTableButtonClickHandler.handleButtonClick(buttonCell.row, buttonCell.column)
        }
    }

    private fun showMenu(event: MouseEvent) {
        popupPoint = event.point
        val point = popupPoint ?: return
        val row = host.table().rowAtPoint(point)
        if (row < 0) {
            return
        }
        host.table().setRowSelectionInterval(row, row)

        val popupMenu = contextMenuBuilder.createContextMenu(host.getFilm(row))
        popupMenu.show(event.component, event.x, event.y)
    }

    private fun selectedFilmAtPopupPoint(): DatenFilm? {
        val point = popupPoint ?: return null
        val row = host.table().rowAtPoint(point)
        if (row == -1) {
            return null
        }
        return host.getFilm(row).orElse(null)
    }

    private fun MouseEvent.buttonCell(): ButtonCell? {
        if (button != MouseEvent.BUTTON1 || clickCount != 1) {
            return null
        }

        val row = host.table().rowAtPoint(point)
        if (row < 0) {
            return null
        }

        val column = host.table().columnAtPoint(point)
        return if (filmTableButtonClickHandler.isButtonColumn(column)) {
            ButtonCell(row, column)
        } else {
            null
        }
    }

    private data class ButtonCell(val row: Int, val column: Int)
}
