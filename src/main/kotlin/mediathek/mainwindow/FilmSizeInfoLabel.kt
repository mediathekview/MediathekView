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

import mediathek.filmlisten.FilmCatalog
import java.beans.PropertyChangeListener
import javax.swing.JLabel
import javax.swing.SwingUtilities

class FilmSizeInfoLabel(
    private val filmCatalog: FilmCatalog,
    private val filmTableRowCount: FilmTableRowCountProperty,
) : JLabel() {
    private var oldGesamt = 0
    private var oldRowCount = 0
    private val rowCountListener = PropertyChangeListener { event ->
        dispatchUpdate(event.newValue as Int)
    }

    override fun addNotify() {
        super.addNotify()
        filmTableRowCount.addListener(rowCountListener)
        updateValues(filmTableRowCount.rowCount)
    }

    override fun removeNotify() {
        filmTableRowCount.removeListener(rowCountListener)
        super.removeNotify()
    }

    internal fun updateDisplayedFilmCount(rowCount: Int) {
        updateValues(rowCount)
    }

    private fun dispatchUpdate(rowCount: Int) {
        if (SwingUtilities.isEventDispatchThread()) {
            updateValues(rowCount)
        } else {
            SwingUtilities.invokeLater { updateValues(rowCount) }
        }
    }

    private fun updateValues(rowCount: Int) {
        val gesamt = filmCatalog.allFilms.size

        if (gesamt == oldGesamt && rowCount == oldRowCount) {
            return
        }

        val textLinks = if (gesamt == rowCount) {
            createFilmLabel(rowCount)
        } else {
            "${createFilmLabel(rowCount)} (Insgesamt: $gesamt)"
        }

        text = textLinks

        oldGesamt = gesamt
        oldRowCount = rowCount
    }

    private fun createFilmLabel(rowCount: Int): String = if (rowCount == 1) {
        "1 Film"
    } else {
        "$rowCount Filme"
    }
}
