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

package mediathek.tool.cellrenderer

import mediathek.controller.starter.DownloadServices
import mediathek.controller.starter.StartStatus
import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.table.FilmTableAppearance
import mediathek.swing.IconUtils
import mediathek.tool.models.FilmColumn
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Color
import java.awt.Component
import javax.swing.JTable

internal class FilmActionCellRenderer(
    private val downloads: DownloadServices,
    appearance: FilmTableAppearance,
) : FilmCellRenderer(appearance) {
    private val stopIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeSolid.STOP),
        selected = FontIcon.of(FontAwesomeSolid.STOP, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )
    private val downloadIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeSolid.DOWNLOAD),
        selected = FontIcon.of(FontAwesomeSolid.DOWNLOAD, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )
    private val playIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeSolid.PLAY),
        selected = FontIcon.of(FontAwesomeSolid.PLAY, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )
    private val bookmarkIcons = rendererIconPair(
        normal = IconUtils.of(FontAwesomeSolid.BOOKMARK),
        selected = FontIcon.of(FontAwesomeSolid.BOOKMARK, IconUtils.DEFAULT_SIZE, Color.WHITE),
    )
    private val selectedBookmarkIconHighlighted =
        FontIcon.of(FontAwesomeSolid.BOOKMARK, IconUtils.DEFAULT_SIZE, Color.ORANGE)

    override fun renderFilmCell(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int,
        filmColumn: FilmColumn,
        film: DatenFilm,
    ): Component {
        require(filmColumn in ACTION_COLUMNS) { "Unsupported film action column: $filmColumn" }
        if (!appearance.lineBreak) horizontalAlignment = CENTER
        when (filmColumn) {
            FilmColumn.PLAY -> renderPlayAction(film, isSelected)
            FilmColumn.SAVE -> setSelectedIconAndToolTip(isSelected, downloadIcons, "Film aufzeichnen")
            FilmColumn.BOOKMARK -> renderBookmarkAction(film, isSelected)
            else -> error("Unsupported film action column: $filmColumn")
        }
        return this
    }

    private fun renderPlayAction(film: DatenFilm, isSelected: Boolean) {
        val download = downloads.findButtonDownloadByFilmUrl(film.urlNormalQuality)
        if (download?.runtime?.runState?.status == StartStatus.RUNNING) {
            setSelectedIconAndToolTip(isSelected, stopIcons, "Film stoppen")
        } else {
            setSelectedIconAndToolTip(isSelected, playIcons, "Film abspielen")
        }
    }

    private fun renderBookmarkAction(film: DatenFilm, isSelected: Boolean) {
        if (film.isLivestream) {
            toolTipText = ""
            return
        }

        toolTipText = if (film.isBookmarked) "Film aus Merkliste entfernen" else "Film merken"
        icon = if (film.isBookmarked) selectedBookmarkIconHighlighted else bookmarkIcons.icon(isSelected)
    }

    private companion object {
        private val ACTION_COLUMNS = setOf(FilmColumn.PLAY, FilmColumn.SAVE, FilmColumn.BOOKMARK)
    }
}
