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

import mediathek.config.Daten
import mediathek.controller.starter.StartStatus
import mediathek.daten.DatenDownload
import mediathek.daten.DatenFilm
import mediathek.swing.IconUtils
import mediathek.tool.ApplicationConfiguration
import mediathek.tool.table.MVTable
import org.apache.logging.log4j.LogManager
import org.kordamp.ikonli.fontawesome6.FontAwesomeSolid
import org.kordamp.ikonli.swing.FontIcon
import java.awt.Color
import java.awt.Component
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import javax.swing.JTable
import javax.swing.SwingConstants

class CellRendererFilme : CellRendererBaseWithStart() {
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
    private val selectedBookmarkIconHighlighted = FontIcon.of(FontAwesomeSolid.BOOKMARK, IconUtils.DEFAULT_SIZE, Color.ORANGE)

    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int
    ): Component {
        try {
            resetComponent()
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)

            val rowModelIndex = table.convertRowIndexToModel(row)
            val columnModelIndex = table.convertColumnIndexToModel(column)
            val datenFilm = table.model.getValueAt(rowModelIndex, DatenFilm.FILM_REF) as DatenFilm
            val mvTable = table as MVTable

            if (mvTable.isLineBreak()) {
                horizontalAlignment = SwingConstants.LEFT
                verticalAlignment = SwingConstants.TOP

                when (columnModelIndex) {
                    DatenFilm.FILM_THEMA,
                    DatenFilm.FILM_TITEL,
                    DatenFilm.FILM_URL,
                        -> return createWrappedTextArea(valueText(value), useLabelFont = true)
                }
            } else {
                applyHorizontalAlignment(columnModelIndex)
            }

            when (columnModelIndex) {
                DatenFilm.FILM_DAUER -> text = datenFilm.filmLengthAsString
                DatenFilm.FILM_ABSPIELEN -> {
                    val datenDownload = Daten.getInstance()
                        .listeDownloadsButton
                        .getDownloadUrlFilm(datenFilm.urlNormalQuality)
                    handleButtonStartColumn(datenDownload, isSelected)
                }

                DatenFilm.FILM_AUFZEICHNEN -> handleButtonDownloadColumn(isSelected)
                DatenFilm.FILM_MERKEN -> handleButtonBookmarkColumn(
                    datenFilm.isBookmarked,
                    isSelected,
                    datenFilm.isLivestream
                )

                DatenFilm.FILM_SENDER -> {
                    if (mvTable.showSenderIcons()) {
                        val targetDim = getSenderCellDimension(table, row, column)
                        setSenderIcon(valueText(value), targetDim, isSelected)
                    }
                }

                DatenFilm.FILM_TITEL -> {
                    text = datenFilm.title
                    setIndicatorIcons(table, datenFilm, isSelected)
                }

                DatenFilm.FILM_GEO -> drawGeolocationIcons(datenFilm, isSelected)
                DatenFilm.FILM_ZEIT -> drawTime(datenFilm)
            }
        } catch (ex: Exception) {
            logger.error("Fehler", ex)
        }

        return this
    }

    private fun drawTime(film: DatenFilm) {
        var zeit = film.sendeZeit
        if (zeit == null || zeit.isBlank()) {
            text = ""
            return
        }

        zeit = zeit.trim()
        try {
            val time = LocalTime.parse(zeit, PARSER)
            val longFormat = ApplicationConfiguration.getConfiguration()
                .getBoolean(ApplicationConfiguration.UI_TAB_FILME_TIME_USE_LONG_FORMAT, false)
            text = (if (longFormat) LONG else SHORT).format(time)
        } catch (_: DateTimeParseException) {
            text = zeit
        }
    }

    private fun applyHorizontalAlignment(columnModelIndex: Int) {
        when (columnModelIndex) {
            DatenFilm.FILM_NR,
            DatenFilm.FILM_DATUM,
            DatenFilm.FILM_ZEIT,
            DatenFilm.FILM_DAUER,
            DatenFilm.FILM_ABSPIELEN,
            DatenFilm.FILM_AUFZEICHNEN,
            DatenFilm.FILM_MERKEN,
                -> horizontalAlignment = SwingConstants.CENTER

            DatenFilm.FILM_GROESSE -> horizontalAlignment = SwingConstants.RIGHT
        }
    }

    private fun handleButtonStartColumn(datenDownload: DatenDownload?, isSelected: Boolean) {
        if (datenDownload?.runtime?.runState?.status == StartStatus.RUNNING) {
            setSelectedIconAndToolTip(isSelected, stopIcons, "Film stoppen")
        }

        if (icon == null) {
            setSelectedIconAndToolTip(isSelected, playIcons, "Film abspielen")
        }
    }

    private fun handleButtonDownloadColumn(isSelected: Boolean) {
        setSelectedIconAndToolTip(isSelected, downloadIcons, "Film aufzeichnen")
    }

    private fun handleButtonBookmarkColumn(isBookMarked: Boolean, isSelected: Boolean, isLivestream: Boolean) {
        if (isLivestream) {
            icon = null
            toolTipText = ""
            return
        }

        toolTipText = if (isBookMarked) "Film aus Merkliste entfernen" else "Film merken"
        icon = when {
            isBookMarked -> selectedBookmarkIconHighlighted
            else -> bookmarkIcons.icon(isSelected)
        }
    }

    private companion object {
        private val logger = LogManager.getLogger(CellRendererFilme::class.java)
        private val PARSER: DateTimeFormatter = DateTimeFormatter.ofPattern("H:mm[:ss]")
        private val SHORT: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")
        private val LONG: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
    }
}
