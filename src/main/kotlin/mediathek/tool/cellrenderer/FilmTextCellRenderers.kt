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

import mediathek.config.application.ApplicationConfiguration
import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.table.FilmTableAppearance
import mediathek.tool.models.FilmColumn
import java.awt.Component
import java.awt.Graphics
import java.awt.Graphics2D
import java.awt.Rectangle
import java.time.LocalTime
import java.time.format.DateTimeFormatter
import java.time.format.DateTimeParseException
import javax.swing.JTable

internal class FilmTextCellRenderer(
    appearance: FilmTableAppearance,
) : FilmCellRenderer(appearance) {
    override fun renderFilmCell(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int,
        filmColumn: FilmColumn,
        film: DatenFilm,
    ): Component {
        if (appearance.lineBreak && filmColumn in WRAPPED_COLUMNS) {
            horizontalAlignment = LEFT
            verticalAlignment = TOP
            return createWrappedTextArea(valueText(value), useLabelFont = true)
        }

        if (!appearance.lineBreak && (filmColumn == FilmColumn.NUMBER || filmColumn == FilmColumn.DATE)) {
            horizontalAlignment = CENTER
        }
        return this
    }

    private companion object {
        private val WRAPPED_COLUMNS = setOf(FilmColumn.TOPIC, FilmColumn.URL)
    }
}

internal class FilmTitleCellRenderer(
    appearance: FilmTableAppearance,
) : FilmCellRenderer(appearance) {
    override fun renderFilmCell(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int,
        filmColumn: FilmColumn,
        film: DatenFilm,
    ): Component {
        if (appearance.lineBreak) {
            horizontalAlignment = LEFT
            verticalAlignment = TOP
            return createWrappedTextArea(valueText(value), useLabelFont = true).apply {
                toolTipText = film.title.takeIf {
                    preferredSize.width > table.getCellRect(row, column, false).width
                }
            }
        }

        text = film.title
        setIndicatorIcons(table, film, isSelected)
        toolTipText = film.title.takeIf {
            preferredSize.width > table.getCellRect(row, column, false).width
        }
        return this
    }
}

internal class FilmSenderCellRenderer(
    appearance: FilmTableAppearance,
) : FilmCellRenderer(appearance) {
    private var arteLocaleCode: String? = null

    override fun renderFilmCell(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int,
        filmColumn: FilmColumn,
        film: DatenFilm,
    ): Component {
        arteLocaleCode = null
        if (appearance.showSenderIcons) {
            setSenderIcon(film.sender, getSenderCellDimension(table, row, column), isSelected)
            arteLocaleCode = ArteLocaleBadge.localeCode(film.sender)
        }
        return this
    }

    override fun paintComponent(graphics: Graphics) {
        super.paintComponent(graphics)
        val localeCode = arteLocaleCode ?: return
        val senderIcon = icon ?: return
        val iconBounds = Rectangle(
            insets.left + (width - insets.left - insets.right - senderIcon.iconWidth) / 2,
            insets.top + (height - insets.top - insets.bottom - senderIcon.iconHeight) / 2,
            senderIcon.iconWidth,
            senderIcon.iconHeight,
        )
        val visibleIconBounds = ArteLocaleBadge.visibleIconBounds(
            iconBounds,
            Rectangle(
                insets.left,
                insets.top,
                width - insets.left - insets.right,
                height - insets.top - insets.bottom,
            ),
        )
        if (visibleIconBounds.isEmpty) {
            return
        }
        val badgeGraphics = graphics.create() as Graphics2D
        try {
            ArteLocaleBadge.paint(badgeGraphics, localeCode, visibleIconBounds)
        } finally {
            badgeGraphics.dispose()
        }
    }
}

internal class FilmGeoCellRenderer(
    appearance: FilmTableAppearance,
) : FilmCellRenderer(appearance) {
    override fun renderFilmCell(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int,
        filmColumn: FilmColumn,
        film: DatenFilm,
    ): Component {
        drawGeolocationIcons(film, isSelected)
        return this
    }
}

internal class FilmFormattedValueCellRenderer(
    appearance: FilmTableAppearance,
) : FilmCellRenderer(appearance) {
    override fun renderFilmCell(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int,
        filmColumn: FilmColumn,
        film: DatenFilm,
    ): Component {
        when (filmColumn) {
            FilmColumn.TIME -> {
                if (!appearance.lineBreak) horizontalAlignment = CENTER
                drawTime(film)
            }

            FilmColumn.DURATION -> {
                if (!appearance.lineBreak) horizontalAlignment = CENTER
                text = film.filmLengthAsString
            }

            FilmColumn.SIZE -> {
                if (!appearance.lineBreak) horizontalAlignment = RIGHT
                text = film.fileSizeAsString
            }

            else -> error("Unsupported formatted film column: $filmColumn")
        }
        return this
    }

    private fun drawTime(film: DatenFilm) {
        val timeText = film.sendeZeit.trim()
        if (timeText.isBlank()) {
            text = ""
            return
        }

        text = try {
            val time = LocalTime.parse(timeText, PARSER)
            val longFormat = ApplicationConfiguration.getInstance().filmTimeUseLongFormat
            (if (longFormat) LONG else SHORT).format(time)
        } catch (_: DateTimeParseException) {
            timeText
        }
    }

    private companion object {
        private val PARSER: DateTimeFormatter = DateTimeFormatter.ofPattern("H:mm[:ss]")
        private val SHORT: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")
        private val LONG: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss")
    }
}
