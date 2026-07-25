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

import ca.odell.glazedlists.swing.AdvancedTableModel
import mediathek.config.MVColor
import mediathek.daten.DatenFilm
import mediathek.gui.tabs.tab_film.table.FilmTableAppearance
import mediathek.tool.models.FilmColumn
import org.apache.logging.log4j.LogManager
import java.awt.Color
import java.awt.Component
import javax.swing.JTable
import javax.swing.UIManager

internal abstract class FilmCellRenderer(
    protected val appearance: FilmTableAppearance,
) : CellRendererBaseWithStart() {
    final override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int,
    ): Component {
        try {
            resetComponent()
            horizontalTextPosition = LEADING
            super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
            if (appearance.lineBreak) {
                horizontalAlignment = LEFT
                verticalAlignment = TOP
            } else {
                horizontalAlignment = LEADING
                verticalAlignment = CENTER
            }

            val filmColumn = FilmColumn.fromIndex(table.convertColumnIndexToModel(column))
            val film = filmAt(table, row)
            val component = renderFilmCell(table, value, isSelected, row, column, filmColumn, film)
            if (!isSelected) {
                applyUnselectedRowColors(component, table, row, film)
            }
            return component
        } catch (ex: Exception) {
            logger.error("Fehler beim Rendern der Filmtabelle", ex)
            return this
        }
    }

    protected abstract fun renderFilmCell(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        row: Int,
        column: Int,
        filmColumn: FilmColumn,
        film: DatenFilm,
    ): Component

    @Suppress("UNCHECKED_CAST")
    private fun filmAt(table: JTable, viewRow: Int): DatenFilm {
        val model = table.model as AdvancedTableModel<DatenFilm>
        return model.getElementAt(table.convertRowIndexToModel(viewRow))
    }

    private fun applyUnselectedRowColors(
        component: Component,
        table: JTable,
        viewRow: Int,
        film: DatenFilm,
    ) {
        component.foreground = if (film.isNew) MVColor.NEW_COLOR.color else table.foreground
        val backgrounds = ArrayList<Color>(4)
        val alternate = if (viewRow % 2 != 0) UIManager.getColor("Table.alternateRowColor") else null
        backgrounds.add(alternate ?: table.background)
        if (film.isSeenInHistory) backgrounds.add(MVColor.FILM_HISTORY.color)
        if (film.isBookmarked) backgrounds.add(MVColor.FILM_BOOKMARKED.color)
        if (film.isDuplicate) backgrounds.add(MVColor.FILM_DUPLICATE.color)
        component.background = if (backgrounds.size == 1) backgrounds[0] else blend(backgrounds)
    }

    private fun blend(colors: Collection<Color>): Color = Color(
        colors.sumOf(Color::getRed) / colors.size,
        colors.sumOf(Color::getGreen) / colors.size,
        colors.sumOf(Color::getBlue) / colors.size,
        colors.sumOf(Color::getAlpha) / colors.size,
    )

    private companion object {
        private val logger = LogManager.getLogger(FilmCellRenderer::class.java)
    }
}
