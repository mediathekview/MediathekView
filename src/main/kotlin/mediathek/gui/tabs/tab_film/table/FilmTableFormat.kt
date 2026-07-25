/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 */

package mediathek.gui.tabs.tab_film.table

import ca.odell.glazedlists.gui.AdvancedTableFormat
import mediathek.daten.DatenFilm
import mediathek.tool.models.FilmColumn

internal class FilmTableFormat : AdvancedTableFormat<DatenFilm> {
    override fun getColumnCount(): Int = FilmColumn.entries.size

    override fun getColumnName(column: Int): String = FilmColumn.fromIndex(column).title()

    override fun getColumnValue(baseObject: DatenFilm, column: Int): Any =
        FilmColumn.fromIndex(column).valueFrom(baseObject)

    override fun getColumnClass(column: Int): Class<*> = FilmColumn.fromIndex(column).valueType

    override fun getColumnComparator(column: Int): Comparator<*>? = when (FilmColumn.fromIndex(column)) {
        FilmColumn.PLAY,
        FilmColumn.SAVE,
        FilmColumn.BOOKMARK,
        FilmColumn.GEO,
            -> null

        FilmColumn.NUMBER,
        FilmColumn.DURATION,
        FilmColumn.SIZE,
            -> Comparator<Int> { left, right -> left.compareTo(right) }

        FilmColumn.HIGH_QUALITY,
        FilmColumn.SUBTITLE,
            -> Comparator<Boolean> { left, right -> left.compareTo(right) }

        else -> Comparator<Comparable<Any>> { left, right -> left.compareTo(right) }
    }
}
