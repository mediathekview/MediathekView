/*
 * Copyright (c) 2025-2026 derreisende77.
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

package mediathek.gui.tabs.tab_film.filter

import com.jidesoft.swing.RangeSlider
import java.util.*
import javax.swing.JComponent
import javax.swing.JLabel

open class FilmLengthSlider : RangeSlider(MIN_FILM_LENGTH, MAX_FILM_LENGTH) {
    init {
        paintLabels = true
        paintTicks = true
        paintTrack = true
        majorTickSpacing = TICK_SPACING
        labelTable = createLabelTable()
    }

    val lowValueText: String
        get() = lowValue.toString()

    val highValueText: String
        get() = valueText(highValue)

    private fun createLabelTable(): Hashtable<Int, JComponent> = Hashtable<Int, JComponent>().apply {
        for (filmLength in MIN_FILM_LENGTH until MAX_FILM_LENGTH step TICK_SPACING) {
            put(filmLength, JLabel(valueText(filmLength)))
        }
        put(MAX_FILM_LENGTH, JLabel(UNLIMITED_TEXT))
    }

    private fun valueText(value: Int): String =
        if (value == UNLIMITED_VALUE) UNLIMITED_TEXT else value.toString()

    companion object {
        private const val MIN_FILM_LENGTH = 0
        private const val MAX_FILM_LENGTH = 240
        const val UNLIMITED_VALUE = MAX_FILM_LENGTH
        private const val TICK_SPACING = 30
        private const val UNLIMITED_TEXT = "\u221e"
    }
}
