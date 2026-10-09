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

package mediathek.gui.tabs.tab_film.filter

import ca.odell.glazedlists.EventList
import mediathek.tool.withWriteLock
import javax.swing.Action
import javax.swing.JCheckBox
import javax.swing.JLabel

class DialogFilterView(
    private val checkBoxBindings: List<Pair<JCheckBox, (FilmFilterState) -> Boolean>>,
    private val senderSelectionRenderer: (FilmFilterState) -> Unit,
    private val sourceThemaList: EventList<String>,
    private val themaSelectionRenderer: (String?) -> Unit,
    private val filmLengthRangeSlider: FilmLengthSlider,
    private val minFilmLengthValueLabel: JLabel,
    private val maxFilmLengthValueLabel: JLabel,
    private val zeitraumSpinner: ZeitraumSpinner,
    private val zeitraumFallbackWriter: (String) -> Unit,
    private val deleteCurrentFilterAction: Action,
) : FilmFilterView {
    override fun render(state: FilmFilterState, availableThemen: List<String>, canDeleteCurrentFilter: Boolean) {
        checkBoxBindings.forEach { (checkBox, selectedStateReader) ->
            checkBox.isSelected = selectedStateReader(state)
        }

        senderSelectionRenderer(state)

        sourceThemaList.withWriteLock {
            clear()
            addAll(availableThemen)
        }
        themaSelectionRenderer(state.thema)

        filmLengthRangeSlider.withValueIsAdjusting {
            filmLengthRangeSlider.lowValue = state.filmLengthMin
            filmLengthRangeSlider.highValue = state.filmLengthMax
        }
        minFilmLengthValueLabel.text = filmLengthRangeSlider.lowValueText
        maxFilmLengthValueLabel.text = filmLengthRangeSlider.highValueText

        zeitraumSpinner.restoreValue(state.zeitraum, zeitraumFallbackWriter)

        deleteCurrentFilterAction.isEnabled = canDeleteCurrentFilter
    }

    inline fun <T> FilmLengthSlider.withValueIsAdjusting(action: FilmLengthSlider.() -> T): T {
        valueIsAdjusting = true
        try {
            return action()
        } finally {
            valueIsAdjusting = false
        }
    }
}
