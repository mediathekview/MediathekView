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

import ca.odell.glazedlists.BasicEventList
import mediathek.tool.FilterDTO
import org.junit.jupiter.api.Assertions.*
import org.junit.jupiter.api.Assumptions.assumeFalse
import org.junit.jupiter.api.Test
import java.awt.GraphicsEnvironment
import java.util.*
import javax.swing.AbstractAction
import javax.swing.Action
import javax.swing.JCheckBox
import javax.swing.JLabel

internal class DialogFilterViewTest {

    @Test
    fun `render applies state to widgets`() {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing render test requires a non-headless environment")

        val checkBox = JCheckBox()
        var renderedSenderState: FilmFilterState? = null
        var renderedThema: String? = null
        val sourceThemaList = BasicEventList<String>()
        val slider = TestFilmLengthSlider()
        val minLabel = JLabel()
        val maxLabel = JLabel()
        val zeitraumSpinner = ZeitraumSpinner()
        var fallbackWritten = ""
        val deleteAction = object : AbstractAction(), Action {
            override fun actionPerformed(e: java.awt.event.ActionEvent?) = Unit
        }

        val view = DialogFilterView(
            checkBoxBindings = listOf(checkBox to FilmFilterState::showNewOnly),
            senderSelectionRenderer = { renderedSenderState = it },
            sourceThemaList = sourceThemaList,
            themaSelectionRenderer = { renderedThema = it },
            filmLengthRangeSlider = slider,
            minFilmLengthValueLabel = minLabel,
            maxFilmLengthValueLabel = maxLabel,
            zeitraumSpinner = zeitraumSpinner,
            zeitraumFallbackWriter = { fallbackWritten = it },
            deleteCurrentFilterAction = deleteAction
        )

        val state = FilmFilterState(
            currentFilter = FilterDTO(UUID.randomUUID(), "Filter 1"),
            showNewOnly = true,
            showBookMarkedOnly = false,
            showHighQualityOnly = false,
            showSubtitlesOnly = false,
            showLivestreamsOnly = false,
            showUnseenOnly = false,
            dontShowAbos = false,
            dontShowSignLanguage = false,
            dontShowGeoblocked = false,
            dontShowTrailers = false,
            dontShowAudioVersions = false,
            dontShowDuplicates = false,
            checkedChannels = setOf("3Sat"),
            thema = "...von oben",
            filmLengthMin = 10,
            filmLengthMax = 60,
            zeitraum = "7"
        )

        view.render(state, listOf("...von oben", "Doku"), canDeleteCurrentFilter = true)

        assertTrue(checkBox.isSelected)
        assertSame(state, renderedSenderState)
        assertEquals("...von oben", renderedThema)
        assertEquals(listOf("...von oben", "Doku"), sourceThemaList.toList())
        assertEquals(10, slider.lowValue)
        assertEquals(60, slider.highValue)
        assertEquals("10", minLabel.text)
        assertEquals("60", maxLabel.text)
        assertEquals(7, zeitraumSpinner.value)
        assertEquals("", fallbackWritten)
        assertTrue(deleteAction.isEnabled)
    }

    @Test
    fun `render disables delete action when current filter cannot be deleted`() {
        assumeFalse(GraphicsEnvironment.isHeadless(), "Swing render test requires a non-headless environment")

        val deleteAction = object : AbstractAction(), Action {
            override fun actionPerformed(e: java.awt.event.ActionEvent?) = Unit
        }
        val view = DialogFilterView(
            checkBoxBindings = emptyList(),
            senderSelectionRenderer = {},
            sourceThemaList = BasicEventList(),
            themaSelectionRenderer = {},
            filmLengthRangeSlider = TestFilmLengthSlider(),
            minFilmLengthValueLabel = JLabel(),
            maxFilmLengthValueLabel = JLabel(),
            zeitraumSpinner = ZeitraumSpinner(),
            zeitraumFallbackWriter = {},
            deleteCurrentFilterAction = deleteAction
        )

        view.render(
            state = FilmFilterState(
                currentFilter = FilterDTO(UUID.randomUUID(), "Filter 1"),
                showNewOnly = false,
                showBookMarkedOnly = false,
                showHighQualityOnly = false,
                showSubtitlesOnly = false,
                showLivestreamsOnly = false,
                showUnseenOnly = false,
                dontShowAbos = false,
                dontShowSignLanguage = false,
                dontShowGeoblocked = false,
                dontShowTrailers = false,
                dontShowAudioVersions = false,
                dontShowDuplicates = false,
                checkedChannels = emptySet(),
                thema = "",
                filmLengthMin = 0,
                filmLengthMax = FilmLengthSlider.UNLIMITED_VALUE,
                zeitraum = "0"
            ),
            availableThemen = emptyList(),
            canDeleteCurrentFilter = false
        )

        assertFalse(deleteAction.isEnabled)
    }

    @Test
    fun `zeitraum spinner falls back to unlimited for invalid persisted value`() {
        val spinner = ZeitraumSpinner()
        var fallbackWritten = ""

        spinner.restoreValue("invalid") { fallbackWritten = it }

        assertEquals(ZeitraumSpinner.INFINITE_VALUE, spinner.value)
        assertEquals(ZeitraumSpinner.INFINITE_TEXT, fallbackWritten)
    }

    @Test
    fun `zeitraum spinner emits infinite symbol for zero value`() {
        val spinner = ZeitraumSpinner()
        var emittedValue = ""

        spinner.installValueChangeListener { emittedValue = it }
        spinner.value = 7
        spinner.value = ZeitraumSpinner.INFINITE_VALUE

        assertEquals(ZeitraumSpinner.INFINITE_TEXT, emittedValue)
    }

    private class TestFilmLengthSlider : FilmLengthSlider() {
        override fun updateUI() = Unit
    }
}
