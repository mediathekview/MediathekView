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

import mediathek.tool.FilterDTO
import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test
import java.util.*

internal class FilterSwitchReloadDecisionTest {

    @Test
    fun `returns none when reload not requested`() {
        val filter = FilterDTO(UUID.randomUUID(), "Filter 1")
        val state = filterState(filter = filter, zeitraum = "∞")

        val result = FilterSwitchReload.determine(state, state, requestReload = false)

        assertEquals(FilterSwitchReloadType.NONE, result)
    }

    @Test
    fun `returns none when selected filter did not change`() {
        val filter = FilterDTO(UUID.randomUUID(), "Filter 1")
        val previous = filterState(filter = filter, zeitraum = "∞")
        val current = filterState(filter = filter, zeitraum = "7")

        val result = FilterSwitchReload.determine(previous, current, requestReload = true)

        assertEquals(FilterSwitchReloadType.NONE, result)
    }

    @Test
    fun `returns zeitraum reload when filter switch changes zeitraum`() {
        val previous = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 1"), zeitraum = "∞")
        val current = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 2"), zeitraum = "7")

        val result = FilterSwitchReload.determine(previous, current, requestReload = true)

        assertEquals(FilterSwitchReloadType.ZEITRAUM, result)
    }

    @Test
    fun `returns table reload when filter switch keeps zeitraum`() {
        val previous = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 1"), zeitraum = "∞")
        val current = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 2"), zeitraum = "∞")

        val result = FilterSwitchReload.determine(previous, current, requestReload = true)

        assertEquals(FilterSwitchReloadType.TABLE, result)
    }

    private fun filterState(filter: FilterDTO, zeitraum: String): FilmFilterState {
        return FilmFilterState(
            currentFilter = filter,
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
            zeitraum = zeitraum
        )
    }
}
