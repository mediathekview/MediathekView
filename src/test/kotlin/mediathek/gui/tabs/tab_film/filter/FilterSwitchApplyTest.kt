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

internal class FilterSwitchApplyTest {

    @Test
    fun `applyFilterSwitchReload triggers table reload for normal filter switch`() {
        val reloadRequester = RecordingReloadRequester()
        val previous = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 1"), zeitraum = "∞")
        val current = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 2"), zeitraum = "∞")

        val reloadType = FilterSwitchReload.apply(previous, current, true, reloadRequester)

        assertEquals(FilterSwitchReloadType.TABLE, reloadType)
        assertEquals(1, reloadRequester.tableReloadRequests)
        assertEquals(0, reloadRequester.zeitraumReloadRequests)
    }

    @Test
    fun `applyFilterSwitchReload triggers zeitraum reload for zeitraum filter switch`() {
        val reloadRequester = RecordingReloadRequester()
        val previous = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 1"), zeitraum = "∞")
        val current = filterState(filter = FilterDTO(UUID.randomUUID(), "Filter 2"), zeitraum = "7")

        val reloadType = FilterSwitchReload.apply(previous, current, true, reloadRequester)

        assertEquals(FilterSwitchReloadType.ZEITRAUM, reloadType)
        assertEquals(0, reloadRequester.tableReloadRequests)
        assertEquals(1, reloadRequester.zeitraumReloadRequests)
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

    private class RecordingReloadRequester : FilmFilterController.ReloadRequester {
        var tableReloadRequests = 0
        var zeitraumReloadRequests = 0

        override fun requestTableReload() {
            tableReloadRequests++
        }

        override fun requestZeitraumReload() {
            zeitraumReloadRequests++
        }
    }
}
